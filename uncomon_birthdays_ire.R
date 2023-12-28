################################################################################
# This file creates the 6 least common birthdays in Ireland based on data from
# the CSO VSA19 dataset
# Created by: Clare Sullivan
# Date last update: 23/12/2023
################################################################################

# Loading libraries
library(dplyr)
library(lubridate)
library(cropgrowdays)
library(babynames)
library(ggplot2)
library(ggtext)
library(dplyr)
library(stringr)
library(ggpubr)

# Read in data
# Data up to date as of 23rd December 2023
# Data can be downloaded from data.cso.ie then search for VSA19
bd_new <- read.csv("VSA19.20231223T111207.csv")
# Correcting column name
names(bd_new)[1] <- 'Statistic.Label'

# Filtering for both sexes, exclude total and rank
bd_new_gp <- bd_new %>% 
  filter(Sex.of.Child == "Both sexes" & Date.of.Occurrence != "Total" & Statistic.Label != "Rank")

# Converting to date format
bd_new_gp$date <- as.numeric(gsub("[a-zA-Z]","",bd_new_gp$Date.of.Occurrence))
bd_new_gp$full_date <- ymd(paste(bd_new_gp$Year,"-",bd_new_gp$Month.of.Occurrence,"-", bd_new_gp$date, sep=""))
bd_new_gp$date2 <- day_of_year(ymd(paste(bd_new_gp$Year,"-",bd_new_gp$Month.of.Occurrence,"-", bd_new_gp$date, sep="")))

# Ordering month as a factor
bd_new_gp$Month.of.Occurrence <- factor(bd_new_gp$Month.of.Occurrence, levels=
                                          c("January", "February", "March", "April", "May", "June", "July",
                                            "August", "September", "October", "November", "December"))
# Aggregating by Day
bd_new_agg <- bd_new_gp %>% 
  select(Year, Month.of.Occurrence, VALUE, date) %>% 
  group_by(Month.of.Occurrence, date) %>%
  summarise(births = mean(VALUE, na.rm=TRUE))

# Calculating separate average for Feb 29th
bd_new_agg$births <- ifelse(bd_new_agg$Month.of.Occurrence=="February" & bd_new_agg$date==29 , 
                            bd_new_agg$births/4, bd_new_agg$births)

# Lowest 6 birthdays based on average births
bd_new_agg_lc <- arrange(bd_new_agg, births)[1:6,]

# Creating shorter month name e.g. Jan, Feb, Mar
bd_new_agg_lc$Month <- substr(bd_new_agg_lc$Month.of.Occurrence, 1, 3)

# Creating vector with overall average births
avg <- c(NA, NA, NA, NA, NA, mean(bd_new_agg$births, na.rm=TRUE))
# Adding overall average to dataframe
bd_new_agg_lc$avg <- avg

# Creating a label for the overall average
lavg <- c(NA, NA, NA, NA, NA, "Overall Average Daily Births")
# Adding label vector to dataframe
bd_new_agg_lc$lavg <- lavg

# Identifying day with least births
min_births <- bd_new %>%
  filter(Statistic.Label=="Births" & Sex.of.Child=="Both sexes" & Date.of.Occurrence !="Total") %>%
  arrange(VALUE)

# Identifying day with most birhts
max_births <- bd_new %>%
  filter(Statistic.Label=="Births" & Sex.of.Child=="Both sexes" & Date.of.Occurrence !="Total") %>%
  arrange(desc(VALUE))

# Creating the subtitle text
subt <- "<br>With just **67 births** recorded, **Christmas Day, December 25th, in 1994** had the **lowest births** recorded in a single day in Ireland <br>between the years 1980 and 2021. Conversely, **September 25th 2008**, exactly **9 months after Christmas**, had the highest <br> daily births recorded with a total of **279 births**! <br><br>On average, **February 29th** is the least common day to be born due to it occurring only once in every 4 years. For dates which <br> occur every year, the least common days to be born in Ireland are the **26th, 25th, 24th December, 17th March and 1st January**, <br>in order of lowest average births, based on the daily births in Ireland between 1980 and 2021. The lowest average daily births <br> of 108, occurring on the 26th December, is **just 64%** of the overall daily average. <br>"

# Creating the plot
plot_births_day <- ggplot(bd_new_agg, aes(x=date, y=births)) +
  geom_hline(yintercept = mean(bd_new_agg$births, na.rm=TRUE), linetype="dashed", color="grey", alpha=0.5)+
  geom_text(data=bd_new_agg_lc, aes(x=16, y=avg-30, label=paste(lavg, round(avg, digits=0))), color="grey", size=3)+
  geom_segment(aes(x=date, xend=date, y=20, yend=births-1,
                   alpha=ifelse(date==29 & Month.of.Occurrence=="February" |
                                  date==26 & Month.of.Occurrence=="December"|
                                  date==25 & Month.of.Occurrence=="December"|
                                  date==24 & Month.of.Occurrence=="December"|
                                  date==17 & Month.of.Occurrence=="March"|
                                  date==1 & Month.of.Occurrence=="January", 1,0.6)), linewidth=1, color="grey")+
  geom_label(data=bd_new_agg_lc, aes(x=date,
                                     y=ifelse(date == 24 & Month.of.Occurrence=="December", births +67, 
                                              ifelse(date==25 & Month.of.Occurrence=="December", births +60,
                                                     ifelse(date==26 & Month.of.Occurrence=="December", births+35,
                                                            ifelse(date==29 & Month.of.Occurrence=="February", 70,
                                                                   180)))),
                                     label=paste(round(births, digits=0))), 
             size=3,
             label.padding = unit(0.10, "lines"), color="darkgrey")+
  geom_text(data=bd_new_agg_lc, aes(x=date, y=ifelse(date==24, -5, 
                                                     ifelse(date==26 & Month.of.Occurrence=="December", -5,
                                                            10)), label=date), size=3, color="darkgrey")+
  geom_point(aes(color=Month.of.Occurrence, size=2, alpha=ifelse(date==29 & Month.of.Occurrence=="February" |
                                                                   date==26 & Month.of.Occurrence=="December"|
                                                                   date==25 & Month.of.Occurrence=="December"|
                                                                   date==24 & Month.of.Occurrence=="December"|
                                                                   date==17 & Month.of.Occurrence=="March"|
                                                                   date==1 & Month.of.Occurrence=="January", 1,0.6)))+ 
  facet_wrap(~Month.of.Occurrence, ncol=3) +
  ylim(-10,200)+
  xlab("Average Births by Day")+
  ylab("Average Birthdays")+
  labs(
    title="Extra Special Birthdays: The 6 Least Common Birthdays in Ireland",
    subtitle=str_wrap(subt,160)
  )+
  scale_x_continuous(position = "top", limits=c(0,32)) +
  theme_bw()+
  theme(
    plot.margin = margin(t = 10,  # Top margin
                         r = 10,  # Right margin
                         b = 10,  # Bottom margin
                         l = 10), # Left margin
    legend.position="none",
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color="grey", face="bold", size=14),
    plot.title = element_markdown(color = "darkgrey", face="bold", size=22),
    plot.subtitle = element_markdown(color="darkgrey", size=14, lineheight = 1.5),
    plot.caption = element_text(color = "darkgrey", size=10),
    axis.title.x=element_text(color="darkgrey"),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(color="white"),
    axis.ticks.y=element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# View the plot
plot_births_day

# Calculate average births by year 
bd_year <- bd_new_gp %>% 
  group_by(Year)%>%
  summarise(avg = mean(VALUE, na.rm=TRUE))

# Plot births by year
plot_births_year <- ggplot(bd_year , aes(x=Year, y=avg)) +
  geom_line(color="darkgrey", linewidth=1)+
  geom_segment(aes(x=1994 , xend=1994, y=132.2, yend=190), color="darkgrey", linewidth=0.5, linetype="dashed")+
  geom_segment(aes(x=2009 , xend=2009, y=160, yend=207), color="darkgrey", linewidth=0.5, linetype="dashed")+
  geom_label(aes(x=1994, y=190), label=paste("1994", "Lowest Daily Births", sep="\n"), color="darkgrey")+
  geom_label(aes(x=2009, y=160), label=paste("2009", "Highest Daily Births", sep="\n"), color="darkgrey")+
  geom_point(aes(x=1994, y=132.2), size=3, color="darkgrey")+
  geom_point(aes(x=2009, y=207), size=3, color="darkgrey")+
  # geom_point(x=1994,y=132.2, size=3, color="deeppink")+
  #  geom_point(x=2008,y=205.4, size=3, color="lightblue")+
  xlab("Average Daily Births by Year")+
  labs(caption="<br>**Data Source:** CSO Table VSA19 (data.cso.ie) <br> **Graphics:** CSullivan @ccsul1")+
  # scale_x_discrete(position = "top") +
  scale_y_continuous(position = "right")+
  theme_bw()+
  theme(
    legend.position="none",
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color="grey", face="bold"),
    plot.title = element_text(color = "darkgrey", face="bold", size=24),
    plot.subtitle = element_text(color="darkgrey", size=12),
    plot.caption = element_markdown(color = "darkgrey", size=10),
    axis.title.x=element_text(color="darkgrey"),
    axis.text.x=element_text(color="darkgrey"),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(color="darkgrey"),
    axis.ticks.y=element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# View the plot
plot_births_year

# Put plots together
plot_all <- ggarrange(plot_births_day, plot_births_year, nrow = 2, ncol = 1, common.legend = FALSE, heights=c(1,0.2)) #common.legend = TRUE, legend="bottom", 
# Save the plots as a PDF
ggsave("plot_births_all.pdf", plot=plot_all, dpi=300, width = 300, height = 370, units = "mm") 
