

# this script investigates accessibility from boat launches around Willison at given water elevations

library(plyr)
library(dplyr)
library(tidyhydat)
library(lubridate)
library(ggplot2)

launches<- c("Dunlevy", "ElizabethCreek")

launch.elev <- data.frame(launches, elev = c(660, 655))

yday(ymd("2010-05-01")) # 121
yday(ymd("2010-10-31")) # 304

#length of the creel season:

season <- data.frame(startend = c("start","end"), Date = c(yday(ymd("2010-05-01")),yday(ymd("2010-10-31"))))


# this function will download a copy of HYDAT, but i will take some time, 
  # and once done it doesn't need updating all the time 

# download_hydat()

# Williston at Lost Cabin station number: 07EF002

lost.cabin <- hy_daily_levels(station_number = "07EF002") %>% 
  mutate(year = year(Date), elev = Value+630.04, julian = yday(Date)) %>% 
  dplyr::filter(year >= 2010) %>% 
  mutate(fyear = factor(year, order = T)) %>% 
  dplyr::group_by(year)

ggplot()+
  geom_line(data=lost.cabin, aes(x=julian, y=elev, colour=fyear), size=2)+
  geom_hline(data=launch.elev, aes(yintercept = elev, linetype=launches))+
  geom_vline(data=season, aes(xintercept = Date))
  
#ask when the first date each year that elevation went above Dunlevy limit (after May 1st):

lost.cabin %>% 
  dplyr::filter(julian >= yday(ymd("2010-05-01"))) %>% 
  dplyr::filter(elev >= launch.elev[1,"elev"]) %>% 
  dplyr::group_by(year) %>% 
  dplyr::slice(1) 

