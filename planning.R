

# this script investigates accessibility from boat launches around Willison at given water elevations

library(plyr)
library(dplyr)
library(tidyhydat)
library(lubridate)
library(ggplot2)
library(AnglerCreelSurveySimulation)
library(calendar)

launches<- c("Dunlevy", "ElizabethCreek")

# Not all launches are available in the spring on Williston, so nee to figure out when they are generally 
# available.

launch.elev <- data.frame(launches, elev = c(660.5, 655))

yday(ymd("2010-05-01")) 
yday(ymd("2010-10-31")) 


#length of the creel season:

season <- data.frame(startend = c("start","end"), Date = c(yday(ymd("2010-05-01")),yday(ymd("2010-10-31"))))


# this function will download a copy of HYDAT, but i will take some time, 
  # and once done it doesn't need updating all the time 

#download_hydat()

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


# creel survey design ####

#anglers per day. This function creates a daily set of anglers based on mean values. 
#   mean_trip_length is the angler's mean trip and fishing day length is the total possible fishing time
#   in the day (i.e. sun-up to sundown)

anglers <- make_anglers(n_anglers = 12, mean_trip_length = 8, fishing_day_length = 15)

anglers %>% 
  get_total_values(start_time = 5, wait_time = 9, sampling_prob = 9/18, mean_catch_rate = 2)


sim <- simulate_bus_route(start_time = 5, wait_time = 9, n_sites = 1, n_anglers = 12,
                          sampling_prob = 9/18, mean_catch_rate = 2, fishing_day_length = 18)

sim1 <- conduct_multiple_surveys(n_sims = 60, start_time = 3, wait_time = 9, n_sites = 1, n_anglers = 6,
                                sampling_prob = 9/15, mean_catch_rate = 2, fishing_day_length = 15)
sim2 <- conduct_multiple_surveys(n_sims = 60, start_time = 5, wait_time = 9, n_sites = 1, n_anglers = 12,
                                 sampling_prob = 9/15, mean_catch_rate = 2, fishing_day_length = 15)

mod1 <- 
  sim1 %>% 
  lm((Ehat * catch_rate_ROM) ~ true_catch, data = .)

summary(mod1)

mod2 <- 
  sim2 %>% 
  lm((Ehat * catch_rate_ROM) ~ true_catch, data = .)
summary(mod2)


sim2 <- 
  sim2 %>%
  mutate(est_catch = Ehat * catch_rate_ROM)

sim2 %>% 
  ggplot(aes(x = true_catch, y = est_catch)) +
  geom_point() +
  geom_abline(intercept = mod2$coefficients[1], slope = mod2$coefficients[2], 
              colour = "red", size = 1.01)


sim2 %>%
  ggplot(aes(x = true_effort, y = Ehat)) +
  geom_point() +
  geom_abline(intercept = mod2$coefficients[1], slope = mod2$coefficients[2], 
              colour = "red", size = 1.01)





# Scant data from Cubberley and Hengeveld, vehicle counts from 2009 to 2013
library(readxl)

car.counts09 <- read_excel("GMSMON20-Raw_2009-2013.xlsx", sheet="Raw_2009")
car.counts10 <- read_excel("GMSMON20-Raw_2009-2013.xlsx", sheet="Raw_2010")
car.counts11 <- read_excel("GMSMON20-Raw_2009-2013.xlsx", sheet="Raw_2011")
car.counts12 <- read_excel("GMSMON20-Raw_2009-2013.xlsx", sheet="Raw_2012")
car.counts13 <- read_excel("GMSMON20-Raw_2009-2013.xlsx", sheet="Raw_2013")

car.counts <- rbind(car.counts09, car.counts10, car.counts11, car.counts12, car.counts13)
unique(car.counts$Year)

Site.names <- data.frame(Site = c(1:7), name = c("Cut Thumb","Six Mile Bay","Finlay Bay","Strandberg",
                                                 "Elizabeth Creek","Dunlevy","Mackenzie Landing"))

car.counts <- car.counts %>% 
  left_join(Site.names, by="Site")

daily.visits <- car.counts %>% 
  mutate(date = dmy(paste(Day, Month,Year)), yday = yday(date)) %>% 
  filter(Site %in% c(5,6)) %>% 
  group_by(Month, Year, yday, name) %>% 
  summarize(visits=length(unique(ID)))

monthly.visits <- car.counts %>% 
  mutate(date = dmy(paste(Day, Month,Year)), fYear = factor(Year)) %>% 
  filter(Site %in% c(5,6)) %>%
  group_by(Month, fYear, name) %>% 
  summarize(total.monthly = length(unique(ID)))

#daily total visits

ggplot(data=daily.visits)+
  geom_point(aes(x=yday, y=visits, colour=name))+
  facet_wrap(~Year)

#monthly total visits

ggplot(data=monthly.visits)+
  geom_point(aes(x=Month, y=total.monthly, colour=fYear))+
  facet_wrap(~name)

#calc ave. daily visits:

daily.visits %>% 
  group_by(name) %>% 
  summarize(mean.daily.visits = mean(visits, na.rm=T), sd.daily.visits = sd(visits, na.rm=T),
            se.daily.visits = sd.daily.visits/sqrt(length(visits)))

#calc ave. monthly visits:

(monthly <- monthly.visits %>% 
  group_by(Month) %>% 
  summarize(mean.monthly.visits = mean(total.monthly, na.rm=T), 
            sd.monthly.visits = sd(total.monthly, na.rm=T),
            se.monthly.visits = sd.monthly.visits/sqrt(length(total.monthly))))

shifts <- data.frame(month = c(5:11),
                     prop = round(monthly$mean.monthly.visits/sum(monthly$mean.monthly.visits),2))
           
shifts$shifts = 30*shifts$prop

shifts



# read in Williston calendar and check if things well distributed

ics_df <- ic_read("Willy Creel Techs Calendar.ics")

#check number of weekends, weekdays per month:
calendar <- ics_df %>% 
    mutate(Date=`DTSTART;VALUE=DATE`,activity=`SUMMARY;LANGUAGE=en-ca`,
       
               dayofweek = wday(Date,label=T,abbr=T), 
           weekend = ifelse(dayofweek %in% c("Sat","Sun"),T,F),
           month=month(Date),year=year(Date)) %>% 
  arrange(Date)


str(calendar)
unique(calendar$activity)


calendar %>% 
filter(activity %in% c("Brian","Brian ")) %>% 
dplyr::group_by(month) %>% 
  summarize(num.days = length(weekend == T))

calendar %>% 
  filter(activity %in% c("Guy")) %>% 
  dplyr::group_by(dayofweek, weekend) %>% 
  summarize(num.days = length(weekend == T))

noquote(paste0(stamp("March 1")(unique(calendar$Date)), sep=","))



