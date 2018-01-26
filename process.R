setwd("~/Documents/MIS/process")

## 1. Load Libraries ----
library(readxl)
library(lubridate)
library(dplyr)
library(reshape)
library(reshape2)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(reshape2)
library(Hmisc)
library(data.table)

## 2. Loading the Workspace Image of Preprocess.R ----
load("../preprocess/Preprocess.RData")

## 3. Collating day-on-day data and preparing Channel/App/Device/Spec/Total_Users Archives ----

## Channel Collation to Channel_Archive

# for(i in 1:(day(Sys.Date())-1)){
#   print(i)
#   temp <- read.csv(paste0("../preprocess/channel_specific/daily/channel archive d 2017-08-", ifelse((nchar(i)==1), paste0("0",i), i),".csv"))[-1]
#   users_channel_monthly <- rbind(users_channel_monthly, temp)
# }
# 
# sum(users_channel_monthly$users[users_channel_monthly$channel == 'Direct' & users_channel_monthly$country == 'India' & users_channel_monthly$month == 'Jul 2017'])

users_channel_monthly$month <- as.yearmon(users_channel_monthly$date)
users_channel_monthly$date <- NULL

Channel_Archive <- users_channel_monthly %>%
                          group_by(month, country, city, channel) %>%
                          summarise(users = sum(users)) %>%
                          ungroup()
Channel_Archive$month <- as.Date(Channel_Archive$month)

## App Collation to App_Archive

# for(i in 1:(day(Sys.Date())-1)){
#   print(i)
#   temp <- read.csv(paste0("../preprocess/app_specific/daily/app archive d 2017-08-", ifelse((nchar(i)==1), paste0("0",i), i),".csv"))[-1]
#   users_app_monthly <- rbind(users_app_monthly, temp)
# }

users_app_monthly$month <- as.yearmon(users_app_monthly$date)
users_app_monthly$date <- NULL

App_Archive <- users_app_monthly %>%
                    mutate(app2 = ifelse((grepl('Android', users_app_monthly$app)), "Mobile App", ifelse((grepl('iOS', users_app_monthly$app)), "Mobile App", "Mobile App"))) %>%
                    group_by(month, country, city, app2) %>%
                    summarise(users = sum(users)) %>%
                    ungroup()

App_Archive$month <- as.Date(App_Archive$month)
App_Archive$app <- App_Archive$app2
App_Archive$app2 <- NULL
App_Archive <- App_Archive[,c(1:3, 5,4)]

## Device Collation to Device_Archive

# for(i in 1:(day(Sys.Date())-1)){
#   print(i)
#   temp <- read.csv(paste0("../preprocess/device_specific//daily/device archive d 2017-08-", ifelse((nchar(i)==1), paste0("0",i), i),".csv"))[-1]
#   users_device_monthly <- rbind(users_device_monthly, temp)
# }

users_device_monthly$month <- as.yearmon(users_device_monthly$date)
users_device_monthly$date <- NULL

Device_Archive <- users_device_monthly %>%
                        group_by(month, country, city, device) %>%
                        summarise(users = sum(users)) %>%
                        ungroup()
Device_Archive$month <- as.Date(Device_Archive$month)

## Total_Users Collation to Users_Archive

# for(i in 1:(day(Sys.Date())-1)){
#   print(i)
#   temp <- read.csv(paste0("../preprocess/total_users/daily/total_users d 2017-08-", ifelse((nchar(i)==1), paste0("0",i), i),".csv"))[-1]
#   total_users_monthly <- rbind(total_users_monthly, temp)
# }

total_users_monthly$month <- as.yearmon(total_users_monthly$date)
total_users_monthly$date <- NULL

Users_Archive <- total_users_monthly %>%
  group_by(month, country, city) %>%
  summarise(users = sum(users)) %>%
  ungroup()
Users_Archive$month <- as.Date(Users_Archive$month)

## YTD_Users Collation to YTD_Users

ytd_users$month <- as.yearmon(ytd_users$date)
ytd_users$date <- NULL

ytd_archive <- ytd_users %>%
  group_by(month, country) %>%
           # source2) 
  summarise(users = sum(users)) %>%
  ungroup()
ytd_archive$month <- as.Date(ytd_archive$month)

mobile_app <- App_Archive %>%
  group_by(month, country, app) %>%
  summarise(users = sum(users)) %>%
  ungroup()
mobile_app <- subset(mobile_app, mobile_app$month != as.Date(paste0(year(Sys.Date()),"-", month(Sys.Date()), "-01")))

users_app_ytd <- users_app_ytd %>%
  group_by(date,country,app) %>%
  summarise(users = sum(users)) %>%
  ungroup()
colnames(users_app_ytd)[1] <- c("month")
mobile_app <- unique(rbind(mobile_app, users_app_ytd))

mobile_app <- subset(mobile_app, mobile_app$app == "Mobile App")
mobile_app$month <- as.Date(mobile_app$month, format = '%Y-%m-%d')

colnames(mobile_app)[3] <- c('channels')
mobile_app_dch_india <- subset(mobile_app, mobile_app$country == "India")
mobile_app_dch_india$channels <- NULL
colnames(mobile_app_dch_india)[3] <- c('users_app')

## DCH_Users Collation to DCH_Archive

DCH_Archive <- rbind(dch_users_monthly, dch_users_daily)
DCH_Archive$month <- DCH_Archive$date
DCH_Archive$date <- NULL
DCH_Archive <- DCH_Archive[c(3,1,2)]
DCH_Archive$month <- as.Date(DCH_Archive$month)

dch_international <- subset(ytd_archive, ytd_archive$country != "India")

DCH_Archive <- rbind(DCH_Archive, dch_international)

DCH_Archive <- merge(DCH_Archive, mobile_app_dch_india, by = c('month', 'country'), all.x = T, all.y = T)
DCH_Archive[is.na(DCH_Archive)] <- 0

DCH_Archive$total <- DCH_Archive$users + DCH_Archive$users_app
DCH_Archive$users <- DCH_Archive$total
DCH_Archive$users_app <- DCH_Archive$total <- NULL

## App_Archive Collation to App_Archive_city

temp_India_app <- App_Archive %>%
  filter(country == "India") %>%
  mutate(tier = ifelse((city %in% c('Mumbai_MMR', 'Delhi_NCR', 'Bengaluru', 'Pune', 'Chennai', 'Hyderabad', 'Kolkata')), "Tier-1", "Tier-2")) %>%
  group_by(month, country, tier) %>%
  summarise(users = sum(users)) %>%
  ungroup()

temp_International_app <- App_Archive %>%
  filter(country != "India") %>%
  mutate(tier = "International") %>%
  group_by(month, country, tier) %>%
  summarise(users = sum(users)) %>%
  ungroup()

App_Archive_city <- rbind(temp_India_app, temp_International_app)

App_Archive_city$country2[App_Archive_city$country == "India"] <- "India"
App_Archive_city$country2[App_Archive_city$country == "Indonesia"] <- "Indonesia"
App_Archive_city$country2[App_Archive_city$country == "Philippines"] <- "Philippines"
App_Archive_city$country2[App_Archive_city$country == "Brazil"] <- "Brazil"
App_Archive_city$country2[App_Archive_city$country == "Singapore"] <- "Singapore"
App_Archive_city$country2[is.na(App_Archive_city$country2)] <- "Rest of the World"
App_Archive_city$country <- App_Archive_city$country2
App_Archive_city$country2 <- NULL

App_Archive_city <- App_Archive_city %>%
  group_by(month, country, tier) %>%
  summarise(users = sum(users)) %>%
  ungroup()

## DCH_Users_City Collation to DCH_Archive_city
DCH_Archive_city <- rbind(dch_users_monthly_city, dch_users_daily_city)
DCH_Archive_city$month <- DCH_Archive_city$date
DCH_Archive_city$date <- NULL
DCH_Archive_city <- DCH_Archive_city[c(4,1,2,3)]
DCH_Archive_city$month <- as.Date(DCH_Archive_city$month)

colnames(amp_users_city)[1] <- c("month")
DCH_Archive_city <- rbind(DCH_Archive_city, amp_users_city)

DCH_Archive_city <- DCH_Archive_city %>%
  mutate(city = ifelse((city == 'Mumbai' | 
                          city == 'Navi Mumbai'| 
                          city == 'Thane'), 'Mumbai_MMR', 
                       ifelse((city == 'New Delhi' | 
                                 city == 'Delhi' | 
                                 city == 'Ghaziabad' | 
                                 city == 'Faridabad' | 
                                 city == 'Noida' |
                                 city == 'Greater Noida' |
                                 city == 'Gurgaon'), 'Delhi_NCR', 
                              ifelse((city == 'Bangalore' | 
                                        city == 'Bangalore Rural'), 'Bengaluru', 
                                     ifelse(city == 'S?o Paulo', 'Sao Paulo', city))))) %>%
  group_by(month, country, city) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

temp_India <- DCH_Archive_city %>%
  filter(country == "India") %>%
  mutate(tier = ifelse((city %in% c('Mumbai_MMR', 'Delhi_NCR', 'Bengaluru', 'Pune', 'Chennai', 'Hyderabad', 'Kolkata')), "Tier-1", "Tier-2")) %>%
  group_by(month, country, tier) %>%
  summarise(users = sum(users)) %>%
  ungroup()

temp_International <- DCH_Archive_city %>%
  filter(country != "India") %>%
  mutate(tier = "International") %>%
  group_by(month, country, tier) %>%
  summarise(users = sum(users)) %>%
  ungroup()

DCH_Archive_city <- rbind(temp_India, temp_International)

DCH_Archive_city$country2[DCH_Archive_city$country == "India"] <- "India"
DCH_Archive_city$country2[DCH_Archive_city$country == "Indonesia"] <- "Indonesia"
DCH_Archive_city$country2[DCH_Archive_city$country == "Philippines"] <- "Philippines"
DCH_Archive_city$country2[DCH_Archive_city$country == "Brazil"] <- "Brazil"
DCH_Archive_city$country2[DCH_Archive_city$country == "Singapore"] <- "Singapore"
DCH_Archive_city$country2[is.na(DCH_Archive_city$country2)] <- "Rest of the World"
DCH_Archive_city$country <- DCH_Archive_city$country2
DCH_Archive_city$country2 <- NULL

DCH_Archive_city <- DCH_Archive_city %>%
  group_by(month, country, tier) %>%
  summarise(users = sum(users)) %>%
  ungroup()

DCH_Archive_city <- merge(DCH_Archive_city, App_Archive_city, by = c('month', 'country', 'tier'), all.x = T, all.y = T)
DCH_Archive_city$users <- DCH_Archive_city$users.x + DCH_Archive_city$users.y
DCH_Archive_city$users.x <- DCH_Archive_city$users.y <- NULL

## Speciality Collation to Spec_Archive

Spec_Archive_Global <- as.data.frame(users_speciality_global)
Spec_Archive_City <- as.data.frame(users_speciality_city)

## mobile_app addition to Device Archive

device_users <- Device_Archive %>%
  group_by(month, country, device) %>%
  summarise(users = sum(users)) %>%
  ungroup()

mobile_app_users <- mobile_app

colnames(mobile_app_users)[3] <- c("device")

device_users <- unique(rbind(device_users, mobile_app_users))

## 4. Limiting the Timeline for the last 12-months ----

Channel_Archive <- subset(Channel_Archive, Channel_Archive$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

Device_Archive <- subset(Device_Archive, Device_Archive$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

App_Archive <- subset(App_Archive, App_Archive$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

Users_Archive <- subset(Users_Archive, Users_Archive$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

ytd_archive <- subset(ytd_archive, ytd_archive$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

mobile_app <- subset(mobile_app, mobile_app$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

Spec_Archive_Global <- subset(Spec_Archive_Global, Spec_Archive_Global$date >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

Spec_Archive_City <- subset(Spec_Archive_City, Spec_Archive_City$date >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

DCH_Archive <- subset(DCH_Archive, DCH_Archive$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

DCH_Archive_city <- subset(DCH_Archive_city, DCH_Archive_city$month >= as.Date(paste0(year(Sys.Date()-365),"-", month(Sys.Date()-365), "-01")))

total_transactions_city <- subset(total_transactions_city, total_transactions_city$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))
total_transactions_city[is.na(total_transactions_city)] <- 0

rotw_vn_country <- subset(rotw_vn_country, rotw_vn_country$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))
rotw_vn_country[is.na(rotw_vn_country)] <- 0

total_transactions_country <- subset(total_transactions_country, total_transactions_country$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))
total_transactions_country[is.na(total_transactions_country)] <- 0

book_practo_spec <- subset(book_practo_spec, book_practo_spec$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))
  
vn_spec <- subset(vn_spec, vn_spec$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))

book_practo_city_spec <- subset(book_practo_city_spec, book_practo_city_spec$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))

vn_city_spec <- subset(vn_city_spec, vn_city_spec$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))

cancellations_country <- subset(cancellations_country, cancellations_country$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))

cancellations_city <- subset(cancellations_city, cancellations_city$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))

summ_trans <- subset(summ_trans, summ_trans$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365))))

mobile_app_dch_india <- subset(mobile_app_dch_india, mobile_app_dch_india$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365)), "-01"))

unique_patient_tranx <- subset(unique_patient_tranx, unique_patient_tranx$month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365)), "-01"))

## Reach Files ##

# all_transactions <- subset(all_transactions, month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365)), "-01") & month <= paste0(year(Sys.Date()),"-", ifelse(nchar(month(Sys.Date())) == 1, paste0("0",month(Sys.Date())), month(Sys.Date())), "-01"))

# all_transactions_nation <- subset(all_transactions_nation, month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365)), "-01") & month <= paste0(year(Sys.Date()),"-", ifelse(nchar(month(Sys.Date())) == 1, paste0("0",month(Sys.Date())), month(Sys.Date())), "-01"))

# df <- subset(df, month >= paste0(year(Sys.Date()-365),"-", ifelse(nchar(month(Sys.Date()-365)) == 1, paste0("0",month(Sys.Date()-365)), month(Sys.Date()-365)), "-01") & month <= paste0(year(Sys.Date()),"-", ifelse(nchar(month(Sys.Date())) == 1, paste0("0",month(Sys.Date())), month(Sys.Date())), "-01"))

## 5. CITY-LEVEL Roll-up for MUMBAI, DELHI & BANGALORE into MUMBAI_MMR, DELHI_NCR & BENGALURU ----
Channel_Archive <- Channel_Archive %>%
  mutate(city = ifelse((city == 'Mumbai' | 
                          city == 'Navi Mumbai'| 
                          city == 'Thane'), 'Mumbai_MMR', 
                       ifelse((city == 'New Delhi' | 
                                 city == 'Delhi' | 
                                 city == 'Ghaziabad' | 
                                 city == 'Faridabad' | 
                                 city == 'Noida' |
                                 city == 'Greater Noida' |
                                 city == 'Gurgaon'), 'Delhi_NCR', 
                              ifelse((city == 'Bangalore' | 
                                        city == 'Bangalore Rural'), 'Bengaluru', 
                                     ifelse(city == 'S?o Paulo', 'Sao Paulo', city))))) %>%
  group_by(month, country, city, channel) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

colnames(App_Archive)[4] <- c("channel")

App_Archive <- App_Archive %>%
  mutate(city = ifelse((city == 'Mumbai' | 
                          city == 'Navi Mumbai'| 
                          city == 'Thane'), 'Mumbai_MMR', 
                       ifelse((city == 'New Delhi' | 
                                 city == 'Delhi' | 
                                 city == 'Ghaziabad' | 
                                 city == 'Faridabad' | 
                                 city == 'Noida' |
                                 city == 'Greater Noida' |
                                 city == 'Gurgaon'), 'Delhi_NCR', 
                              ifelse((city == 'Bangalore' | 
                                        city == 'Bangalore Rural'), 'Bengaluru', 
                                     ifelse(city == 'S?o Paulo', 'Sao Paulo', city))))) %>%
  group_by(month, country, city, channel) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

Device_Archive <- Device_Archive %>%
  mutate(city = ifelse((city == 'Mumbai' | 
                          city == 'Navi Mumbai'| 
                          city == 'Thane'), 'Mumbai_MMR', 
                       ifelse((city == 'New Delhi' | 
                                 city == 'Delhi' | 
                                 city == 'Ghaziabad' | 
                                 city == 'Faridabad' | 
                                 city == 'Noida' |
                                 city == 'Greater Noida' |
                                 city == 'Gurgaon'), 'Delhi_NCR', 
                              ifelse((city == 'Bangalore' | 
                                        city == 'Bangalore Rural'), 'Bengaluru', 
                                     ifelse(city == 'S?o Paulo', 'Sao Paulo', city))))) %>%
  group_by(month, country, city, device) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

Users_Archive <- Users_Archive %>%
  mutate(city = ifelse((city == 'Mumbai' | 
                          city == 'Navi Mumbai'| 
                          city == 'Thane'), 'Mumbai_MMR', 
                       ifelse((city == 'New Delhi' | 
                                 city == 'Delhi' | 
                                 city == 'Ghaziabad' | 
                                 city == 'Faridabad' | 
                                 city == 'Noida' |
                                 city == 'Greater Noida' |
                                 city == 'Gurgaon'), 'Delhi_NCR', 
                              ifelse((city == 'Bangalore' | 
                                        city == 'Bangalore Rural'), 'Bengaluru', 
                                     ifelse(city == 'S?o Paulo', 'Sao Paulo', city))))) %>%
  group_by(month, country, city) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

## 6. Transformations in the GA - Users data ----

# Removing the superflous Channels & bringing everything under DIRECT, ORGANIC, PAID & REFERRAL #

Channel_Archive <- subset(Channel_Archive, 
                          Channel_Archive$channel != "Display" & 
                            Channel_Archive$channel != "Email" & 
                            Channel_Archive$channel != "Facebook" & 
                            Channel_Archive$channel != "Social" & 
                            Channel_Archive$channel != "Desktop Push Notification" & 
                            Channel_Archive$channel != "(Other)")

Channel_Archive$users <- as.numeric(Channel_Archive$users)

Channel_Archive <- Channel_Archive %>%
                        mutate(channels = ifelse(grepl('Paid',Channel_Archive$channel), 'Paid', ifelse(grepl('Organic',Channel_Archive$channel), 'Organic', Channel_Archive$channel)))  %>%
                        group_by(month, country, city, channels) %>%
                        summarise(users = sum(users)) %>%
                        ungroup()

# Removing the superflous Apps & bringing everything under MOBILE APP #

App_Archive <- subset(App_Archive, 
                      App_Archive$channel != "(not set)" & 
                        App_Archive$channel != "Linux" & 
                        App_Archive$channel != "Macintosh" & 
                        App_Archive$channel != "Windows" & 
                        App_Archive$channel != "Windows Phone" &
                        App_Archive$channel != "BlackBerry")

App_Archive$users <- as.numeric(App_Archive$users)

App_Archive <- App_Archive %>%
                    mutate(App = ifelse(grepl('Android',App_Archive$channel), 'Mobile App', ifelse(grepl('iOS',App_Archive$channel), 'Mobile App', App_Archive$channel)))  %>%
                    group_by(month,country, city, App) %>%
                    summarise(users = sum(users)) %>%
                    ungroup()

# Merging Apps data to Channels, making it DIRECT, ORGANIC, PAID, REFERRAL & MOBILE APP #

colnames(App_Archive)[4] <- c("channels")
Channel_Archive <- unique(rbind(Channel_Archive, App_Archive))

# Preparing the Current-Month-Users & Previous-Month-Users data for Shiny Infoboxes #

amp_users$month <- paste0(substr(amp_users$month,1,4),"-",substr(amp_users$month,6,8),"-01")
colnames(amp_users)[3] <- c("amp_users")
amp_users$country2[amp_users$country == "India"] <- "India"
amp_users$country2[amp_users$country == "Indonesia"] <- "Indonesia"
amp_users$country2[amp_users$country == "Philippines"] <- "Philippines"
amp_users$country2[amp_users$country == "Brazil"] <- "Brazil"
amp_users$country2[amp_users$country == "Singapore"] <- "Singapore"
amp_users$country2[is.na(amp_users$country2)] <- "Rest of the World"
amp_users$country <- amp_users$country2
amp_users$country2 <- NULL

amp_users <- amp_users %>%
  group_by(month, country) %>%
  summarise(amp_users = sum(amp_users)) %>%
  ungroup()

amp_users_current_month <- subset(amp_users,(month(month) == month(Sys.Date())) & (year(month) == year(Sys.Date())))
amp_users_previous_month <- subset(amp_users,(month(month) == (month(Sys.Date()) - 1)) & (year(month) == year(Sys.Date())))

## Addition of Mobile App and AMP To -> Total Users & DCH Users ##

DCH_Archive <- merge(DCH_Archive, amp_users, by = c('month', 'country'), all.x = T, all.y = T)
DCH_Archive[is.na(DCH_Archive)] <- 0
DCH_Archive$users <- DCH_Archive$users + DCH_Archive$amp_users
DCH_Archive$amp_users <- NULL

##

users_current_month <- unique(subset(DCH_Archive,(month(DCH_Archive$month) == month(Sys.Date())) & (year(DCH_Archive$month) == year(Sys.Date()))))
users_previous_month <- unique(subset(DCH_Archive,(month(DCH_Archive$month) == (month(Sys.Date()) - 1)) & (year(DCH_Archive$month) == year(Sys.Date()))))

## 

amp_as_device <- amp_users
amp_as_device$device <- "AMP"
amp_as_device <- amp_as_device[,c(1,2,4,3)]
colnames(amp_as_device)[4] <- c("users")

device_users <- unique(rbind(device_users, amp_as_device))

device_users <- device_users %>%
         mutate(device = ifelse(device == "desktop", "web", ifelse(device == "mobile", "sapphire-mweb", ifelse(device == "tablet",                     "tablet", ifelse(device == "AMP", "sapphire-mweb", device))))) %>%
         group_by(month, country, device) %>%
         summarise(users = sum(users)) %>%
         ungroup()
  
# sum(subset(Spec_Profile_Archive, Month_Names == "Apr 2017")[,c("Users")])

Spec_Archive_City <- subset(Spec_Archive_City, city == 'Mumbai_MMR'|
                                 city == 'Delhi_NCR'|
                                 city == 'Hyderabad'|
                                 city == 'Chennai'|
                                 city == 'Pune'|
                                 city == 'Kolkata'|
                                 city == 'Singapore'|
                                 city == 'Jakarta'|
                                 city == 'Bengaluru'|
                                 city == 'Noida')

channel_country <- Channel_Archive %>%
  group_by(month,country,channels) %>%
  summarise(users = sum(users)) %>% 
  ungroup()

channel_country <- subset(channel_country, channel_country$channels != "Mobile App")
channel_country$country2[channel_country$country == "India"] <- "India"
channel_country$country2[channel_country$country == "Indonesia"] <- "Indonesia"
channel_country$country2[channel_country$country == "Philippines"] <- "Philippines"
channel_country$country2[channel_country$country == "Brazil"] <- "Brazil"
channel_country$country2[channel_country$country == "Singapore"] <- "Singapore"
channel_country$country2[is.na(channel_country$country2)] <- "Rest of the World"
channel_country$country <- channel_country$country2
channel_country$country2 <- NULL

channel_country <- unique(rbind(channel_country, mobile_app))
channel_country$month <- as.Date(channel_country$month)

device_country <- Device_Archive %>%
  group_by(month, country, device) %>%
  summarise(users = sum(users)) %>%
  ungroup()

channel_city <- Channel_Archive %>%
  group_by(month, city, channels) %>%
  summarise(users = sum(users)) %>%
  ungroup()

# Calculation of Total Users M-on-M for INDIA, SINGAPORE, PHILIPPINES, BRAZIL, INDONESIA & ROW #

country_users <- ytd_archive %>%
  group_by(month, country) %>%
  summarise(users = sum(users)) %>%
  ungroup()

country_users <- subset(country_users, country == 'India' | 
                          country == 'Singapore' |
                          country == 'Philippines' |
                          country == 'Brazil' |
                          country == 'Indonesia' | 
                          country == 'Rest of the World')

country_users <- merge(country_users, mobile_app_dch_india, by = c("month", "country"), all.x = T, all.y = T)
country_users[is.na(country_users)] <- 0
country_users$users <- country_users$users + country_users$users_app

country_users <- merge(country_users, amp_users, by = c("month", "country"), all.x = T, all.y = T)
country_users[is.na(country_users)] <- 0
country_users$users <- country_users$users + country_users$amp_users
country_users$users_app <- country_users$amp_users <- NULL

country_users$month_names <- as.Date(country_users$month)
country_users$month <- NULL
country_users <- country_users[,c(3,1,2)]

# Calculation of Total Users M-on-M for TOP-10 CITIES #

city_users <- Users_Archive %>%
  group_by(month, city) %>%
  summarise(users = sum(users)) %>% 
  ungroup()

city_users <- subset(city_users, city == 'Mumbai_MMR'|
                                 city == 'Delhi_NCR'|
                       city == 'Hyderabad'|
                       city == 'Chennai'|
                       city == 'Pune'|
                       city == 'Kolkata'|
                       city == 'Singapore'|
                       city == 'Jakarta'|
                       city == 'Bengaluru'|
                       city == 'Sao Paulo')

city_users$month_names <- as.factor(as.yearmon(city_users$month))
city_users$month_names <- as.character(city_users$month_names)
# city_users <- city_users[,c(3,1,2)]

device_country$month_names <- as.Date(device_country$month)
device_country$month <- NULL
# device_country$month_names <- as.character(device_country$month_names)
device_country <- device_country[,c(4,1:3)]

channel_country$month_names <- as.Date(channel_country$month)
channel_country$month <- NULL
# channel_country$month_names <- as.character(channel_country$month_names)
channel_country <- channel_country[,c(4,1:3)]

# Calculation of Total Users M-on-M for TOP-10 SPECIALITIES #

# Spec_Profile_India <- subset(Spec_Profile_India, speciality == 'dermatologist'|
#                                speciality == 'gynecologist-obstetrician'|
#                                speciality == 'ear-nose-throat-ent-specialist'|
#                                speciality == 'orthopedist'|
#                                speciality == 'dentist'|
#                                speciality == 'general-physician'|
#                                speciality == 'psychologist'|
#                                speciality == 'urologist'|
#                                speciality == 'ophthalmologist'|
#                                speciality == 'pediatrician')

# Minute change in dch_users for ComboChart1 #

DCH_Archive$variable <- as.character("Marketplace_Users")
colnames(DCH_Archive) <- c("month_names", "country", "users", "variable")

# Writing section #

write.csv(channel_country, "../Process/channel_country.csv")
write.csv(device_country, "../Process/device_country.csv")
write.csv(users_current_month, "../Process/users_current_month.csv")
write.csv(users_previous_month, "../Process/users_previous_month.csv")
write.csv(Spec_Archive_Global, "../process/spec_archive_global.csv")
write.csv(Spec_Archive_City, "../process/spec_archive_city.csv")
write.csv(DCH_Archive, "../process/dch_archive.csv")
write.csv(device_users, "../process/device_users.csv")
write.csv(DCH_Archive_city, "../process/DCH_Archive_city.csv")

## 7. Transformations in the Db - Transactions data ----

# Preparing the Current-Month-Transactions data for Shiny Infoboxes #

total_transactions_country$month <- as.Date(paste0(total_transactions_country$month,"-01"))
total_transactions_country$vn_attempt <- total_transactions_country$vn_forward <- total_transactions_country$vn_connect <- NULL
rotw_vn_country$month <- as.Date(paste0(rotw_vn_country$month,"-01"))
total_transactions_country <- merge(total_transactions_country, rotw_vn_country, by = c('month', 'country'), all.x = T, all.y = T)
total_transactions_country <- total_transactions_country[c(1:5, 7:9, 6)]
total_transactions_country[is.na(total_transactions_country)] <- 0

transactions_current_month <- subset(total_transactions_country, month(total_transactions_country$month) == month(Sys.Date()) & year(total_transactions_country$month) == year(Sys.Date()))
transactions_current_month$value <- transactions_current_month$book_practo +  transactions_current_month$book_qikwell + transactions_current_month$qikwell + transactions_current_month$vn_attempt + transactions_current_month$widgets

transactions_previous_month <- subset(total_transactions_country, month(total_transactions_country$month) == (month(Sys.Date()) - 1) & year(total_transactions_country$month) == year(Sys.Date()))
transactions_previous_month$value <- transactions_previous_month$book_practo +  transactions_previous_month$book_qikwell + transactions_previous_month$qikwell + transactions_previous_month$vn_attempt + transactions_previous_month$widgets

# COUNTRY-WISE Transactions - BOOK_PRACTO, BOOK_QIKWELL, QIKWELL, VN & WIDGETS #

total_transactions_country$month_names <- as.Date(total_transactions_country$month)
total_transactions_country$month <- NULL
total_transactions_country <- total_transactions_country[,c(ncol(total_transactions_country),1:ncol(total_transactions_country)-1)]
total_transactions_country[is.na(total_transactions_country)] <- 0

total_transactions_country <- melt(total_transactions_country, id.vars = c('month_names', 'country'), measure.vars = c("book_practo", "book_qikwell", "qikwell", "vn_attempt", "vn_forward", "vn_connect", "widgets"), variable_name = "mode")
total_transactions_country$filter <- total_transactions_country$mode
total_transactions_country$mode <- paste0(total_transactions_country$mode, "_", total_transactions_country$country)

# COUNTRY-WISE & MONTH-WISE Transactions - BOOK_PRACTO, BOOK_QIKWELL, QIKWELL, VN & WIDGETS #

total_transactions_country$mode <- NULL

# Preparing the Current-Month-Transactions-City & Previous-Month-Transactions-City data for Shiny Infoboxes #

total_transactions_city$month <- as.Date(paste0(total_transactions_city$month,"-01"))

transactions_current_month_city <- subset(total_transactions_city, month(total_transactions_city$month) == month(Sys.Date()) & year(total_transactions_city$month) == year(Sys.Date()))
transactions_current_month_city$month <- NULL
transactions_current_month_city$transactions <- transactions_current_month_city$book_practo +  transactions_current_month_city$book_qikwell + transactions_current_month_city$qikwell + transactions_current_month_city$vn + transactions_current_month_city$widgets

transactions_current_month_city$book <- transactions_current_month_city$book_practo +  transactions_current_month_city$book_qikwell + transactions_current_month_city$qikwell + transactions_current_month_city$widgets
transactions_current_month_city$book_practo <- transactions_current_month_city$book_qikwell <- transactions_current_month_city$qikwell <- transactions_current_month_city$widgets <- NULL

transactions_current_month_city <- transactions_current_month_city[c(2,1,4,5,3)]
transactions_current_month_city <- transactions_current_month_city[order(-transactions_current_month_city$transactions),]

transactions_previous_month_city <- subset(total_transactions_city, month(total_transactions_city$month) == (month(Sys.Date()) - 1) & year(total_transactions_city$month) == year(Sys.Date()))
transactions_previous_month_city$month <- NULL
transactions_previous_month_city$transactions <- transactions_previous_month_city$book_practo +  transactions_previous_month_city$book_qikwell + transactions_previous_month_city$qikwell + transactions_previous_month_city$vn + transactions_previous_month_city$widgets
transactions_previous_month_city$book <- transactions_previous_month_city$book_practo +  transactions_previous_month_city$book_qikwell + transactions_previous_month_city$qikwell + transactions_previous_month_city$widgets
transactions_previous_month_city$book_practo <- transactions_previous_month_city$book_qikwell <- transactions_previous_month_city$qikwell <- transactions_previous_month_city$widgets <- NULL

transactions_previous_month_city <- transactions_previous_month_city[c(2,1,4,5,3)]
transactions_previous_month_city <- transactions_previous_month_city[order(-transactions_previous_month_city$transactions),]

transactions_month_city <- merge(transactions_current_month_city, transactions_previous_month_city, by = c('city', 'country'))
transactions_month_city <- transactions_month_city[,c(1,2,3,6,4,7,5,8)]
colnames(transactions_month_city)[3:8] <- c('transactions_cm', 'transactions_pm', 'book_cm', 'book_pm', 'vn_cm', 'vn_pm')

# CITY-WISE Transactions - BOOK_PRACTO, BOOK_QIKWELL, QIKWELL, VN & WIDGETS #

total_transactions_city$month_names <- as.Date(total_transactions_city$month)
total_transactions_city$month <- NULL
total_transactions_city <- total_transactions_city[,c(ncol(total_transactions_city),1:ncol(total_transactions_city)-1)]
total_transactions_city[is.na(total_transactions_city)] <- 0

setDT(total_transactions_city)
total_transactions_city <- melt(total_transactions_city, id.vars = c('month_names', 'country', 'city'), measure.vars = c("book_practo", "book_qikwell", "qikwell", "vn", "widgets"))
total_transactions_city$value <- unlist(total_transactions_city$value)
total_transactions_city <- data.frame(total_transactions_city)

total_transactions_city$filter <- total_transactions_city$variable
total_transactions_city$variable <- paste0(total_transactions_city$variable, "_", total_transactions_city$country, "_", total_transactions_city$city)

# Calculation of Total Transactions M-on-M for INDIA, S, P, B & INDONESIA #

total_transactions_country_revised <- subset(total_transactions_country, total_transactions_country$filter != "vn_forward" & total_transactions_country$filter != "vn_attempt")
country_transactions <- total_transactions_country_revised %>%
                              group_by(month_names, country) %>%
                              summarise(transactions = sum(value)) %>%
                              ungroup()

# Calculation of Total Transactions M-on-M for TOP-25 CITIES #

city_transactions <- total_transactions_city %>%
                              group_by(month_names, city) %>%
                              summarise(transactions = sum(value)) %>%
                              ungroup()

# total_transactions_city$city[total_transactions_city$city == 'Bengaluru'] <- 'Bangalore'


# city_transactions <- subset(city_transactions, city == 'Mumbai'|
#                        city == 'Delhi'|
#                        city == 'Hyderabad'|
#                        city == 'Chennai'|
#                        city == 'Pune'|
#                        city == 'Kolkata'|
#                        city == 'Singapore'|
#                        city == 'Jakarta'|
#                        city == 'Banglaore'|
#                        city == 'Noida')

# Book_Practo bifurcation according to the SOURCES (web, mweb + sapphire-mweb, iOS, android)

book_practo_source <- book_practo_source %>%
                          mutate(sources = ifelse(source == 'mweb', 'sapphire-mweb', source)) %>%
                          group_by(month, country, sources) %>%
                          summarise(book_practo = sum(book_practo)) %>%
                          ungroup()

book_practo_source$month <- as.Date(paste0(book_practo_source$month,"-01"))
book_practo_source$month_names <- as.Date(book_practo_source$month)
book_practo_source$month <- NULL
book_practo_source <- subset(book_practo_source, book_practo_source$sources != 'GOK')
book_practo_source <- book_practo_source[,c(4,1,2,3)]

# SPECIALITY-WISE Transactions - BOOK_PRACTO & VN #

Total_Transactions_Spec <- merge(book_practo_spec, vn_spec, by = c('month', 'speciality'), all.x = T, all.y = T)
Total_Transactions_Spec[is.na(Total_Transactions_Spec)] <- 0
Total_Transactions_Spec$transactions <- Total_Transactions_Spec$book_practo + Total_Transactions_Spec$vn

Total_Transactions_Spec$month <- as.Date(paste0(Total_Transactions_Spec$month,"-01"))
Total_Transactions_Spec[is.na(Total_Transactions_Spec)] <- 0
Total_Transactions_Spec$book_practo <- Total_Transactions_Spec$vn <- NULL

Total_Transactions_Spec$speciality <- tolower(Total_Transactions_Spec$speciality)
Total_Transactions_Spec$speciality <- gsub("\\s", "-", Total_Transactions_Spec$speciality)
Total_Transactions_Spec$speciality <- gsub("\\&|\\/", "-", Total_Transactions_Spec$speciality)
Total_Transactions_Spec$speciality <- gsub("\\(|\\)","",Total_Transactions_Spec$speciality)

# SPECIALITY-WISE & CITY-WISE Transactions - BOOK_PRACTO & VN #

book_practo_city_spec$month <- as.Date(paste0(book_practo_city_spec$month,"-01"))
vn_city_spec$month <- as.Date(paste0(vn_city_spec$month,"-01"))

Total_Transactions_Spec_City <- merge(book_practo_city_spec, vn_city_spec, by = c('month', 'country', 'city', 'speciality'), all.x = T, all.y = T)
# Total_Transactions_Spec_City$month <- NULL
Total_Transactions_Spec_City[is.na(Total_Transactions_Spec_City)] <- 0
Total_Transactions_Spec_City$transactions <- Total_Transactions_Spec_City$book_practo + Total_Transactions_Spec_City$vn
Total_Transactions_Spec_City$book_practo <- Total_Transactions_Spec_City$vn <- NULL

Total_Transactions_Spec_City$speciality <- tolower(Total_Transactions_Spec_City$speciality)
Total_Transactions_Spec_City$speciality <- gsub("\\s", "-", Total_Transactions_Spec_City$speciality)
Total_Transactions_Spec_City$speciality <- gsub("\\&|\\/", "-", Total_Transactions_Spec_City$speciality)
Total_Transactions_Spec_City$speciality <- gsub("\\(|\\)","",Total_Transactions_Spec_City$speciality)

Total_Transactions_Spec_City <- subset(Total_Transactions_Spec_City, city == 'Mumbai_MMR'|
                              city == 'Delhi_NCR'|
                              city == 'Hyderabad'|
                              city == 'Chennai'|
                              city == 'Pune'|
                              city == 'Kolkata'|
                              city == 'Singapore'|
                              city == 'Jakarta'|
                              city == 'Bengaluru')

# COUNTRY-WISE Book_Practo CANCELLATION (%) #

cancellations_country$month <- as.Date(paste0(cancellations_country$month,"-01"))
cancellations_country[is.na(cancellations_country)] <- 0
colnames(cancellations_country) <- c('month_names', 'country', 'cancellation_rate')

# CITY-WISE Book_Practo CANCELLATION (%) #

cancellations_city$month <- as.Date(paste0(cancellations_city$month,"-01"))
cancellations_city[is.na(cancellations_city)] <- 0
colnames(cancellations_city) <- c('month_names', 'country', 'city', 'cancellation_rate')

# New Conversion(%) Transactional prep #

unique_patient_tranx$country2[unique_patient_tranx$country == "India"] <- "India"
unique_patient_tranx$country2[unique_patient_tranx$country == "Indonesia"] <- "Indonesia"
unique_patient_tranx$country2[unique_patient_tranx$country == "Philippines"] <- "Philippines"
unique_patient_tranx$country2[unique_patient_tranx$country == "Brazil"] <- "Brazil"
unique_patient_tranx$country2[unique_patient_tranx$country == "Singapore"] <- "Singapore"
unique_patient_tranx$country2[is.na(unique_patient_tranx$country2)] <- "Rest of the World"
unique_patient_tranx$country <- unique_patient_tranx$country2
unique_patient_tranx$country2 <- NULL

unique_patient_tranx <- unique_patient_tranx %>%
  group_by(month, country) %>%
  summarise(unique_patients = sum(total_patients)) %>%
  ungroup()

# unique patient-transactions #

unique_patient_tranx$country[is.na(unique_patient_tranx$country)] <- "Rest of the World"

# unique patient-transactions - CITY Split #
unique_patient_tranx_city$country2[unique_patient_tranx_city$country == "India"] <- "India"
unique_patient_tranx_city$country2[unique_patient_tranx_city$country == "Indonesia"] <- "Indonesia"
unique_patient_tranx_city$country2[unique_patient_tranx_city$country == "Philippines"] <- "Philippines"
unique_patient_tranx_city$country2[unique_patient_tranx_city$country == "Brazil"] <- "Brazil"
unique_patient_tranx_city$country2[unique_patient_tranx_city$country == "Singapore"] <- "Singapore"
unique_patient_tranx_city$country2[is.na(unique_patient_tranx_city$country2)] <- "Rest of the World"
unique_patient_tranx_city$country <- unique_patient_tranx_city$country2
unique_patient_tranx_city$country2 <- NULL

unique_patient_tranx_city <- unique_patient_tranx_city %>%
  group_by(month, country, city) %>%
  summarise(unique_patients = sum(total_patients)) %>%
  ungroup()

unique_patient_tranx_city$country[is.na(unique_patient_tranx_city$country)] <- "Rest of the World"

temp_India <- unique_patient_tranx_city %>%
  filter(country == "India") %>%
  mutate(tier = ifelse((city %in% c('Mumbai_MMR', 'Delhi_NCR', 'Bengaluru', 'Pune', 'Chennai', 'Hyderabad', 'Kolkata')), "Tier-1", "Tier-2")) %>%
  group_by(month, country, tier) %>%
  summarise(unique_patients = sum(unique_patients)) %>%
  ungroup()

temp_International <- unique_patient_tranx_city %>%
  filter(country != "India") %>%
  mutate(tier = "International") %>%
  group_by(month, country, tier) %>%
  summarise(unique_patients = sum(unique_patients)) %>%
  ungroup()

unique_patient_tranx_city <- rbind(temp_India, temp_International)

# Writing section #

write.csv(total_transactions_country, "../Process/total_transactions_country.csv")
write.csv(total_transactions_city, "../Process/total_transactions_city.csv")
write.csv(transactions_current_month, "../Process/transactions_current_month.csv")
write.csv(transactions_previous_month, "../Process/transactions_previous_month.csv")
write.csv(transactions_current_month_city, "../Process/transactions_current_month_city.csv")
write.csv(transactions_previous_month_city, "../Process/transactions_previous_month_city.csv")
write.csv(cancellations_country, "../Process/cancellations_country.csv")
write.csv(cancellations_city, "../Process/cancellations_city.csv")
write.csv(book_practo_source, "../process/book_practo_source.csv")
write.csv(Total_Transactions_Spec, "../process/total_transactions_spec.csv")
write.csv(Total_Transactions_Spec_City, "../process/total_transactions_spec_city.csv")
write.csv(unique_patient_tranx, "../process/unique_patient_tranx.csv")
write.csv(unique_patient_tranx_city, "../process/unique_patient_tranx_city.csv")

## 8. Conversion Percentages (%s) ----

# Calculation of Conversion% M-on-M for INDIA, S, P, B & INDONESIA #

country_transactions <- merge(country_transactions, DCH_Archive, by.all = c("month_names", "country"))
country_transactions$conversion_percentage <- round((country_transactions$transactions/country_transactions$users)*100,2)

is.na(country_transactions) <- sapply(country_transactions, is.infinite)
country_transactions[is.na(country_transactions)] <- 0

# Calculation of Conversion% M-on-M for TOP-25 CITIES #

city_transactions <- merge(city_transactions, city_users, by.x = c("month_names", "city"), by.y = c("month", "city"))
city_transactions[,5] <- NULL
city_transactions$conversion_percentage <- round((city_transactions$transactions/city_transactions$users)*100,2)

is.na(city_transactions) <- sapply(city_transactions, is.infinite)
city_transactions[is.na(city_transactions)] <- 0

city_transactions$country <- city_country_map$country[match(city_transactions$city, city_country_map$city)]
city_transactions$country[is.na(city_transactions$country)] <- "India"
city_transactions <- city_transactions[,c(1,6,2,3,4,5)]

# Calculation of Conversion% Current & Previous months for TOP-25 CITIES #

city_users_current_month <- city_users
city_users_previous_month <- city_users

city_users_current_month <- subset(city_users_current_month, month(city_users_current_month$month) == month(Sys.Date()) & year(city_users_current_month$month) == year(Sys.Date()))
city_users_current_month$month <- city_users_current_month$month_names <- NULL

city_users_previous_month <- subset(city_users_previous_month, month(city_users_previous_month$month) == (month(Sys.Date()) - 1) & year(city_users_previous_month$month) == year(Sys.Date()))
city_users_previous_month$month <- city_users_previous_month$month_names <- NULL

transactions_month_city <- merge(transactions_month_city, city_users_current_month, by = 'city')
transactions_month_city <- merge(transactions_month_city, city_users_previous_month, by = 'city')

colnames(transactions_month_city)[9:10] <- c('users_cm', 'users_pm') 

transactions_month_city$conv_rate_cm <- round(((transactions_month_city$transactions_cm/transactions_month_city$users_cm)*100),2)
transactions_month_city$conv_rate_pm <- round(((transactions_month_city$transactions_pm/transactions_month_city$users_pm)*100),2)
transactions_month_city$users_cm <- transactions_month_city$users_pm <- NULL

transactions_month_city <- transactions_month_city[,c(1, 2, 9, 10, 3:8)]

# SPECIALITY-WISE Calculation of Conversion% M-on-M for INDIA #

Spec_Archive_India <- subset(Spec_Archive_Global, Spec_Archive_Global$country == "India")
Spec_Archive_India$country <- NULL
Total_Transactions_Spec <- merge(Total_Transactions_Spec, Spec_Archive_India, by.x = c("month", "speciality"), by.y = c('date', 'speciality'), all.x = T, all.y = T)
Total_Transactions_Spec[is.na(Total_Transactions_Spec)] <- 0
is.na(Total_Transactions_Spec) <- sapply(Total_Transactions_Spec, is.infinite)
Total_Transactions_Spec[is.na(Total_Transactions_Spec)] <- 0

# Segregation of Unimportant Specs as OTHERS #

Total_Transactions_Spec$conversion_percentage <- round((Total_Transactions_Spec$transactions/Total_Transactions_Spec$users)*100,2)
Total_Transactions_Spec <- subset(Total_Transactions_Spec, 
                                  !is.infinite(Total_Transactions_Spec$conversion_percentage) & 
                                  Total_Transactions_Spec$conversion_percentage != 0 & 
                                  Total_Transactions_Spec$conversion_percentage <= 80 & 
                                  Total_Transactions_Spec$conversion_percentage >= 0)

Total_Transactions_Spec$conversion_percentage <- NULL

Total_Transactions_Spec <- Total_Transactions_Spec %>%
  # mutate(speciality = ifelse((users < 30000), "others", speciality)) %>%
  group_by(month, speciality) %>%
  summarise(transactions = sum(transactions), users = sum(users)) %>%
  ungroup()

Total_Transactions_Spec$conversion_percentage <- round((Total_Transactions_Spec$transactions/Total_Transactions_Spec$users)*100,2)

# Current Month Speciality-level Conversions for SUNBURST CHART #

Total_Transactions_Spec_Current_Month <- subset(Total_Transactions_Spec, 
                                                month(Total_Transactions_Spec$month) == month(Sys.Date()) & 
                                                year(Total_Transactions_Spec$month) == year(Sys.Date()) 
                                                # & Total_Transactions_Spec$users >= 30000
                                                )
Total_Transactions_Spec_Current_Month$month <- NULL
Total_Transactions_Spec_Current_Month <- Total_Transactions_Spec_Current_Month[order(-Total_Transactions_Spec_Current_Month$conversion_percentage),]

Total_Transactions_Spec$temp <- Total_Transactions_Spec_Current_Month$speciality[match(Total_Transactions_Spec$speciality, Total_Transactions_Spec_Current_Month$speciality)]
Total_Transactions_Spec <- subset(Total_Transactions_Spec, !is.na(Total_Transactions_Spec$temp))
Total_Transactions_Spec$temp <- NULL

# SPECIALITY-WISE & CITY-WISE Calculation of Conversion% M-on-M for INDIA #

Total_Transactions_Spec_City <- merge(Total_Transactions_Spec_City, Spec_Archive_City, by.x = c("month", "country", "city", "speciality"), by.y = c("date", "country", "city", "speciality"), all.x = T, all.y = T)
Total_Transactions_Spec_City[is.na(Total_Transactions_Spec_City)] <- 0

Total_Transactions_Spec_City <- Total_Transactions_Spec_City %>%
  mutate(speciality = ifelse((users < 100), "others", speciality)) %>%
  group_by(month, country, city, speciality) %>%
  summarise(transactions = sum(transactions), users = sum(users)) %>%
  ungroup()

Total_Transactions_Spec_City$imp_spec <- Total_Transactions_Spec_Current_Month$speciality[match(Total_Transactions_Spec_City$speciality, Total_Transactions_Spec_Current_Month$speciality)]
Total_Transactions_Spec_City <- subset(Total_Transactions_Spec_City, !is.na(Total_Transactions_Spec_City$imp_spec))
Total_Transactions_Spec_City$imp_spec <- NULL

Total_Transactions_Spec_City$conversion_percentage <- round((Total_Transactions_Spec_City$transactions/Total_Transactions_Spec_City$users)*100,2)

is.na(Total_Transactions_Spec_City) <- sapply(Total_Transactions_Spec_City, is.infinite)
Total_Transactions_Spec_City[is.na(Total_Transactions_Spec_City)] <- 0

Total_Transactions_Spec_City <- subset(Total_Transactions_Spec_City, Total_Transactions_Spec_City$speciality != "others")
Total_Transactions_Spec_City$transactions <- Total_Transactions_Spec_City$users <- NULL
Total_Transactions_Spec_City$country <- "India"

# Minute change in Country_Users for ComboChart1 #

amp_users$variable <- as.character("AMP_Users")
colnames(amp_users)[1:3] <- c("month_names", "country", "users")

country_users$variable <- as.character("Total_Users")
country_users <- unique(rbind(country_users, DCH_Archive, amp_users))

# New Conversion% definition (Unique/Unique) #

colnames(unique_patient_tranx)[1] <- c("month_names")
new_conversion <- merge(unique_patient_tranx, DCH_Archive, by = c("month_names", "country"))
new_conversion$variable <- NULL
new_conversion$conversion_perc <- (new_conversion$unique_patients/ new_conversion$users)*100
new_conversion <- unique(new_conversion)
new_conversion$conversion_perc <- round(new_conversion$conversion_perc, 3)
new_conversion$month_names <- as.Date(new_conversion$month_names)

# Changes for spike in September #

# Tier-wise conversion #



# Writing section #

write.csv(country_users, "../process/country_users.csv")
write.csv(country_transactions, "../process/country_transactions_conversion_percentage.csv")
write.csv(city_transactions, "../process/city_transactions_conversion_percentage.csv")
write.csv(Total_Transactions_Spec, "../process/Spec_Transactions_Conversion_percentage.csv")
write.csv(Total_Transactions_Spec_City, "../process/City_Spec_Transactions_Conversion_percentage.csv")
write.csv(transactions_month_city, "../process/transactions_month_city_data_table.csv")
write.csv(Total_Transactions_Spec_Current_Month, "../process/conversion_spec_current_month.csv")
write.csv(new_conversion, "../process/new_conversion_percentage.csv")

## 9. Users RUN-RATE Line Chart (Channel split) ----

# Channel collation of day-on-day data (Country & Channel split) #

archive_channel_dod <- data.frame(date = as.Date(character()), 
                            country = character(), 
                            city = character(), 
                            channel = character(), 
                            users = numeric())

for(i in 9:(month(Sys.Date())-1)){
  for(j in 1:31){
  print(i)
    print(j)
    
    # tryCatch({
      temp <- read.csv(paste0("../preprocess/channel_specific/daily/channel archive d 2017-0", 
                      i, 
                      "-",
                      ifelse((nchar(j))==1, paste0("0",j), j),
                      ".csv"), stringsAsFactors = F)[,-1]
    # }, error=function(e){
      # print('Y')
    # })
    
  archive_channel_dod <- rbind(archive_channel_dod, temp)
  }
}

for(i in 11:(month(Sys.Date()))){
  for(j in 1:(day(Sys.Date())-1)){
    print(i)
    print(j)
    temp <- read.csv(paste0("../preprocess/channel_specific/daily/channel archive d 2017-", 
                            i, 
                            "-",
                            ifelse((nchar(j))==1, paste0("0",j), j),
                            ".csv"), stringsAsFactors = F)[,-1]
    archive_channel_dod <- rbind(archive_channel_dod, temp)
  }
}

archive_channel_dod <- archive_channel_dod %>%
  group_by(date, country, channel) %>%
  summarise(users = sum(users)) %>%
  ungroup()

archive_channel_dod <- subset(archive_channel_dod, 
                                archive_channel_dod$channel != "Display" & 
                                archive_channel_dod$channel != "Email" & 
                                archive_channel_dod$channel != "Facebook" & 
                                archive_channel_dod$channel != "Social" & 
                                archive_channel_dod$channel != "Desktop Push Notification" & 
                                archive_channel_dod$channel != "(Other)")

archive_channel_dod <- archive_channel_dod %>%
  mutate(channels = ifelse(grepl('Paid',archive_channel_dod$channel), 'Paid', ifelse(grepl('Organic',archive_channel_dod$channel), 'Organic', archive_channel_dod$channel)))  %>%
  group_by(date, country, channels) %>%
  summarise(users = sum(users)) %>%
  ungroup()

archive_channel_dod$day <- day(archive_channel_dod$date)
archive_channel_dod$month_day <- substr(archive_channel_dod$date, 1, 7)
archive_channel_dod$date <- NULL

archive_channel_dod <- archive_channel_dod[,c(4,5,1,2,3)]

# App collation of day-on-day data (Country & Channel split) #

archive_app_dod <- data.frame(date = as.Date(character()), 
                              country = character(), 
                              city = character(), 
                              app = character(), 
                              users = numeric())

for(i in 9:(month(Sys.Date())-1)){
  for(j in 1:31){
    print(i)
    print(j)
    tryCatch({
      temp <- read.csv(paste0("../preprocess/app_specific/daily/app archive d 2017-0", 
                              i, 
                              "-",
                              ifelse((nchar(j))==1, paste0("0",j), j),
                              ".csv"), stringsAsFactors = F)[,-1]
    }, error=function(e){
      temp <- data.frame(date = as.Date(character()), country = character(), city = character(), channel = character(), users = numeric(), stringsAsFactors = F)
    }) 
    
    archive_app_dod <- rbind(archive_app_dod, temp)
  }
}

for(i in 11:(month(Sys.Date()))){
  for(j in 1:(day(Sys.Date())-1)){
    print(i)
    print(j)
    temp <- read.csv(paste0("../preprocess/app_specific/daily/app archive d 2017-", 
                            i, 
                            "-",
                            ifelse((nchar(j))==1, paste0("0",j), j),
                            ".csv"), stringsAsFactors = F)[,-1]
    archive_app_dod <- rbind(archive_app_dod, temp)
  }
}

archive_app_dod <- archive_app_dod %>%
  group_by(date, country, app) %>%
  summarise(users = sum(users)) %>%
  ungroup()

archive_app_dod <- subset(archive_app_dod, 
                          archive_app_dod$app != "(not set)" & 
                            archive_app_dod$app != "Linux" & 
                            archive_app_dod$app != "Macintosh" & 
                            archive_app_dod$app != "Windows" & 
                            archive_app_dod$app != "Windows Phone" &
                            archive_app_dod$app != "BlackBerry")

archive_app_dod <- archive_app_dod %>%
  mutate(App = ifelse(grepl('Android',archive_app_dod$app), 'Mobile App', ifelse(grepl('iOS',archive_app_dod$app), 'Mobile App', archive_app_dod$app)))  %>%
  group_by(date,country, App) %>%
  summarise(users = sum(users)) %>%
  ungroup()

archive_app_dod$day <- day(archive_app_dod$date)
archive_app_dod$month_day <- substr(archive_app_dod$date, 1, 7)
archive_app_dod$date <- NULL

archive_app_dod <- archive_app_dod[,c(4,5,1,2,3)]

# Combining the Channel & App data for the final output day-on-day #

colnames(archive_app_dod)[4] <- c('channels')

archive_dod <- rbind(archive_channel_dod, archive_app_dod)

archive_dod <- archive_dod %>%
                      group_by(month_day, country, channels) %>%
                      mutate(cum_users = cumsum(users)) %>%
                      ungroup()

archive_dod$country2[archive_dod$country == "India"] <- "India"
archive_dod$country2[archive_dod$country == "Indonesia"] <- "Indonesia"
archive_dod$country2[archive_dod$country == "Philippines"] <- "Philippines"
archive_dod$country2[archive_dod$country == "Brazil"] <- "Brazil"
archive_dod$country2[archive_dod$country == "Singapore"] <- "Singapore"
archive_dod$country2[is.na(archive_dod$country2)] <- "Rest of the World"
archive_dod$country <- archive_dod$country2
archive_dod$country2 <- NULL

# Writing section #

write.csv(archive_dod, "../process/archive_dod.csv")

## 10. Transactions RUN-RATE Line Chart (Mode split) ----

transactions_run_rate <- subset(transactions_run_rate, (month(Sys.Date()) - month(transactions_run_rate$date)) <= 2)
rotw_vn_dod <- subset(rotw_vn_dod, (month(Sys.Date()) - month(rotw_vn_dod$date)) <= 2)

transactions_run_rate$vn <- NULL
transactions_run_rate <- merge(transactions_run_rate, rotw_vn_dod, by = c('date', 'country'), all.x = T, all.y = T)
transactions_run_rate <- transactions_run_rate[c(1:5, 7, 6)]

transactions_run_rate[is.na(transactions_run_rate)] <- 0
transactions_run_rate$book <- transactions_run_rate$book_practo + transactions_run_rate$book_qikwell + transactions_run_rate$qikwell + transactions_run_rate$widgets
transactions_run_rate$tranx <- transactions_run_rate$book + transactions_run_rate$vn
transactions_run_rate$book_practo <- transactions_run_rate$book_qikwell <- transactions_run_rate$qikwell <- transactions_run_rate$widgets <- NULL

transactions_run_rate$day <- day(transactions_run_rate$date)
transactions_run_rate$month_day <- substr(transactions_run_rate$date, 1, 7)
transactions_run_rate$date <- NULL

transactions_run_rate <- transactions_run_rate[,c(5,6,1,3,2,4)]
transactions_run_rate <- transactions_run_rate[order(transactions_run_rate$day, transactions_run_rate$month_day),]

transactions_run_rate <- transactions_run_rate %>%
  group_by(month_day, country) %>%
  mutate(cum_book = cumsum(book), cum_vn = cumsum(vn), cum_tranx = cumsum(tranx)) %>%
  ungroup()

# Writing section #

write.csv(transactions_run_rate, "../process/transactions_dod.csv")

## 11. LIQUIDITY trends & valueBoxes' population ----

# Collating data-cut for Current & Previous month Liquidity Score #

summ_trans$month <- as.Date(paste0(summ_trans$month,"-01"))
liquidity_current_month <- subset(summ_trans, month(summ_trans$month) == month(Sys.Date()) & year(summ_trans$month) == year(Sys.Date()))
liquidity_previous_month <- subset(summ_trans, month(summ_trans$month) == (month(Sys.Date())-1) & year(summ_trans$month) == year(Sys.Date()))

# Month-on-Month trend for Liquidity (Bar Chart) #

liquidity_mom <- summ_trans[,c(1,6)]
liquidity_mom$liq <- round(liquidity_mom$liq*100,2)

# Liquidity Charting #

temp_2$month <- as.Date(paste0(temp_2$month,"-01"))
temp_2 <- temp_2[order(temp_2$month),]
temp_2$month <- substr(temp_2$month, 1, 7)

# Writing section #

write.csv(temp_2, "../process/temp_2.csv")
write.csv(liquidity_current_month, "../process/liquidity_current_month.csv")
write.csv(liquidity_previous_month, "../process/liquidity_previous_month.csv")
write.csv(liquidity_mom, "../process/liquidity_mom.csv")

## 12. GMV & VPE trends & valueBoxes' population ----

final_vpe_gmv$month <- as.Date(paste0(final_vpe_gmv$month, "-01"))
vpe_current_month <- subset(final_vpe_gmv, month(final_vpe_gmv$month) == month(Sys.Date()) & year(final_vpe_gmv$month) == year(Sys.Date()))
vpe_previous_month <- subset(final_vpe_gmv, month(final_vpe_gmv$month) == (month(Sys.Date())-1) & year(final_vpe_gmv$month) == year(Sys.Date()))

gmv_vpe <- final_vpe_gmv[c(1,(ncol(final_vpe_gmv))-1,ncol(final_vpe_gmv))]

segregated_gmv_vpe <- final_vpe_gmv[c(1:10)]
segregated_gmv <- melt(segregated_gmv_vpe, id.vars = c('month'), measure.vars = c("gmv_clinic", "gmv_mm", "gmv_sa"), variable_name = "gmv")
segregated_gmv$bu <- substr(segregated_gmv$gmv, 5, nchar(as.character(segregated_gmv$gmv)))

segregated_vpe <- melt(segregated_gmv_vpe, id.vars = c('month'), measure.vars = c("avg_vpe_clinic", "avg_vpe_mm", "avg_vpe_sa"), variable_name = "vpe")
segregated_vpe$bu <- substr(segregated_vpe$vpe, 9, nchar(as.character(segregated_vpe$vpe)))

segregated_trans <- melt(segregated_gmv_vpe, id.vars = c('month'), measure.vars = c("trans_clinic", "trans_mm", "trans_sa"), variable_name = "trans")
segregated_trans$bu <- substr(segregated_trans$trans, 7, nchar(as.character(segregated_trans$trans)))

temp <- merge(segregated_vpe, segregated_trans, by = c('month', 'bu'))
segregated_vpe <- temp[,c(1,2,4,6)]
colnames(segregated_vpe)[3:4] <- c('vpe', 'trans')

# Writing section #

write.csv(vpe_current_month, "../process/vpe_current_month.csv")
write.csv(vpe_previous_month, "../process/vpe_previous_month.csv")
write.csv(gmv_vpe, "../process/gmv_vpe.csv")
write.csv(segregated_gmv, "../process/segregated_gmv.csv")
write.csv(segregated_vpe, "../process/segregated_vpe.csv")
write.csv(final_vpe_gmv, "../process/final_vpe_gmv.csv")

## 13. Reach Monetization tweaks & plot variables ----

##   PLOT A: Plot of transactions by month and type
temp_1 <- all_transactions
temp_2 <- all_transactions_nation
temp_2$city_new <- "Nation"
temp_2$country <- "Nation"

data_plot_A <- rbind(temp_1[,c("month","city_new", "country", "perc_abs_plus_vn_connect_patients")],
                     temp_2[,c("month","city_new", "country", "perc_abs_plus_vn_connect_patients")])

# temp <- all_transactions
# temp$cust_org_abs_appointments <- temp$cust_abs_appointments - temp$cust_reach_card_appointments
# temp$non_cust_transactions <- temp$all_transactions - temp$cust_abs_appointments - temp$cust_vn_connected_calls
# temp <- subset(temp, select = -c(cust_abs_appointments, all_transactions))
# colnames(temp)[4:7] <- c("Reach card ABS, Customers", "Organic VN, Customers", "Organic ABS, Customers", "VN + ABS, Non-Customers")
# data_plot_A <- melt(temp, id = c("month", "city_new", "country"))

##   PLOT B: doughnut chart for abs and reach card appointments by source
data_plot_B <- subset(df, substr(month,1,7) == substr(Sys.Date(),1,7))

# data_plot_B <- subset(df_reach, substr(month,1,7) == substr(Sys.Date(),1,7))

##   PLOT C: trend of abs and reach card appointments by source

data_plot_C <- df

##   PLOT D: Trend of # of live customers and # live subscriptions
temp <- subset(df, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
data_plot_D <- unique(temp[,c("practice_id", "month", "final_num_subscriptions", "city_new", "country")])

# data_plot_D <- subset(df_reach, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
# data_plot_D <- unique(data_plot_D[,c("practice_id", "month", "final_num_subscriptions", "city_new", "country")])

##   PLOT E: NEW ACQUISITIONS VS EXISTING CUSTOMERS
temp <- subset(df, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
temp <- temp[,c("practice_id", "month", "final_num_subscriptions", "city_new", "country")]
temp <- temp[order(temp$practice_id,temp$month),]

# Identify when the practice first bought a subscription
temp <- temp %>%
  group_by(practice_id) %>%
  mutate(first_month = min(month))

temp$new <- ifelse(temp$month == temp$first_month, 1, 0)

data_plot_E <- temp %>%
  group_by(month, city_new, country) %>% 
  summarise(new_acquisitions = length(unique(practice_id[new == 1])),
            repeat_customers = length(unique(practice_id[new == 0])))

setDT(data_plot_E)
data_plot_E <- melt(data_plot_E, id = c("month", "city_new", "country"))

# data_plot_D <- data_plot_D[order(data_plot_D$practice_id,data_plot_D$month),]
# 
# data_plot_E <- data_plot_D %>%
#   group_by(practice_id) %>%
#   mutate(first_month = min(month))
# 
# data_plot_E$new <- ifelse(data_plot_E$month == data_plot_E$first_month, 1, 0)
# 
# data_plot_E <- data_plot_E %>%
#   group_by(month, city_new, country) %>% 
#   summarise(new_acquisitions = length(unique(practice_id[new == 1])),
#             repeat_customers = length(unique(practice_id[new == 0])))
# 
# setDT(data_plot_E)
# data_plot_E <- melt(data_plot_E, id = c("month", "city_new", "country"))

##   PLOT F: Average subscription duration trend
temp <- subset(df, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
temp <- temp[,c("practice_id", "month", "final_num_subscriptions", "avg_subscription_duration", "city_new", "country")]
data_plot_F <- unique(temp)

# data_plot_F <- subset(df_reach, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
# data_plot_F <- data_plot_F[,c("practice_id", "month", "final_num_subscriptions", "avg_subscription_duration", "city_new", "country")]
# data_plot_F <- unique(data_plot_F)

##   PLOT G: CTR trend
temp <- subset(df, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
data_plot_G <- unique(temp[,c("practice_id", "month", "city_new", "country", "source", "impressions", "clicks")])

# data_plot_G <- subset(df_reach, impressions > 0) # As directed by Pruthvi. Practices with zero impressions removed
# data_plot_G <- unique(data_plot_G[,c("practice_id", "month", "city_new", "country", "source", "impressions", "clicks")])

##   PLOT H: Transactions liquidity (ONLY FOR INDIA)
data_plot_H <- df %>%
  select(practice_id, month, city_new, country, abs_appointments, reach_card_appointments) %>%
  group_by(practice_id, month, city_new, country) %>%
  summarise(cust_abs_appointments = sum(abs_appointments),
            cust_reach_card_appointments = sum(reach_card_appointments)) %>%
  ungroup()

data_plot_H <- merge(x=data_plot_H, y=vn_calls[,c("month", "practice_id", "vn_connect_calls")], by=c("month", "practice_id"), all.x = T)
data_plot_H$vn_connect_calls[is.na(data_plot_H$vn_connect_calls)] <- 0

data_plot_H$transactions <- data_plot_H$cust_abs_appointments + data_plot_H$vn_connect_calls

data_plot_H$transactions_bucket <- ifelse(data_plot_H$transactions == 0, "0",
                                   ifelse(data_plot_H$transactions >= 1 & data_plot_H$transactions <= 5 , "1-5",
                                          ifelse(data_plot_H$transactions >= 6 & data_plot_H$transactions <= 10 , "6-10",
                                                 ifelse(data_plot_H$transactions >= 11 & data_plot_H$transactions <= 15 , "11-15",
                                                        ifelse(data_plot_H$transactions >= 16 & data_plot_H$transactions <= 20 , "16-20",
                                                               ifelse(data_plot_H$transactions >= 21 & data_plot_H$transactions <= 25 , "21-25",
                                                                      ifelse(data_plot_H$transactions >= 26 & data_plot_H$transactions <= 30 , "26-30",
                                                                             ifelse(data_plot_H$transactions >= 31 & data_plot_H$transactions <= 35 , "31-35",
                                                                                    ifelse(data_plot_H$transactions >= 36 & data_plot_H$transactions <= 40 , "36-40",
                                                                                           ifelse(data_plot_H$transactions >= 41 & data_plot_H$transactions <= 45 , "41-45",
                                                                                                  ifelse(data_plot_H$transactions >= 46 & data_plot_H$transactions <= 50 , "46-50", "50+")))))))))))

data_plot_H$transactions_bucket <- as.factor(data_plot_H$transactions_bucket)
data_plot_H$month <- as.character(data_plot_H$month)

# data_plot_H <- df_reach %>%
#   select(practice_id, month, city_new, country, abs_appointments, reach_card_appointments) %>%
#   group_by(practice_id, month, city_new, country) %>%
#   summarise(cust_abs_appointments = sum(abs_appointments),
#             cust_reach_card_appointments = sum(reach_card_appointments)) %>%
#   ungroup()
# 
# data_plot_H <- merge(x=data_plot_H, y=vn_calls[,c("month", "practice_id", "vn_connect")], by=c("month", "practice_id"), all.x = T)
# data_plot_H$vn_connect[is.na(data_plot_H$vn_connect)] <- 0
# 
# data_plot_H$transactions <- data_plot_H$cust_abs_appointments + data_plot_H$vn_connect
# 
# data_plot_H$transactions_bucket <- ifelse(data_plot_H$transactions == 0, "0",
#                                    ifelse(data_plot_H$transactions >= 1 & data_plot_H$transactions <= 5 , "01-05",
#                                           ifelse(data_plot_H$transactions >= 6 & data_plot_H$transactions <= 10 , "06-10",
#                                                  ifelse(data_plot_H$transactions >= 11 & data_plot_H$transactions <= 15 , "11-15",
#                                                         ifelse(data_plot_H$transactions >= 16 & data_plot_H$transactions <= 20 , "16-20",
#                                                                ifelse(data_plot_H$transactions >= 21 & data_plot_H$transactions <= 25 , "21-25",
#                                                                       ifelse(data_plot_H$transactions >= 26 & data_plot_H$transactions <= 30 , "26-30",
#                                                                              ifelse(data_plot_H$transactions >= 31 & data_plot_H$transactions <= 35 , "31-35",
#                                                                                     ifelse(data_plot_H$transactions >= 36 & data_plot_H$transactions <= 40 , "36-40",
#                                                                                            ifelse(data_plot_H$transactions >= 41 & data_plot_H$transactions <= 45 , "41-45",
#                                                                                                   ifelse(data_plot_H$transactions >= 46 & data_plot_H$transactions <= 50 , "46-50", "50+")))))))))))
# 
# data_plot_H$transactions_bucket <- as.factor(data_plot_H$transactions_bucket)
# data_plot_H$month <- as.character(data_plot_H$month)

##   WRITING SECTION
write.csv(data_plot_A, "../process/tranx_type.csv")
write.csv(data_plot_B, "../process/pie_absVSreach.csv")
write.csv(data_plot_C, "../process/absVSreach.csv")
write.csv(data_plot_D, "../process/live_cust_sub.csv")
write.csv(data_plot_E, "../process/newVSexisting.csv")
write.csv(data_plot_F, "../process/avg_duration.csv")
write.csv(data_plot_G, "../process/ctr.csv")
write.csv(data_plot_H, "../process/liq_reach.csv")


