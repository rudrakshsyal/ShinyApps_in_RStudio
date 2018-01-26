setwd("~/Documents/MIS/preprocess")

## 1. Set WORKING DIRECTORY & Loading LIBRARIES ----
library(readxl)
library(lubridate)
library(dplyr)
library(reshape)
library(stringr)
library(tidyr)

## 2. Loading the Workspace Image of Load.R ----
load("../load/load.RData")

## 3A. Aggregating & Archiving the Channel files [monthly] ----
users_channel_monthly$country <- city_country_map$country[match(users_channel_monthly$city, city_country_map$city)]
users_channel_monthly$country[is.na(users_channel_monthly$country)] <- "Unknown"
users_channel_monthly$date <- paste0(substr(users_channel_monthly$month,1,4),"-",substr(users_channel_monthly$month,6,8),"-01")
users_channel_monthly$month <- NULL
colnames(users_channel_monthly) <- c("city", "channel", "users", "country", "date")
users_channel_monthly <- users_channel_monthly[,c(5, 4, 1:2, 3)]
users_channel_monthly <- users_channel_monthly[order(users_channel_monthly$date),]
users_channel_monthly$date <- as.Date(users_channel_monthly$date, format = '%Y-%m-%d')

write.csv(users_channel_monthly, paste0("./channel_specific/monthly/channel archive m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 3B. Aggregating & Archiving the Channel files [daily] ----
users_channel_daily$country <- city_country_map$country[match(users_channel_daily$city, city_country_map$city)]
users_channel_daily$country[is.na(users_channel_daily$country)] <- "Unknown"

users_channel_daily$date <- as.Date(paste0(year(Sys.Date()),"-", month(Sys.Date()),"-", as.numeric(users_channel_daily$day)))
users_channel_daily$day <- NULL
colnames(users_channel_daily) <- c("city", "channel", "users", "country", "date")
users_channel_daily <- users_channel_daily[,c(5,4,1:3)]

users_channel_daily <- subset(users_channel_daily, users_channel_daily$date == Sys.Date() - 1)

write.csv(users_channel_daily, paste0("./channel_specific/daily/channel archive d ", Sys.Date() - 1, ".csv"))

## 4A. Aggregating & Archiving the Device files [monthly] ----
colnames(users_device_monthly) <- tolower(colnames(users_device_monthly))

users_device_monthly$country <- city_country_map$country[match(users_device_monthly$city, city_country_map$city)]
users_device_monthly$country[is.na(users_device_monthly$country)] <- "Unknown"

users_device_monthly$date <- paste0(substr(users_device_monthly$month,1,4),"-",substr(users_device_monthly$month,6,8),"-01")
users_device_monthly$month <- NULL
colnames(users_device_monthly) <- c("city", "device", "users", "country", "date")
users_device_monthly <- users_device_monthly[,c(5, 4, 1:2, 3)]
users_device_monthly <- users_device_monthly[order(users_device_monthly$date),]
users_device_monthly$date <- as.Date(users_device_monthly$date, format = '%Y-%m-%d')

write.csv(users_device_monthly, paste0("./device_specific/monthly/device archive m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 4B. Aggregating & Archiving the Device files [daily] ----
colnames(users_device_daily) <- tolower(colnames(users_device_daily))

users_device_daily$country <- city_country_map$country[match(users_device_daily$city, city_country_map$city)]
users_device_daily$country[is.na(users_device_daily$country)] <- "Unknown"

users_device_daily$date <- as.Date(paste0(year(Sys.Date()),"-", month(Sys.Date()),"-", as.numeric(users_device_daily$day)))
users_device_daily$day <- NULL
colnames(users_device_daily) <- c("city", "device", "users", "country", "date")
users_device_daily <- users_device_daily[,c(5,4,1:3)]

users_device_daily <- subset(users_device_daily, users_device_daily$date == Sys.Date() - 1)

write.csv(users_device_daily, paste0("./device_specific/daily/device archive d ", Sys.Date() - 1, ".csv"))

## 5A. Aggregating & Archiving the App files [monthly] ----
colnames(users_app_monthly) <- tolower(colnames(users_app_monthly))

users_app_monthly$country <- city_country_map$country[match(users_app_monthly$city, city_country_map$city)]
users_app_monthly$country[is.na(users_app_monthly$country)] <- "Unknown"

users_app_monthly$date <- paste0(substr(users_app_monthly$month,1,4),"-",substr(users_app_monthly$month,6,8),"-01")
users_app_monthly$month <- NULL
users_app_monthly$app <- "Mobile App"
colnames(users_app_monthly) <- c("city", "users", "country", "date", "app")
users_app_monthly <- users_app_monthly[,c(4, 3, 1, 5, 2)]
users_app_monthly <- users_app_monthly[order(users_app_monthly$date),]
users_app_monthly$date <- as.Date(users_app_monthly$date, format = '%Y-%m-%d')

write.csv(users_app_monthly, paste0("./app_specific//monthly/app archive m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 5B. Aggregating & Archiving the App files [daily] ----
colnames(users_app_daily) <- tolower(colnames(users_app_daily))

users_app_daily$country <- city_country_map$country[match(users_app_daily$city, city_country_map$city)]
users_app_daily$country[is.na(users_app_daily$country)] <- "Unknown"

users_app_daily$date <- as.Date(paste0(year(Sys.Date()),"-", month(Sys.Date()),"-", as.numeric(users_app_daily$day)))
users_app_daily$day <- users_app_daily$month <- NULL
users_app_daily$app <- "Mobile App"
colnames(users_app_daily) <- c("city", "users", "country", "date", "app")
users_app_daily <- users_app_daily[,c(4, 3, 1, 5, 2)]

users_app_daily <- subset(users_app_daily, users_app_daily$date == Sys.Date() - 1)

write.csv(users_app_daily, paste0("./app_specific//daily/app archive d ", Sys.Date() - 1, ".csv"))

## 5C. Aggregating & Archiving the App files [YTD] ----
colnames(users_app_ytd) <- tolower(colnames(users_app_ytd))

users_app_ytd$country <- city_country_map$country[match(users_app_ytd$city, city_country_map$city)]
users_app_ytd$country[is.na(users_app_ytd$country)] <- "Unknown"

users_app_ytd$date <- paste0(substr(users_app_ytd$month,1,4),"-",substr(users_app_ytd$month,6,8),"-01")
users_app_ytd$month <- NULL
users_app_ytd$app <- "Mobile App"
colnames(users_app_ytd) <- c("city", "users", "country", "date", "app")
users_app_ytd <- users_app_ytd[,c(4, 3, 1, 5, 2)]

## 6A. Aggregating & Archiving the Total_Users files [monthly] ----
total_users_monthly$country <- city_country_map$country[match(total_users_monthly$city, city_country_map$city)]
total_users_monthly$country[is.na(total_users_monthly$country)] <- "Unknown"

colnames(total_users_monthly) <- tolower(colnames(total_users_monthly))

total_users_monthly$date <- paste0(substr(total_users_monthly$month,1,4),"-",substr(total_users_monthly$month,6,8),"-01")
total_users_monthly$month <- NULL
colnames(total_users_monthly) <- c("city", "users", "country", "date")
total_users_monthly <- total_users_monthly[,c(4, 3, 1, 2)]
total_users_monthly <- total_users_monthly[order(total_users_monthly$date),]
total_users_monthly$date <- as.Date(total_users_monthly$date, format = '%Y-%m-%d')

total_users_monthly <- total_users_monthly %>%
                            group_by(date, country, city) %>%
                            summarise(users = sum(users)) %>%
                            ungroup()

write.csv(total_users_monthly, paste0("./total_users/monthly/total_users m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 6B. Aggregating & Archiving the Total_Users files [daily] ----
total_users_daily$date <- as.Date(paste0(year(Sys.Date()),"-", month(Sys.Date()),"-", as.numeric(total_users_daily$day)))
total_users_daily$day <- NULL

total_users_daily$country <- city_country_map$country[match(total_users_daily$city, city_country_map$city)]
total_users_daily$country[is.na(total_users_daily$country)] <- "Unknown"

colnames(total_users_daily) <- c("users", "city", "date", "country")

total_users_daily <- total_users_daily %>%
                      group_by(date, country, city) %>%
                      summarise(users = sum(users)) %>%
                      ungroup()

total_users_daily <- subset(total_users_daily, total_users_daily$date == Sys.Date() - 1)

write.csv(total_users_daily, paste0("./total_users/daily/total_users d ", Sys.Date() - 1, ".csv"))

## 7A. Aggregating & Archiving the YTD_Users files [monthly & daily] ----
colnames(ytd_users) <- tolower(colnames(ytd_users))

ytd_users$date <- paste0(substr(ytd_users$month,1,4),"-",substr(ytd_users$month,6,8),"-01")
ytd_users$month <- NULL
ytd_users$country2[ytd_users$country == "India"] <- "India"
ytd_users$country2[ytd_users$country == "Indonesia"] <- "Indonesia"
ytd_users$country2[ytd_users$country == "Philippines"] <- "Philippines"
ytd_users$country2[ytd_users$country == "Brazil"] <- "Brazil"
ytd_users$country2[ytd_users$country == "Singapore"] <- "Singapore"
ytd_users$country2[is.na(ytd_users$country2)] <- "Rest of the World"
ytd_users$country <- ytd_users$country2
ytd_users$country2 <- NULL

colnames(ytd_users) <- c("country", "users", "date")
ytd_users <- ytd_users[,c(3, 1, 2)]
ytd_users <- ytd_users[order(ytd_users$date),]
ytd_users$date <- as.Date(ytd_users$date, format = '%Y-%m-%d')

ytd_users <- ytd_users %>%
  # mutate(source2 = ifelse((grepl('android', tolower(ytd_users$source)) | grepl('ios', tolower(ytd_users$source))), "Mobile App", "Fabric")) %>%
  group_by(date, country) %>%
           # source2) 
  summarise(users = sum(users)) %>%
  ungroup()

write.csv(ytd_users, paste0("./ytd_users/ytd_users wo source ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 8A. Aggregating & Archiving the DCH_Users files [monthly] ----
colnames(dch_users_monthly) <- tolower(colnames(dch_users_monthly))

dch_users_monthly$date <- paste0(substr(dch_users_monthly$month,1,4),"-",substr(dch_users_monthly$month,6,8),"-01")
dch_users_monthly$month <- NULL
dch_users_monthly$country <- "India"
dch_users_monthly <- dch_users_monthly[,c(2, 3, 1)]
dch_users_monthly <- dch_users_monthly[order(dch_users_monthly$date),]
dch_users_monthly$date <- as.Date(dch_users_monthly$date, format = '%Y-%m-%d')

write.csv(dch_users_monthly, paste0("./dch_users/monthly/dch_users m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 8B. Aggregating & Archiving the DCH_Users files [daily] ----
colnames(dch_users_daily) <- c("users")

dch_users_daily$country <- "India"
dch_users_daily$date <- paste0(year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01")
dch_users_daily <- dch_users_daily[,c(3, 2, 1)]
dch_users_daily <- dch_users_daily[order(dch_users_daily$date),]
dch_users_daily$date <- as.Date(dch_users_daily$date, format = '%Y-%m-%d')

write.csv(dch_users_daily, paste0("./dch_users/daily/dch_users d ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 8C. Aggregating & Archiving the DCH_Users files with CITY SPLIT ----

# monthly
colnames(dch_users_monthly_city) <- tolower(colnames(dch_users_monthly_city))

dch_users_monthly_city$date <- paste0(substr(dch_users_monthly_city$month,1,4),"-",substr(dch_users_monthly_city$month,6,8),"-01")
dch_users_monthly_city$month <- NULL
dch_users_monthly_city$country <- city_country_map$country[match(dch_users_monthly_city$city, city_country_map$city)]
dch_users_monthly_city$country[is.na(dch_users_monthly_city$country)] <- "Unknown"

dch_users_monthly_city <- dch_users_monthly_city[,c(3, 4, 1, 2)]
dch_users_monthly_city <- dch_users_monthly_city[order(dch_users_monthly_city$date),]
dch_users_monthly_city$date <- as.Date(dch_users_monthly_city$date, format = '%Y-%m-%d')

# daily
dch_users_daily_city$country <- city_country_map$country[match(dch_users_daily_city$city, city_country_map$city)]
dch_users_daily_city$country[is.na(dch_users_daily_city$country)] <- "Unknown"

dch_users_daily_city$date <- paste0(year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01")
dch_users_daily_city <- dch_users_daily_city[,c(4, 3, 1, 2)]
dch_users_daily_city <- dch_users_daily_city[order(dch_users_daily_city$date),]
dch_users_daily_city$date <- as.Date(dch_users_daily_city$date, format = '%Y-%m-%d')

# amp
amp_users_city$date <- paste0(substr(amp_users_city$month,1,4),"-",substr(amp_users_city$month,6,8),"-01")
amp_users_city <- amp_users_city[,c(5, 2, 3, 4)]

## 9. Aggregating & Archiving the Speciality_Users file ----

# users_speciality <- read.csv("../preprocess/users_speciality.csv", stringsAsFactors = F)[,-1]
# users_speciality_city <- read.csv("../preprocess/users_speciality_city.csv", stringsAsFactors = F)[,-1]
# users_speciality_global <- read.csv("../preprocess/users_speciality_global.csv", stringsAsFactors = F)[,-1]

colnames(users_speciality) <- c("landing_page", "month", "users")
users_speciality$landing_page <- gsub("s?o-paulo", "Sao-Paulo", users_speciality$landing_page)

specs$speciality <- tolower(specs$speciality)
specs$speciality <- gsub("\\s", "-", specs$speciality)
specs$speciality <- gsub("\\&|\\/", "-", specs$speciality)
specs$speciality <- gsub("\\(|\\)","",specs$speciality)

cities <- data.frame(gsub("\\s", "-", total_users_daily$city))
colnames(cities) <- c("city")
# cities <- gsub("S??o-Paulo", "Sao-Paulo", total_users_daily$city)

# Speciality & City Segregation - Chetan's Approach #

spec <- as.vector(specs$speciality)
city <- tolower(unique(as.vector(cities$city)))

find_spec <- function(str){
  x <- specs[which(apply(specs[1], 1, function(x) grepl(x, str))),1]
  # if(length(x) == 0){
  #   ""
  # }else{ x }
}

find_city <- function(str){
  y <- unique(as.vector(str[[1]]))
  y[which(y %in% city)]
}

users_speciality$landing_page <- tolower(users_speciality$landing_page)
users_speciality$landing_page <- gsub("bangalore", "bengaluru", users_speciality$landing_page)
users_speciality$landing_page_new <- strsplit(users_speciality$landing_page, "-")

system.time(users_speciality$spec_1 <- apply(users_speciality[1], 1, function(x) find_spec(x)))
system.time(users_speciality$city_1 <- apply(users_speciality[4], 1, function(x) find_city(x)))

users_speciality$landing_page_new <- strsplit(users_speciality$landing_page, "/")
system.time(users_speciality$city_2 <- apply(users_speciality[4], 1, function(x) find_city(x)))

users_speciality$spec <- ifelse(users_speciality$spec_1 == "character(0)", "", users_speciality$spec_1)
users_speciality$city <- ifelse(users_speciality$city_1 == "character(0)" & users_speciality$city_2 == "character(0)", "", ifelse(users_speciality$city_1 == "character(0)" & users_speciality$city_2 != "character(0)", users_speciality$city_2, users_speciality$city_1))

users_speciality$speciality <- as.character(users_speciality$spec)
users_speciality$city <- as.character(users_speciality$city)
users_speciality$landing_page_new <- users_speciality$spec_1 <- users_speciality$spec_2 <- users_speciality$city_1 <- users_speciality$city_2 <- users_speciality$landing_page <- users_speciality$spec <- NULL
users_speciality$date <- paste0(substr(users_speciality$month,1,4),"-",substr(users_speciality$month,5,7),"-01")
users_speciality$month <- NULL

substr(users_speciality$city,1,1) <- paste0(toupper(substr(users_speciality$city, 1, 1)))
users_speciality$country <- city_country_map$country[match(users_speciality$city, city_country_map$city)]
users_speciality$country[is.na(users_speciality$country)] <- ""
users_speciality <- users_speciality[c(4,5,2,3,1)]

users_speciality_global <- users_speciality %>%
  group_by(date, country, speciality) %>%
  summarise(users = sum(users)) %>%
  ungroup()

users_speciality_city <- users_speciality %>%
  group_by(date, country, speciality, city) %>%
  summarise(users = sum(users)) %>%
  ungroup()

users_speciality_global <- subset(users_speciality_global, !grepl(',',users_speciality_global$speciality) 
                             & users_speciality_global$speciality != "")

users_speciality_city <- subset(users_speciality_city, !grepl(',',users_speciality_city$speciality)
                                  & !grepl(',',users_speciality_city$city)
                                  & users_speciality_city$speciality != ""
                                  & users_speciality_city$city != "")

users_speciality_global$country2[users_speciality_global$country == "India"] <- "India"
users_speciality_global$country2[users_speciality_global$country == "Indonesia"] <- "Indonesia"
users_speciality_global$country2[users_speciality_global$country == "Philippines"] <- "Philippines"
users_speciality_global$country2[users_speciality_global$country == "Brazil"] <- "Brazil"
users_speciality_global$country2[users_speciality_global$country == "Singapore"] <- "Singapore"
users_speciality_global$country2[is.na(users_speciality_global$country2)] <- "Rest of the World"
users_speciality_global$country <- users_speciality_global$country2
users_speciality_global$country2 <- NULL

users_speciality_city$country2[users_speciality_city$country == "India"] <- "India"
users_speciality_city$country2[users_speciality_city$country == "Indonesia"] <- "Indonesia"
users_speciality_city$country2[users_speciality_city$country == "Philippines"] <- "Philippines"
users_speciality_city$country2[users_speciality_city$country == "Brazil"] <- "Brazil"
users_speciality_city$country2[users_speciality_city$country == "Singapore"] <- "Singapore"
users_speciality_city$country2[is.na(users_speciality_city$country2)] <- "Rest of the World"
users_speciality_city$country <- users_speciality_city$country2
users_speciality_city$country2 <- NULL

##
write.csv(users_speciality, "../preprocess/users_speciality.csv")
write.csv(users_speciality_global, "../preprocess/users_speciality_global.csv")
write.csv(users_speciality_city, "../preprocess/users_speciality_city.csv")
##

write.csv(users_speciality_global, paste0("./spec_specific/monthly/speciality archive global m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

write.csv(users_speciality_city, paste0("./spec_specific/monthly/speciality archive city m ", year(Sys.Date()), "-", ifelse((nchar(month(Sys.Date()))==1), paste0("0", month(Sys.Date())), month(Sys.Date())), "-01.csv"))

## 10. CITY-LEVEL Roll-up for MUMBAI, DELHI & BANGALORE into MUMBAI_MMR, DELHI_NCR & BENGALURU ----
book_practo_city_spec <- book_practo_city_spec %>%
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
  group_by(month, country, city, speciality) %>%  
  summarise(book_practo = sum(book_practo)) %>%
  ungroup()

vn_city_spec <- vn_city_spec %>%
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
  group_by(month, country, city, speciality) %>%  
  summarise(vn = sum(vn)) %>%
  ungroup()

total_transactions_city[is.na(total_transactions_city)] <- 0
total_transactions_city <- total_transactions_city %>%
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
  summarise(book_practo = sum(book_practo), 
            book_qikwell = sum(book_qikwell), 
            qikwell = sum(qikwell), 
            vn = sum(vn), 
            widgets = sum(widgets)) %>%
  ungroup()

total_users_daily <- total_users_daily %>%
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
  group_by(date, country, city) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

total_users_monthly <- total_users_monthly %>%
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
  group_by(date, country, city) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

users_app_daily <- users_app_daily %>%
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
  group_by(date, country, city, app) %>%  
  summarise(users = sum(users)) %>%
  ungroup()
  
users_app_monthly <- users_app_monthly %>%
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
  group_by(date, country, city, app) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

users_channel_daily <- users_channel_daily %>%
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
  group_by(date, country, city, channel) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

users_channel_monthly <- users_channel_monthly %>%
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
  group_by(date, country, city, channel) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

users_device_daily <- users_device_daily %>%
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
  group_by(date, country, city, device) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

users_device_monthly <- users_device_monthly %>%
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
  group_by(date, country, city, device) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

users_speciality_city <- users_speciality_city %>%
  mutate(city = ifelse((city == 'Mumbai' | 
                          city == 'Navi Mumbai'| 
                          city == 'Thane' |
                          city == 'Navi-mumbai'), 'Mumbai_MMR', 
                       ifelse((city == 'New Delhi' |
                                 city == 'New-delhi' |
                                 city == 'Delhi' | 
                                 city == 'Ghaziabad' | 
                                 city == 'Faridabad' | 
                                 city == 'Noida' |
                                 city == 'Greater Noida' |
                                 city == 'Greater-noida' |
                                 city == 'Gurgaon'), 'Delhi_NCR', 
                              ifelse((city == 'Bangalore' | 
                                        city == 'Bangalore Rural' |
                                        city == 'Bangalore-rural'), 'Bengaluru', 
                                     ifelse(city == 'S?o Paulo', 'Sao Paulo', city))))) %>%
  group_by(date, country, city, speciality) %>%  
  summarise(users = sum(users)) %>%
  ungroup()

## 11. Saving the Workspace Image for Process.R ----
save.image("./Preprocess.RData")






