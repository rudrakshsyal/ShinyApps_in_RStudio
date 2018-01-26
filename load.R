setwd("~/Documents/MIS/load")

print("---------------------------Setting up the Working Directory--------------------------------------------------")

## 0. Load Libraries ----
library(readxl)
library(lubridate)
library(dplyr)
library(reshape)
library(rJava)
library(RJDBC)
library(RGoogleAnalytics)
library(RGA)
library(RCurl)
library(rjson)

print("---------------------------Loading the requisite Libraries--------------------------------------------------")

## 1. Setting up the Connection & Authorising the GA Account----
options(java.parameters = "-Xmx8192m")

## download Amazon Redshift JDBC driver ##
# download.file('http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar','RedshiftJDBC41-1.1.9.1009.jar')

# Connect to Amazon Redshift
driver <- JDBC("com.amazon.redshift.jdbc41.Driver", "RedshiftJDBC41-1.1.9.1009.jar", identifier.quote="`")
url <- "jdbc:redshift://localhost:5439/practowarehouse?user=rudraksh_syal&password=7zSghmdy864HdTy8"

conn <- dbConnect(driver, url)

## One time run for TOKEN ##
# client.id <- "446530658631-tgglr9ngoba0nr0vgruefci35s7dd6r4.apps.googleusercontent.com"
# client.secret <- "lUjGAVkjNOCwRIu2wIOXDD8t"
# token <- Auth(client.id,client.secret)
# save(token,file="./token_file")

load("./token_file")
ValidateToken(token)

print("---------------------------Connecting to the Database & Reading the GA Credentials---------------------------")

## 2. Reading ONE Static file ----
specs <- read.csv("./static files/specs.csv", stringsAsFactors = F)

print("---------------------------Reading a static local csv file--------------------------------------------------")

## 3a. Reading GA Automated Daily Files -----

# Date <- as.Date("2017-10-01")
Date <- Sys.Date()

duration_start <- "2016-10-01" 

durM <- as.character(seq(as.Date(duration_start), by = "month", length.out = as.numeric(floor((Date - as.Date(duration_start))/30))))
durD <- as.character(seq(as.Date(paste0(year(Date), "-", ifelse(nchar(month(Date-365)) == 1, paste0("0",month(Date-365)), month(Date-365)), "-01")), by = "month", length.out = 1))
durYTD <- as.character(seq(as.Date(duration_start), by = "month", length.out = as.numeric(round((Date - as.Date(duration_start))/30)+1)))

mstart <- durM[1]
mend <- as.character(as.Date(durD[1]) - 1)
dstart <- durD[1]
dend <- as.character(Sys.Date())

print("---------------------------Specifying Date Ranges for GA Data pull-------------------------------------")

## 3b. Sourcing GA_Data_Pull.R for Channel, Device, App, Users (Total/YTD/DCH), AMP and City-Country Map ----

system.time(source("./GA_Data_Pull.R"))

print("---------------------------Sourcing another R-script for Total/YTD/DCH Users metric-------------------------")

## 3c. Sourcing listing_profile_segregated.R for Listing v/s Profile Page Users Analysis ----

system.time(source("../preprocess/listing vs profile page/listing_profile_segregated.R"))

print("---------------------------Sourcing another R-script for Profile/Listing Page Users metric-------------------")

## 3d. Sourcing speciality.R for Speciality Users ----

# users_speciality <- read.csv("../preprocess/users_speciality.csv", stringsAsFactors = F)[,-1]
# users_speciality_city <- read.csv("../preprocess/users_speciality_city.csv", stringsAsFactors = F)[,-1]
# users_speciality_global <- read.csv("../preprocess/users_speciality_global.csv", stringsAsFactors = F)[,-1]

print("---------------------------Sourcing another R-script for Speciality-wise Users metric-------------------")

system.time(source("./speciality.R"))

## 4. Querying Transactions Data ----

conn <- dbConnect(driver, url)

print("---------------------------Connecting to the Database again---------------------------")

## 4a. Transactions (Country & City split) ----
system.time(total_transactions_country <- dbGetQuery(conn, "SELECT 
                                         Book_Practo.Month,
                                         Book_Practo.Country,
                                         Book_Practo.Book_Practo, 
                                         Book_Qikwell.Book_Qikwell,
                                         Qikwell.Qikwell,
                                         VN_attempt.VN_attempt,
                                         VN_forward.VN_forward,
                                         VN_connect.VN_connect,
                                         Widgets.Widgets
                                         
                                         FROM
                                         (
                                         (SELECT 
                                         left(a.created_at,7) as Month,
                                         mcc.name as Country,
                                         count(*) as Book_Practo
                                         FROM fabric.appointments a
                                         INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                         INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                         INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                         INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                         INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                         INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                         where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                         and a.type IN ('ABS', 'abs')
                                         and a.source not in ('tab', 'widget')
                                         and c.name != 'Tawang'
                                         and a.created_at >= '2016-04-01'
                                         group by 1,2
                                         order by 1 desc) Book_Practo
                                         
                                         LEFT JOIN
                                         
                                         (select 
                                         left(a.created,7) as Month,
                                         'India' as Country,
                                         count(*) as Book_Qikwell
                                         from qikwell.appointments a
                                         join qikwell.departments d on d.id = a.department_id
                                         join qikwell.clinics c on c.id = d.clinic_id
                                         where a.aptSource in ('12')
                                         and a.created >= '2016-04-01'
                                         group by 1,2
                                         order by 1 desc) Book_Qikwell
                                         
                                         on (Book_Practo.Month = Book_Qikwell.Month 
                                         and Book_Practo.Country = Book_Qikwell.Country)
                                         
                                         LEFT JOIN
                                         
                                         (select 
                                         left(a.created,7) as Month,
                                         'India' as Country,
                                         count(*) as Qikwell
                                         from qikwell.appointments a
                                         where a.aptSource in ('0','5','8')
                                         and a.created >= '2016-04-01'
                                         group by 1,2
                                         order by 1 desc) Qikwell
                                         
                                         on (Book_Practo.Month = Qikwell.Month
                                         and Book_Practo.Country = Qikwell.Country)
                                         
                                         LEFT JOIN
                                         
                                         (select 
                                         left(vn.created_at,7) as Month,
                                         case 
                                         when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                                         when vpn.vn_zone_id in (9) then 'Singapore'
                                         when vpn.vn_zone_id in (10) then 'Philippines'
                                         when vpn.vn_zone_id in (11) then 'Indonesia'
                                         when vpn.vn_zone_id in (13,14) then 'Brazil'
                                         else NULL
                                         end as Country,
                                         count(*) as VN_attempt
                                         from fabric.vn_calls vn
                                         left join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                         left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                         left join fabric.practices_published p on p.id = vp.fabric_practice_id
                                         LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                                         LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                                         LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                                         LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                         WHERE vn.created_at >='2016-04-01'
                                         and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                                         --and mcc.name = 'India'
                                         GROUP by 1,2
                                         order by 1 desc) VN_attempt
                                         
                                         on (Book_Practo.Month = VN_attempt.Month
                                         and Book_Practo.Country = VN_attempt.Country)
                                         
                                         LEFT JOIN
                                         
                                         (select 
                                         left(vn.created_at,7) as Month,
                                         case 
                                         when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                                         when vpn.vn_zone_id in (9) then 'Singapore'
                                         when vpn.vn_zone_id in (10) then 'Philippines'
                                         when vpn.vn_zone_id in (11) then 'Indonesia'
                                         when vpn.vn_zone_id in (13,14) then 'Brazil'
                                         else NULL
                                         end as Country,
                                         count(*) as VN_forward
                                         from fabric.vn_calls vn
                                         join fabric.vn_call_forwardings vcf on vn.id = vcf.vn_call_id
                                         join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                         left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                         left join fabric.practices_published p on p.id = vp.fabric_practice_id
                                         LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                                         LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                                         LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                                         LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                         WHERE vn.created_at >='2016-04-01'
                                         and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                                         --and mcc.name = 'India'
                                         GROUP by 1,2
                                         order by 1 desc) VN_forward
                                         
                                         on (Book_Practo.Month = VN_forward.Month
                                         and Book_Practo.Country = VN_forward.Country)
                                         
                                         LEFT JOIN
                                         
                                         (select 
                                         left(vn.created_at,7) as Month,
                                         case 
                                         when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                                         when vpn.vn_zone_id in (9) then 'Singapore'
                                         when vpn.vn_zone_id in (10) then 'Philippines'
                                         when vpn.vn_zone_id in (11) then 'Indonesia'
                                         when vpn.vn_zone_id in (13,14) then 'Brazil'
                                         else NULL
                                         end as Country,
                                         count(*) as VN_connect
                                         from fabric.vn_calls vn
                                         join fabric.vn_call_forwardings vcf on vn.id = vcf.vn_call_id
                                         join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                         left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                         left join fabric.practices_published p on p.id = vp.fabric_practice_id
                                         LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                                         LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                                         LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                                         LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                         WHERE vn.created_at >='2016-04-01'
                                         and vcf.duration_in_seconds >= 10
                                         and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                                         --and mcc.name = 'India'
                                         GROUP by 1,2
                                         order by 1 desc) VN_connect
                                         
                                         on (Book_Practo.Month = VN_connect.Month
                                         and Book_Practo.Country = VN_connect.Country)
                                         
                                         LEFT JOIN
                                         
                                         (
                                         select 
                                         Month,
                                         Country,
                                         sum(Widgets) as Widgets
                                         from
                                         (
                                         (SELECT  
                                         left(a.created_at,7) as Month,
                                         mcc.name as Country,
                                         count(*) as Widgets
                                         FROM fabric.appointments a
                                         INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                         INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                         INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                         INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                         INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                         INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                         where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                         and a.type IN ('ABS', 'abs')
                                         and a.source in ('widget')
                                         AND a.created_at >= '2016-04-01'
                                         --and mcc.name = 'India'
                                         group by 1,2
                                         order by 1 desc)
                                         
                                         UNION  
                                         
                                         (select 
                                         left(a.created,7) as Month,
                                         'India' as Country,
                                         count(*) as Widgets
                                         from qikwell.appointments a
                                         join qikwell.departments d on d.id = a.department_id
                                         join qikwell.clinics c on c.id = d.clinic_id
                                         where a.aptSource in ('7')
                                         --and c.city in ('Bangalore', 'Chennai', 'Pune', 'Delhi', 'Noida', 'Gurgaon', 'Hyderabad', 'Thane', 'Mumbai')
                                         and a.created >= '2016-04-01'
                                         group by 1,2
                                         order by 1 desc)
                                         )
                                         group by 1,2
                                         order by 1 desc
                                         ) Widgets
                                         
                                         on (Book_Practo.Month = Widgets.Month
                                         and Book_Practo.Country = Widgets.Country)
                                         
                                         )
                                         order by 1 desc, 2"))

system.time(rotw_vn_country <- dbGetQuery(conn, "SELECT 
                              VN_attempt.Month,
                              VN_attempt.Country,
                              VN_attempt.VN_attempt,
                              VN_forward.VN_forward,
                              VN_connect.VN_connect
                              
                              FROM
                              (
                              (select 
                              left(vn.created_at,7) as Month,
                              case 
                              when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                              when vpn.vn_zone_id in (9) then 'Singapore'
                              when vpn.vn_zone_id in (10) then 'Philippines'
                              when vpn.vn_zone_id in (11) then 'Indonesia'
                              when vpn.vn_zone_id in (13,14) then 'Brazil'
                              else 'Rest of the World'
                              end as Country,
                              count(*) as VN_attempt
                              from fabric.vn_calls vn
                              left join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                              left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                              left join fabric.practices_published p on p.id = vp.fabric_practice_id
                              LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                              LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                              LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                              LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                              WHERE vn.created_at >='2016-04-01'
                              and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                              --and mcc.name = 'India'
                              GROUP by 1,2
                              order by 1 desc) VN_attempt
                              
                              LEFT JOIN
                              
                              (select 
                              left(vn.created_at,7) as Month,
                              case 
                              when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                              when vpn.vn_zone_id in (9) then 'Singapore'
                              when vpn.vn_zone_id in (10) then 'Philippines'
                              when vpn.vn_zone_id in (11) then 'Indonesia'
                              when vpn.vn_zone_id in (13,14) then 'Brazil'
                              else 'Rest of the World'
                              end as Country,
                              count(*) as VN_forward
                              from fabric.vn_calls vn
                              join fabric.vn_call_forwardings vcf on vn.id = vcf.vn_call_id
                              join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                              left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                              left join fabric.practices_published p on p.id = vp.fabric_practice_id
                              LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                              LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                              LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                              LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                              WHERE vn.created_at >='2016-04-01'
                              and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                              --and mcc.name = 'India'
                              GROUP by 1,2
                              order by 1 desc) VN_forward
                              
                              on (VN_attempt.Month = VN_forward.Month
                              and VN_attempt.Country = VN_forward.Country)
                              
                              LEFT JOIN
                              
                              (select 
                              left(vn.created_at,7) as Month,
                              case 
                              when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                              when vpn.vn_zone_id in (9) then 'Singapore'
                              when vpn.vn_zone_id in (10) then 'Philippines'
                              when vpn.vn_zone_id in (11) then 'Indonesia'
                              when vpn.vn_zone_id in (13,14) then 'Brazil'
                              else 'Rest of the World'
                              end as Country,
                              count(*) as VN_connect
                              from fabric.vn_calls vn
                              join fabric.vn_call_forwardings vcf on vn.id = vcf.vn_call_id
                              join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                              left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                              left join fabric.practices_published p on p.id = vp.fabric_practice_id
                              LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                              LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                              LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                              LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                              WHERE vn.created_at >='2016-04-01'
                              and vcf.duration_in_seconds >= 10
                              and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                              --and mcc.name = 'India'
                              GROUP by 1,2
                              order by 1 desc) VN_connect
                              
                              on (VN_attempt.Month = VN_connect.Month
                              and VN_attempt.Country = VN_connect.Country)
                              )
                              order by 1 desc,2"))
  
system.time(total_transactions_city <- dbGetQuery(conn, "select 
                                      Book_Practo.Month,
                                      Book_Practo.Country,
                                      Book_Practo.City,
                                      Book_Practo.Book_Practo, 
                                      Book_Qikwell.Book_Qikwell,
                                      Qikwell.Qikwell,
                                      VN.VN,
                                      Widgets.Widgets
                                      
                                      FROM
                                      (
                                      (SELECT 
                                      left(a.created_at,7) as Month,
                                      mcc.name as Country,
                                      c.name as City,
                                      count(*) as Book_Practo
                                      FROM fabric.appointments a
                                      INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                      INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                      INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                      INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                      INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                      INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                      where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                      and a.type IN ('ABS', 'abs')
                                      and a.source not in ('tab', 'widget')
                                      and c.name != 'Tawang'
                                      and c.name in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata','Singapore','Jakarta','Quezon City','Pasay','Makati','Paranaque','Cebu City','Cavite','S?o Paulo','Las Pinas','Mandaluyong','Pasig')
                                      and a.created_at >= '2016-04-01'
                                      group by 1,2,3
                                      order by 1 desc) Book_Practo
                                      
                                      LEFT JOIN
                                      
                                      (select 
                                      left(a.created,7) as Month,
                                      'India' as Country,
                                      c.city as City,
                                      count(*) as Book_Qikwell
                                      from qikwell.appointments a
                                      join qikwell.departments d on d.id = a.department_id
                                      join qikwell.clinics c on c.id = d.clinic_id
                                      where a.aptSource in ('12')
                                      and c.city in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata')
                                      and a.created >= '2016-04-01'
                                      group by 1,2,3
                                      order by 1 desc) Book_Qikwell
                                      
                                      on (Book_Practo.Month = Book_Qikwell.Month 
                                      and Book_Practo.Country = Book_Qikwell.Country
                                      and Book_Practo.City = Book_Qikwell.City)
                                      
                                      LEFT JOIN
                                      
                                      (select 
                                      left(a.created,7) as Month,
                                      'India' as Country,
                                      c.city as City,
                                      count(*) as Qikwell
                                      from qikwell.appointments a
                                      join qikwell.departments d on d.id = a.department_id
                                      join qikwell.clinics c on c.id = d.clinic_id
                                      where a.aptSource in ('0','5','8')
                                      and c.city in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata')
                                      and a.created >= '2016-04-01'
                                      group by 1,2,3
                                      order by 1 desc) Qikwell
                                      
                                      on (Book_Practo.Month = Qikwell.Month
                                      and Book_Practo.Country = Qikwell.Country
                                      and Book_Practo.City = Qikwell.City)
                                      
                                      LEFT JOIN
                                      
                                      (select 
                                      left(vn.created_at,7) as Month,
                                      case 
                                      when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                                      when vpn.vn_zone_id in (9) then 'Singapore'
                                      when vpn.vn_zone_id in (10) then 'Philippines'
                                      when vpn.vn_zone_id in (11) then 'Indonesia'
                                      when vpn.vn_zone_id in (13,14) then 'Brazil'
                                      else NULL
                                      end as Country,
                                      c.name as City,
                                      count(*) as VN
                                      from fabric.vn_calls vn
                                      left join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                      left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                      left join fabric.practices_published p on p.id = vp.fabric_practice_id
                                      LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                                      LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                                      LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                                      LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                      WHERE vn.created_at >='2016-04-01'
                                      and c.name in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata','Singapore','Jakarta','Quezon City','Pasay','Makati','Paranaque','Cebu City','Cavite','S?o Paulo','Las Pinas','Mandaluyong','Pasig')
                                      GROUP by 1,2,3
                                      order by 1 desc) VN
                                      
                                      on (Book_Practo.Month = VN.Month
                                      and Book_Practo.Country = VN.Country
                                      and Book_Practo.City = VN.City)
                                      
                                      LEFT JOIN
                                      
                                      (
                                      select 
                                      Month,
                                      Country,
                                      City,
                                      sum(Widgets) as Widgets
                                      from
                                      (
                                      (SELECT  
                                      left(a.created_at,7) as Month,
                                      mcc.name as Country,
                                      c.name as City,
                                      count(*) as Widgets
                                      FROM fabric.appointments a
                                      INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                      INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                      INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                      INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                      INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                      INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                      where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                      and a.type IN ('ABS', 'abs')
                                      and a.source in ('widget')
                                      AND a.created_at >= '2016-04-01'
                                      and c.name in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata','Singapore','Jakarta','Quezon City','Pasay','Makati','Paranaque','Cebu City','Cavite','S?o Paulo','Las Pinas','Mandaluyong','Pasig'
                                      )
                                      group by 1,2,3
                                      order by 1 desc)
                                      
                                      UNION  
                                      
                                      (select 
                                      left(a.created,7) as Month,
                                      'India' as Country,
                                      c.city as City,
                                      count(*) as Widgets
                                      from qikwell.appointments a
                                      join qikwell.departments d on d.id = a.department_id
                                      join qikwell.clinics c on c.id = d.clinic_id
                                      where a.aptSource in ('7')
                                      and c.city in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata')
                                      and a.created >= '2016-04-01'
                                      group by 1,2,3
                                      order by 1 desc)
                                      )
                                      group by 1,2,3
                                      order by 1 desc
                                      ) Widgets
                                      
                                      on (Book_Practo.Month = Widgets.Month
                                      and Book_Practo.Country = Widgets.Country
                                      and Book_Practo.City = Widgets.City)
                                      
                                      )
                                      order by 1 desc, 4 desc"))

## 4b. Transactions (Spec split) ----
system.time(book_practo_spec <- dbGetQuery(conn, "select
                                           Month,
                                           Speciality,
                                           sum(Book_Practo) as Book_Practo
                                           
                                           FROM
                                           
                                           (select 
                                           parent.Month as Month,
                                           parent.PDID as PDID,
                                           parent.Speciality as Speciality,
                                           (max.Max_Flag - parent.Approved_Flag) as Diff_Flag,
                                           count(parent.Book_Practo) as Book_Practo
                                           
                                           FROM
                                           (
                                           (SELECT distinct
                                           pd.id as PDID,
                                           left(a.created_at,7) as Month,
                                           mds.speciality as Speciality,
                                           ds.approved as Approved_Flag,
                                           a.id as Book_Practo
                                           FROM fabric.appointments a
                                           INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                           INNER JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                           INNER JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                           INNER JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                           INNER JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id
                                           INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                           INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                           INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                           INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                           INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                           where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                           and a.type IN ('ABS', 'abs')
                                           and a.source not in ('tab', 'widget')
                                           and c.name != 'Tawang'
                                           and mcc.name = 'India'
                                           and a.created_at >= '2016-04-01'
                                           --group by 1,2,3,4
                                           order by 1 desc, 2) parent
                                           
                                           LEFT JOIN
                                           
                                           (SELECT distinct
                                           PDID,
                                           max(Approved_Flag) as Max_Flag
                                           FROM
                                           (SELECT
                                           pd.id as PDID,
                                           ds.approved as Approved_Flag
                                           FROM fabric.practice_doctors_published pd
                                           INNER JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                           INNER JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                           INNER JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                           INNER JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id)
                                           group by 1) max
                                           
                                           ON parent.PDID = max.PDID)
                                           group by 1,2,3,4)
                                           where Diff_Flag = 0
                                           group by 1,2
                                           order by 1 desc, 3 desc
                                           limit 50000"))
  
system.time(vn_spec <- dbGetQuery(conn, "select
                                  Month,
                                  Speciality,
                                  sum(VN) as VN
                                  
                                  FROM
                                  
                                  (select 
                                  parent.Month as Month,
                                  parent.PDID as PDID,
                                  parent.Speciality as Speciality,
                                  (max.Max_Flag - parent.Approved_Flag) as Diff_Flag,
                                  count(parent.VN) as VN
                                  
                                  FROM
                                  (
                                  (SELECT
                                  pd.id as PDID,
                                  left(vn.created_at,7) as Month,
                                  mds.speciality as Speciality,
                                  ds.approved as Approved_Flag,
                                  vn.id as VN
                                  from fabric.vn_calls vn
                                  JOIN fabric.vn_call_forwardings vcf on vn.id = vcf.vn_call_id
                                  JOIN fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                  LEFT JOIN fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                  LEFT JOIN fabric.practice_doctors_published pd ON vp.practice_doctor_id = pd.id
                                  LEFT JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                  LEFT JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                  LEFT JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                  LEFT JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id 
                                  LEFT JOIN fabric.practices_published p on p.id = vp.fabric_practice_id
                                  LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                                  LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                                  LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                                  LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                  WHERE vn.created_at >='2016-04-01'
                                  AND vcf.duration_in_seconds >= 10
                                  AND vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) 
                                  AND vn.created_at >='2016-04-01'
                                  AND mds.speciality is NOT NULL
                                  order by 1 desc, 2) parent
                                  
                                  LEFT JOIN
                                  
                                  (SELECT distinct
                                  PDID,
                                  max(Approved_Flag) as Max_Flag
                                  FROM
                                  (SELECT
                                  pd.id as PDID,
                                  ds.approved as Approved_Flag
                                  FROM fabric.practice_doctors_published pd
                                  left JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                  left JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                  left JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                  left JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id)
                                  group by 1) max
                                  
                                  ON parent.PDID = max.PDID)
                                  group by 1,2,3,4)
                                  where Diff_Flag = 0
                                  group by 1,2
                                  order by 1 desc, 3 desc
                                  limit 50000"))

## 4c. Transactions (City & Spec split) ----
system.time(book_practo_city_spec <- dbGetQuery(conn, "select
                                                Month,
                                                Country,
                                                City,
                                                Speciality,
                                                sum(Book_Practo) as Book_Practo
                                                
                                                FROM
                                                
                                                (select 
                                                parent.Month as Month,
                                                parent.PDID as PDID,
                                                parent.Country,
                                                parent.City,
                                                parent.Speciality as Speciality,
                                                (max.Max_Flag - parent.Approved_Flag) as Diff_Flag,
                                                count(parent.Book_Practo) as Book_Practo
                                                
                                                FROM
                                                (
                                                (SELECT distinct
                                                pd.id as PDID,
                                                left(a.created_at,7) as Month,
                                                mcc.name as Country,
                                                c.name as City,
                                                mds.speciality as Speciality,
                                                ds.approved as Approved_Flag,
                                                a.id as Book_Practo
                                                FROM fabric.appointments a
                                                INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                                INNER JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                                INNER JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                                INNER JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                                INNER JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id
                                                INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                                INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                                INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                                INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                                INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                                where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                                and a.type IN ('ABS', 'abs')
                                                and a.source not in ('tab', 'widget')
                                                and c.name != 'Tawang'
                                                and c.name in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata','Singapore','Jakarta','Quezon City','Pasay','Makati','Paranaque','Cebu City','Cavite','S?o Paulo','Las Pinas','Mandaluyong','Pasig')
                                                and a.created_at >= '2016-04-01'
                                                order by 1 desc, 2) parent
                                                
                                                LEFT JOIN
                                                
                                                (SELECT distinct
                                                PDID,
                                                max(Approved_Flag) as Max_Flag
                                                FROM
                                                (SELECT
                                                pd.id as PDID,
                                                ds.approved as Approved_Flag
                                                FROM fabric.practice_doctors_published pd
                                                INNER JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                                INNER JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                                INNER JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                                INNER JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id)
                                                group by 1) max
                                                
                                                ON parent.PDID = max.PDID)
                                                group by 1,2,3,4,5,6)
                                                where Diff_Flag = 0
                                                group by 1,2,3,4
                                                order by 1 desc, 5 desc
                                                limit 500000"))

system.time(vn_city_spec <- dbGetQuery(conn, "select
                                       Month,
                                       Country,
                                       City,
                                       Speciality,
                                       sum(VN) as VN
                                       
                                       FROM
                                       
                                       (select 
                                       parent.Month as Month,
                                       parent.PDID as PDID,
                                       parent.Country as Country,
                                       parent.City as City,
                                       parent.Speciality as Speciality,
                                       (max.Max_Flag - parent.Approved_Flag) as Diff_Flag,
                                       count(parent.VN) as VN
                                       
                                       FROM
                                       (
                                       (SELECT distinct
                                       pd.id as PDID,
                                       left(vn.created_at,7) as Month,
                                       case 
                                       when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                                       when vpn.vn_zone_id in (9) then 'Singapore'
                                       when vpn.vn_zone_id in (10) then 'Philippines'
                                       when vpn.vn_zone_id in (11) then 'Indonesia'
                                       when vpn.vn_zone_id in (13,14) then 'Brazil'
                                       else NULL
                                       end as Country,
                                       c.name as City,
                                       mds.speciality as Speciality,
                                       ds.approved as Approved_Flag,
                                       vn.id as VN
                                       FROM fabric.vn_calls vn
                                       left join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                       left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                       LEFT JOIN fabric.practice_doctors_published pd ON vp.practice_doctor_id = pd.id
                                       LEFT JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                       LEFT JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                       LEFT JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                       LEFT JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id 
                                       left join fabric.practices_published p on p.id = vp.fabric_practice_id
                                       left JOIN fabric.master_localities l ON p.locality_id = l.id
                                       left JOIN fabric.master_cities c ON l.city_id = c.id
                                       left JOIN fabric.master_states ms ON c.state_id = ms.id
                                       left JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                       WHERE vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) 
                                       AND vn.created_at >='2016-04-01'
                                       and c.name in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata','Singapore','Jakarta','Quezon City','Pasay','Makati','Paranaque','Cebu City','Cavite','S?o Paulo','Las Pinas','Mandaluyong','Pasig')
                                       --and mcc.name = 'India'
                                       order by 1 desc, 2) parent
                                       
                                       LEFT JOIN
                                       
                                       (SELECT distinct
                                       PDID,
                                       max(Approved_Flag) as Max_Flag
                                       FROM
                                       (SELECT
                                       pd.id as PDID,
                                       ds.approved as Approved_Flag
                                       FROM fabric.practice_doctors_published pd
                                       left JOIN fabric.doctors_published dd ON dd.id = pd.doctor_id 
                                       left JOIN fabric.doctor_specializations_published ds ON dd.id = ds.doctor_id
                                       left JOIN fabric.master_doctor_subspecialities mdss ON ds.subspecialization_id = mdss.id
                                       left JOIN fabric.master_doctor_specialities mds ON  mds.id = mdss.speciality_id)
                                       group by 1) max
                                       
                                       ON parent.PDID = max.PDID)
                                       group by 1,2,3,4,5,6)
                                       where Diff_Flag = 0
                                       group by 1,2,3,4
                                       order by 1 desc, 5 desc
                                       limit 50000"))

## 4d. Source-wise - Book_Practo (Country split) ----
system.time(book_practo_source <- dbGetQuery(conn, "SELECT 
 left(a.created_at,7) as Month,
 mcc.name as Country,
a.source as Source,
  count(*) as Book_Practo
 FROM fabric.appointments a
INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
INNER JOIN fabric.master_localities l ON p.locality_id = l.id
INNER JOIN fabric.master_cities c ON l.city_id = c.id
INNER JOIN fabric.master_states ms ON c.state_id = ms.id
INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
where a.STATUS IN ('CONFIRMED', 'CANCELLED')
and a.type IN ('ABS', 'abs')
and a.source not in ('tab', 'widget')
and c.name != 'Tawang'
and a.created_at >= '2016-04-01'
group by 1,2,3
order by 1 desc"))

## 4e. Cancellations - Book_Practo (Country & City split) ----
system.time(cancellations_country <- dbGetQuery(conn, "SELECT 
left(a.created_at,7) as Month,
mcc.name as Country,
(COUNT(DISTINCT CASE WHEN a.status = 'CANCELLED' THEN a.id END)*100)/COUNT(DISTINCT a.id) as Cancellation_Rate
FROM fabric.appointments a
INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
INNER JOIN fabric.master_localities l ON p.locality_id = l.id
INNER JOIN fabric.master_cities c ON l.city_id = c.id
INNER JOIN fabric.master_states ms ON c.state_id = ms.id
INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
where a.STATUS IN ('CONFIRMED', 'CANCELLED')
and a.type IN ('ABS', 'abs')
and a.source not in ('tab', 'widget')
and c.name != 'Tawang'
and a.created_at >= '2016-04-01'
group by 1,2
order by 1 desc, 3 desc"))
  
system.time(cancellations_city <- dbGetQuery(conn, "SELECT 
left(a.created_at,7) as Month,
mcc.name as Country,
c.name as City,
(COUNT(DISTINCT CASE WHEN a.status = 'CANCELLED' THEN a.id END)*100)/COUNT(DISTINCT a.id) Cancellation_Rate
FROM fabric.appointments a
INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
INNER JOIN fabric.master_localities l ON p.locality_id = l.id
INNER JOIN fabric.master_cities c ON l.city_id = c.id
INNER JOIN fabric.master_states ms ON c.state_id = ms.id
INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
where a.STATUS IN ('CONFIRMED', 'CANCELLED')
and a.type IN ('ABS', 'abs')
and a.source not in ('tab', 'widget')
and c.name != 'Tawang'
and c.name in ('Bangalore','Mumbai','Thane','Navi Mumbai','Delhi','Gurgaon','Faridabad','Ghaziabad','Noida','Pune','Hyderabad','Chennai','Kolkata','Singapore','Jakarta','Quezon City','Pasay','Makati','Paranaque','Cebu City','Cavite','S?o Paulo','Las Pinas','Mandaluyong','Pasig')
and a.created_at >= '2016-04-01'
group by 1,2,3
order by 1 desc, 4 desc"))

## 4f. Daily Run Rate - Transactions ----
system.time(transactions_run_rate <- dbGetQuery(conn, "select 
                                    Book_Practo.Date,
                                    Book_Practo.Country,
                                    Book_Practo.Book_Practo, 
                                    Book_Qikwell.Book_Qikwell,
                                    Qikwell.Qikwell,
                                    VN.VN,
                                    Widgets.Widgets
                                    
                                    FROM
                                    (
                                    (SELECT 
                                    left(a.created_at,10) as Date,
                                    mcc.name as Country,
                                    count(*) as Book_Practo
                                    FROM fabric.appointments a
                                    INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                    INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                    INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                    INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                    INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                    INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                    where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                    and a.type IN ('ABS', 'abs')
                                    and a.source not in ('tab', 'widget')
                                    and c.name != 'Tawang'
                                    and a.created_at >= '2017-05-01'
                                    group by 1,2
                                    order by 1 desc) Book_Practo
                                    
                                    LEFT JOIN
                                    
                                    (select 
                                    left(a.created,10) as Date,
                                    'India' as Country,
                                    count(*) as Book_Qikwell
                                    from qikwell.appointments a
                                    join qikwell.departments d on d.id = a.department_id
                                    join qikwell.clinics c on c.id = d.clinic_id
                                    where a.aptSource in ('12')
                                    and a.created >= '2017-05-01'
                                    group by 1,2
                                    order by 1 desc) Book_Qikwell
                                    
                                    on (Book_Practo.Date = Book_Qikwell.Date 
                                    and Book_Practo.Country = Book_Qikwell.Country)
                                    
                                    LEFT JOIN
                                    
                                    (select 
                                    left(a.created,10) as Date,
                                    'India' as Country,
                                    count(*) as Qikwell
                                    from qikwell.appointments a
                                    where a.aptSource in ('0','5','8')
                                    and a.created >= '2016-04-01'
                                    group by 1,2
                                    order by 1 desc) Qikwell
                                    
                                    on (Book_Practo.Date = Qikwell.Date
                                    and Book_Practo.Country = Qikwell.Country)
                                    
                                    LEFT JOIN
                                    
                                    (select 
                                    left(vn.created_at,10) as Date,
                                    case 
                                    when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
                                    when vpn.vn_zone_id in (9) then 'Singapore'
                                    when vpn.vn_zone_id in (10) then 'Philippines'
                                    when vpn.vn_zone_id in (11) then 'Indonesia'
                                    when vpn.vn_zone_id in (13,14) then 'Brazil'
                                    else NULL
                                    end as Country,
                                    count(*) as VN
                                    from fabric.vn_calls vn
                                    left join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
                                    left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
                                    left join fabric.practices_published p on p.id = vp.fabric_practice_id
                                    LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                                    LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                                    LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                                    LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                    WHERE vn.created_at >='2017-05-01'
                                    and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
                                    --and mcc.name = 'India'
                                    GROUP by 1,2
                                    order by 1 desc) VN
                                    
                                    on (Book_Practo.Date = VN.Date
                                    and Book_Practo.Country = VN.Country)
                                    
                                    LEFT JOIN
                                    
                                    (
                                    select 
                                    Date,
                                    Country,
                                    sum(Widgets) as Widgets
                                    from
                                    (
                                    (SELECT  
                                    left(a.created_at,10) as Date,
                                    mcc.name as Country,
                                    count(*) as Widgets
                                    FROM fabric.appointments a
                                    INNER JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                                    INNER JOIN fabric.practices_published p ON p.id = pd.practice_id
                                    INNER JOIN fabric.master_localities l ON p.locality_id = l.id
                                    INNER JOIN fabric.master_cities c ON l.city_id = c.id
                                    INNER JOIN fabric.master_states ms ON c.state_id = ms.id
                                    INNER JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                                    where a.STATUS IN ('CONFIRMED', 'CANCELLED')
                                    and a.type IN ('ABS', 'abs')
                                    and a.source in ('widget')
                                    AND a.created_at >= '2017-05-01'
                                    --and mcc.name = 'India'
                                    group by 1,2
                                    order by 1 desc)
                                    
                                    UNION  
                                    
                                    (select 
                                    left(a.created,10) as Date,
                                    'India' as Country,
                                    count(*) as Widgets
                                    from qikwell.appointments a
                                    join qikwell.departments d on d.id = a.department_id
                                    join qikwell.clinics c on c.id = d.clinic_id
                                    where a.aptSource in ('7')
                                    --and c.city in ('Bangalore', 'Chennai', 'Pune', 'Delhi', 'Noida', 'Gurgaon', 'Hyderabad', 'Thane', 'Mumbai')
                                    and a.created >= '2017-05-01'
                                    group by 1,2
                                    order by 1 desc)
                                    )
                                    group by 1,2
                                    order by 1 desc
                                    ) Widgets
                                    
                                    on (Book_Practo.Date = Widgets.Date
                                    and Book_Practo.Country = Widgets.Country)
                                    
                                    )
                                    order by 1 desc, 2"))

system.time(rotw_vn_dod <- dbGetQuery(conn, "select 
left(vn.created_at,10) as Date,
case 
 when vpn.vn_zone_id in (1,2,3,4,5,6,7,8,16,17,18,19,20) then 'India'
 when vpn.vn_zone_id in (9) then 'Singapore'
 when vpn.vn_zone_id in (10) then 'Philippines'
 when vpn.vn_zone_id in (11) then 'Indonesia'
 when vpn.vn_zone_id in (13,14) then 'Brazil'
 else 'Rest of the World'
 end as Country,
count(*) as VN
from fabric.vn_calls vn
left join fabric.vn_phone_numbers vpn on vn.vn_phone_number_id = vpn.id
left join fabric.vn_practices vp on vp.id = vn.vn_practice_id
left join fabric.practices_published p on p.id = vp.fabric_practice_id
LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
LEFT JOIN fabric.master_cities c ON l.city_id = c.id
LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
WHERE vn.created_at >='2017-05-01'
and vpn.vn_zone_id in (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) 
--and mcc.name = 'India'
GROUP by 1,2
order by 1 desc"))

## 4g. Unique Patient-Transactions for Conversion% (Unique/Unique) ----
system.time(df <- dbGetQuery(conn, "Select left(Date,7) as month, practice_id, Country, City, Mobile, count(id) as trans from
                ((SELECT p.id as practice_id, a.id as id, a.created_at as Date, mcc.name as Country, c.name as City, pt.mobile as Mobile
                 FROM fabric.appointments a
                 LEFT JOIN fabric.practice_doctors_published pd ON a.practiceDoctor_id = pd.id
                 LEFT JOIN fabric.practices_published p ON p.id = pd.practice_id
                 LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
                 LEFT JOIN fabric.master_cities c ON l.city_id = c.id
                 LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
                 LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
                 LEFT JOIN fabric.patients pt ON pt.id = a.patient_id
                 WHERE a.STATUS IN ('CONFIRMED', 'CANCELLED')
                 AND a.type IN ('ABS', 'abs')
                 AND a.source NOT IN ('tab', 'widget')
                 AND a.created_at >= '2016-10-01'
                 --AND mcc.name = 'India'
)
UNION
(SELECT p.id as practice_id, vn.id as id, vn.created_at as Date, mcc.name as Country, c.name as City, vn.caller_phone_number as Mobile
FROM fabric.vn_calls vn
INNER JOIN fabric.vn_call_forwardings vncf on vncf.vn_call_id = vn.id
LEFT JOIN fabric.vn_practices vp ON vp.id = vn.vn_practice_id
LEFT JOIN fabric.practices_published p ON p.id = vp.fabric_practice_id
LEFT JOIN fabric.master_localities l ON p.locality_id = l.id
LEFT JOIN fabric.master_cities c ON l.city_id = c.id
LEFT JOIN fabric.master_states ms ON c.state_id = ms.id
LEFT JOIN fabric.master_countries mcc ON ms.country_id = mcc.id
--WHERE vn.duration_in_seconds >= 10
--AND mcc.name = 'India'
WHERE vn.status = 'Finished'
AND vn.created_at >= '2016-10-01'))
GROUP BY 1,2,3,4,5"))

system.time(df$mobile <- as.integer(factor(df$mobile)))
df$month <- as.Date(paste0(df$month,"-01"), format = "%Y-%m-%d")

unique_patient_tranx <- df %>%
  group_by(month, country, practice_id) %>%  
  summarise(total_patients = length(unique(mobile))) %>%
  group_by(month, country) %>%
  summarise(total_patients = sum(total_patients)) %>%
  ungroup()

unique_patient_tranx_city <- df %>%
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
  group_by(month, country, practice_id, city) %>%  
  summarise(total_patients = length(unique(mobile))) %>%
  group_by(month, country, city) %>%
  summarise(total_patients = sum(total_patients)) %>%
  ungroup()

## 5. Sourcing Liquidity.R for Liquidity scores ----
system.time(source("../preprocess/liquidity/Liquidity.R"))

## 6. Sourcing GMV.R for GMV data ----
system.time(source("../preprocess/gmv/GMV.R"))

## 7. Sourcing Reach.R for Reach Monetization data ----
system.time(source("../preprocess/reach_monetization/Reach_v4.R"))

## 8. Saving the Workspace Image for Preprocess.R ----
setwd("~/Documents/MIS/load")
save.image("./load.RData")






