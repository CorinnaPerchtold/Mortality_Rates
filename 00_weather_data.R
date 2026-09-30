library(lubridate)
library(omnibus)
library(dplyr)
#library(tidyverse)
library(mice)
library(zoo)

#data can be freely downloaded from the datahub of GeoSphere
#data.hub.geosphere.at/dataset/klima-vs-1d

################ functions to find double stations ####
#does one station name have two station ID's?
extract_ids_of_stations<-function(df){
  ids_by_station<-split(df$station, df$Stationsname)
  ids_by_station<-lapply(ids_by_station, unique)
  ids_by_station<-ids_by_station[sapply(ids_by_station, function(ids) length(ids)>1)]
  return(ids_by_station)
}

#are there more than one substation per station?
check_multiple_substations <- function(data) {
  
  grouped_data <- group_by(data, station, time)
  grouped_data<- summarize(grouped_data,n_substations = n_distinct(substation), .groups = 'drop')
  
  # Check for stations and dates with more than one substation
  result <- filter(grouped_data, n_substations > 1)
  
  return(result)
}

##################### read in metadata #######

austria<-read.csv(file="stations_metadaten.csv", header=TRUE, stringsAsFactors=FALSE)
austria.clean<-austria[is.na(austria$Verknüpfungsnummer),]

austria.clean<-austria.clean[,-c(2,7:17)]
colnames(austria.clean)<-c("station","Stationsname","Longitude","Latitude","Elevation")

##################### daily humidity_mean data in % #######
data00<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20000101_20001231.csv", header=TRUE)
data01<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20010101_20021231.csv", header=TRUE)
data02<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20030101_20041231.csv", header=TRUE)
data03<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20050101_20061231.csv", header=TRUE)
data04<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20070101_20081231.csv", header=TRUE)
data05<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20090101_20101231.csv", header=TRUE)
data06<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20110101_20121231.csv", header=TRUE)
data07<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20130101_20141231.csv", header=TRUE)
data08<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20150101_20161231.csv", header=TRUE)
data09<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20170101_20181231.csv", header=TRUE)
data10<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20190101_20201231.csv", header=TRUE)
data11<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20210101_20231231.csv", header=TRUE)


df_list_22_humidity<-list( data00, data01, data02, data03, data04,
                           data05, data06,data07, data08, data09,data10, data11)

data_22_humidity<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list_22_humidity)
data_humidity<-inner_join(austria.clean,data_22_humidity, by="station")
data_humidity$time<-as.Date(data_humidity$time)
data_humidity$Year<-year(data_humidity$time)
data_humidity$Month<-month(data_humidity$time)

#find double stations: 
test<-extract_ids_of_stations(data_humidity)

#remove them
data_humidity<-data_humidity[  !(data_humidity$station=="20021") ,]

#how many substations does one station have:
test<-check_multiple_substations(data_humidity)

#remove double substation
data_humidity<-data_humidity %>% group_by(Stationsname,time) %>% slice(1)
data_humidity<-data_humidity[,-c(8)]


##################### daily temperature_mean and _min data in ° ############################
data00<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20000101T0000_20001231T0000.csv", header=TRUE)
data01<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20010101T0000_20011231T0000.csv", header=TRUE)
data02<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20020101T0000_20021231T0000.csv", header=TRUE)
data03<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20030101T0000_20031231T0000.csv", header=TRUE)
data04<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20040101T0000_20041231T0000.csv", header=TRUE)
data05<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20050101T0000_20051231T0000.csv", header=TRUE)
data06<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20060101T0000_20061231T0000.csv", header=TRUE)
data07<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20070101T0000_20071231T0000.csv", header=TRUE)
data08<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20080101T0000_20081231T0000.csv", header=TRUE)
data09<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20090101T0000_20091231T0000.csv", header=TRUE)
data10<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20100101T0000_20101231T0000.csv", header=TRUE)
data11<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20110101T0000_20111231T0000.csv", header=TRUE)
data12<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20120101T0000_20121231T0000.csv", header=TRUE)
data13<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20130101T0000_20131231T0000.csv", header=TRUE)
data14<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20140101T0000_20141231T0000.csv", header=TRUE)
data15<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20150101T0000_20151231T0000.csv", header=TRUE)
data16<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20160101T0000_20161231T0000.csv", header=TRUE)
data17<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20170101T0000_20171231T0000.csv", header=TRUE)
data18<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20180101T0000_20181231T0000.csv", header=TRUE)
data19<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20190101T0000_20191231T0000.csv", header=TRUE)
data20<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20200101T0000_20201231T0000.csv", header=TRUE)
data21<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20210101T0000_20211231T0000.csv", header=TRUE)
data22<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20220101T0000_20221231T0000.csv", header=TRUE)
data23<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20230101T0000_20231231T0000.csv", header=TRUE)


df_list_22_temp<-list( data00, data01, data02, data03, data04,
                       data05, data06,data07, data08, data09,data10, data11, data12, data13, data14,data15, data16,
                       data17,data18, data19, data20, data21, data22, data23)

#merge all data frames in list
data_22_temp<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list_22_temp)
data_temp<-inner_join(austria.clean, data_22_temp, by="station")
data_temp$time<-as.Date(data_temp$time)
data_temp$Year<-year(data_temp$time)
data_temp$Month<-month(data_temp$time)

#find double stations: 
test<-extract_ids_of_stations(data_temp)

#remove them
data_temp<-data_temp[(!data_temp$station=="8807"& !data_temp$station=="12016" &
                        !data_temp$station=="17003" &  !data_temp$station=="19711" &   !data_temp$station=="9111" & !data_temp$station=="14311" &
                        !data_temp$station=="11506" & !data_temp$station=="11707" & !data_temp$station=="20021" & 
                        !data_temp$station=="11306"),]

#how many substations does one station have:
test<-check_multiple_substations(data_temp)

data_temp<-data_temp %>% group_by(Stationsname,time) %>% slice(1)
data_temp<-data_temp[,-c(9)]

entries_per_station_year<-data_temp %>% group_by(Stationsname,time) %>% summarise(count=n())

##################### daily temperature_max ############################
data00<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20000101T0000_20001231T0000.csv", header=TRUE)
data01<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20010101T0000_20011231T0000.csv", header=TRUE)
data02<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20020101T0000_20021231T0000.csv", header=TRUE)
data03<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20030101T0000_20031231T0000.csv", header=TRUE)
data04<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20040101T0000_20041231T0000.csv", header=TRUE)
data05<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20050101T0000_20051231T0000.csv", header=TRUE)
data06<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20060101T0000_20061231T0000.csv", header=TRUE)
data07<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20070101T0000_20071231T0000.csv", header=TRUE)
data08<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20080101T0000_20081231T0000.csv", header=TRUE)
data09<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20090101T0000_20091231T0000.csv", header=TRUE)
data10<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20100101T0000_20101231T0000.csv", header=TRUE)
data11<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20110101T0000_20111231T0000.csv", header=TRUE)
data12<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20120101T0000_20121231T0000.csv", header=TRUE)
data13<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20130101T0000_20131231T0000.csv", header=TRUE)
data14<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20140101T0000_20141231T0000.csv", header=TRUE)
data15<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20150101T0000_20151231T0000.csv", header=TRUE)
data16<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20160101T0000_20161231T0000.csv", header=TRUE)
data17<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20170101T0000_20171231T0000.csv", header=TRUE)
data18<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20180101T0000_20181231T0000.csv", header=TRUE)
data19<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20190101T0000_20191231T0000.csv", header=TRUE)
data20<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20200101T0000_20201231T0000.csv", header=TRUE)
data21<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20210101T0000_20211231T0000.csv", header=TRUE)
data22<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20220101T0000_20221231T0000.csv", header=TRUE)
data23<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20230101T0000_20231231T0000.csv", header=TRUE)


df_list_22_temp_max<-list( data00, data01, data02, data03, data04,
                           data05, data06,data07, data08, data09,data10, data11, data12, data13, data14,data15, data16,
                           data17,data18, data19, data20, data21, data22,data23)

#merge all data frames in list
data_22_temp_max<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list_22_temp_max)
data_temp_max<-inner_join(austria.clean, data_22_temp_max, by="station")

data_temp_max$time<-as.Date(data_temp_max$time)
data_temp_max$Year<-year(data_temp_max$time)
data_temp_max$Month<-month(data_temp_max$time)

#find double stations: 
test<-extract_ids_of_stations(data_temp_max)

#remove them
data_temp_max<-data_temp_max[(!data_temp_max$station=="8807" & !data_temp_max$station=="12016" &  
                                !data_temp_max$station=="17003" & !data_temp_max$station=="19711" &
                                !data_temp_max$station=="9111" & !data_temp_max$station=="14311" & !data_temp_max$station=="11506" &
                                !data_temp_max$station=="11707" & !data_temp_max$station=="20021" & !data_temp_max$station=="11306"),]

#how many substations does one station have:
test<-check_multiple_substations(data_temp_max)

data_temp_max<-data_temp_max %>% group_by(Stationsname,time) %>% slice(1)
data_temp_max<-data_temp_max[,-c(8)]

entries_per_station_year<-data_temp_max %>% group_by(Stationsname,Year) %>% summarise(count=n())

data_temp_all<-merge(data_temp, data_temp_max, by=c("station", "Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))
data_temp_humidity<-merge(data_temp_all, data_humidity, by=c("station", "Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))


################ daily rain sum data #############
data00<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20000101T0000_20001231T0000.csv", header=TRUE)
data01<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20010101T0000_20011231T0000.csv", header=TRUE)
data02<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20020101T0000_20021231T0000.csv", header=TRUE)
data03<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20030101T0000_20031231T0000.csv", header=TRUE)
data04<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20040101T0000_20041231T0000.csv", header=TRUE)
data05<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20050101T0000_20051231T0000.csv", header=TRUE)
data06<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20060101T0000_20061231T0000.csv", header=TRUE)
data07<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20070101T0000_20071231T0000.csv", header=TRUE)
data08<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20080101T0000_20081231T0000.csv", header=TRUE)
data09<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20090101T0000_20091231T0000.csv", header=TRUE)
data10<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20100101T0000_20101231T0000.csv", header=TRUE)
data11<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20110101T0000_20111231T0000.csv", header=TRUE)
data12<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20120101T0000_20121231T0000.csv", header=TRUE)
data13<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20130101T0000_20131231T0000.csv", header=TRUE)
data14<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20140101T0000_20141231T0000.csv", header=TRUE)
data15<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20150101T0000_20151231T0000.csv", header=TRUE)
data16<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20160101T0000_20161231T0000.csv", header=TRUE)
data17<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20170101T0000_20171231T0000.csv", header=TRUE)
data18<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20180101T0000_20181231T0000.csv", header=TRUE)
data19<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20190101T0000_20191231T0000.csv", header=TRUE)
data20<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20200101T0000_20201231T0000.csv", header=TRUE)
data21<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20210101T0000_20211231T0000.csv", header=TRUE)
data22<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20220101T0000_20221231T0000.csv", header=TRUE)
data23<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20230101T0000_20231231T0000.csv", header=TRUE)


df_list_22_rain<-list( data00, data01, data02, data03, data04,
                       data05, data06,data07, data08, data09,data10, data11, data12, data13, data14,data15, data16,
                       data17,data18, data19, data20, data21, data22,data23)

#merge all data frames in list
data_22_rain<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list_22_rain)
data_rain<-inner_join(austria.clean,data_22_rain, by="station")
data_rain$time<-as.Date(data_rain$time)
data_rain$Year<-year(data_rain$time)
data_rain$Month<-month(data_rain$time)

#declare negative threshold as zero rain
data_rain["rr"][data_rain["rr"]==-0.1]<-0 

#find double stations: 
test<-extract_ids_of_stations(data_rain)

data_rain<-data_rain[(!data_rain$station=="8807" & !data_rain$station=="12016" &  !data_rain$station=="11113" & 
                        !data_rain$station=="17003" &  !data_rain$station=="17006" & 
                        !data_rain$station=="9111" & !data_rain$station=="14311" & !data_rain$station=="11506" &
                        !data_rain$station=="11707" & !data_rain$station=="20021" & !data_rain$station=="11306"),]

#how many substations does one station have:
test<-check_multiple_substations(data_rain)

data_rain<-data_rain %>% group_by(Stationsname,time) %>% slice(1)
data_rain<-data_rain[,-c(8)]

entries_per_station_year<-data_rain %>% group_by(Stationsname,time) %>% summarise(count=n())

############ merge to final data frame ######
weather_data_all<-merge( data_temp_humidity,data_rain, by=c("station","Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))

colnames(weather_data_all)<-c("ID", "Station","Longitude", "Latitude", "Elevation",  "Year", "Time","Month","Temp_min", "Temp_mean",  "Temp_max","Humidity_mean", "Precip_sum")

#remove 2000.01.01-2000.01.02 since we start with calender week 1: 2000.01.03-2000.01.09
weather_data_all<-weather_data_all[-which(weather_data_all$Time=="2000-01-01"),]
weather_data_all<-weather_data_all[-which(weather_data_all$Time=="2000-01-02"),]

weather_data_all$Day<-wday(weather_data_all$Time, week_start=1)
weather_data_all$Week<-isoweek(weather_data_all$Time)
weather_data_all$Month<-month(weather_data_all$Time)

entries_per_station_year<-weather_data_all %>% group_by(Station,Year) %>% summarise(count=n())

save(weather_data_all,file="01_raw_data.R")