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

##################### daily pressure_mean data in hPA ############################
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
data23<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20230101_20231231.csv", header=TRUE)


df_list_22_pressure<-list( data00, data01, data02, data03, data04,
                           data05, data06,data07, data08, data09,data10, data11, data12, data13, data14,data15, data16,
                           data17,data18, data19, data20, data21, data22,data23)

data_22_pressure<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list_22_pressure)
data_pressure<-inner_join(austria.clean,data_22_pressure, by="station")
data_pressure$time<-as.Date(data_pressure$time)
data_pressure$Year<-year(data_pressure$time)
data_pressure$Month<-month(data_pressure$time)

#find double stations: 
test<-extract_ids_of_stations(data_pressure)
print(test) #remove 8807, 12016, 11113, 17003, 17006, 9111, 14311, 11506, 11707, 8302, 20021, 11306

#remove them
data_pressure<-data_pressure[!(data_pressure$station=="8807")| !(data_pressure$station=="12016") | 
                               !(data_pressure$station=="11113") | 
                               !(data_pressure$station=="17003") | !(data_pressure$station=="17006") |
                               !(data_pressure$station=="9111") | !(data_pressure$station=="14311") | !(data_pressure$station=="11506") |
                               !(data_pressure$station=="11707") | !(data_pressure$station=="8302") | 
                               !(data_pressure$station=="20021") | !(data_pressure$station=="11306"),]

#how many substations does one station have:
test<-check_multiple_substations(data_pressure)
data_pressure<-data_pressure %>% group_by(Stationsname,time) %>% slice(1)

#remove double substation
data_pressure<-data_pressure[,-c(8)]

data_pressure_humidity<-merge(data_pressure, data_humidity, by=c("station", "Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))

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
data_temp_pressure_humidity<-merge(data_temp_all, data_pressure_humidity, by=c("station", "Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))

################## min temperature during night in C° ##############
data00<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20000101_20051231.csv", header=TRUE)
data01<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20060101_20101231.csv", header=TRUE)
data02<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20110101_20151231.csv", header=TRUE)
data03<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20160101_20201231.csv", header=TRUE)
data04<-read.csv(file="Messstationen Tagesdaten v2 Datensatz_20210101_20231231.csv", header=TRUE)

df_list_22_temp_night<-list( data00, data01, data02, data03, data04)

#merge all data frames in list
data_22_temp_night<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list_22_temp_night)
data_temp_night<-inner_join(austria.clean, data_22_temp_night, by="station")

data_temp_night$time<-as.Date(data_temp_night$time)
data_temp_night$Year<-year(data_temp_night$time)
data_temp_night$Month<-month(data_temp_night$time)

#find double stations: 
test<-extract_ids_of_stations(data_temp_night)

#remove them
data_temp_night<-data_temp_night[  !(data_temp_night$station=="20021") ,]

#how many substations does one station have:
test<-check_multiple_substations(data_temp_night)

#remove double substation
data_temp_night<-data_temp_night %>% group_by(Stationsname,time) %>% slice(1)
data_temp_night<-data_temp_night[,-c(8)]

data_temp_night_pressure_humidity<-merge(data_temp_night, data_temp_pressure_humidity, by=c("station", "Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))

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
weather_data_all<-merge( data_temp_night_pressure_humidity,data_rain, by=c("station","Stationsname","Longitude","Latitude", "Elevation",  "Year","time","Month"))

colnames(weather_data_all)<-c("ID", "Station","Longitude", "Latitude", "Elevation",  "Year", "Time","Month","Temp_min_night","Temp_min", "Temp_mean",  "Temp_max","Pressure_mean","Humidity_mean", "Precip_sum")

#remove 2000.01.01-2000.01.02 since we start with calender week 1: 2000.01.03-2000.01.09
weather_data_all<-weather_data_all[-which(weather_data_all$Time=="2000-01-01"),]
weather_data_all<-weather_data_all[-which(weather_data_all$Time=="2000-01-02"),]

weather_data_all$Day<-wday(weather_data_all$Time, week_start=1)
weather_data_all$Week<-isoweek(weather_data_all$Time)
weather_data_all$Month<-month(weather_data_all$Time)

entries_per_station_year<-weather_data_all %>% group_by(Station,Year) %>% summarise(count=n())

setwd("~/Documents/Mortality & heat")
save(weather_data_all,file="01_raw_data.R")

############## with imputation ##########
load("01_raw_data.R")
library(lubridate)
library(omnibus)
library(dplyr)
library(tidyverse)
library(mice)

label_hot_weeks <- function(df) {
  
  #get all unique stations from data.frame
  Stations<-unique(df$Station)
  
  #generate date_frame for weekly_summary_statistics
  summary_data_frame<-data.frame()
  
  df$Last_week_was_dry <- FALSE
  df$Cold_week<-FALSE
  df$Hot_week<-FALSE
  df$Mild_week<-FALSE
  df$Length_no_rain<-NA
  df$Super_cold_week<-FALSE
  df$Tropical_week<-FALSE
  df$Strong_discomfort_humidity<-FALSE
  df$Severe_malaise_humidity<-FALSE  
  df$Increased_risk_humidity<-FALSE
  df$Serious_risk_humidity<-FALSE
  
  
  
  for(i in 1:length(Stations)){
    
    cat("\nWe have Station number",i, "from 211.")
    start_day<-1
    index<-1
    
    #create data.frame for each station
    subset_df<-df[which(df$Station==Stations[i]),]
    
    #order dataframe according to time
    subset_df <- subset_df[order(subset_df$Date), ]#subset_df$Station, 
    total_days <- nrow(subset_df)
    
    subset_df$Temp_spread_day<-subset_df$Temp_max-subset_df$Temp_min    #too little data for Temp_spread_night
    
    
    for (k in 1:(total_days/7)){
      
      # Get the range of days for the current window of 7 days
      current_week_data <- subset_df[start_day:(start_day + 6), ]
      #print(current_week_data)
      
      current_week_data$Length_no_rain<-longRun(current_week_data$Precip_sum,0)
      
      subset_df[start_day:(start_day + 6), ]<-current_week_data
      
      #if there are missing values then impute them
      if(any(is.na(current_week_data$Precip_sum) | is.na(current_week_data$Temp_max) |
             is.na(current_week_data$Temp_min) | is.na(current_week_data$Temp_mean)  |
             is.na(current_week_data$Humidity_mean)))      {
        
        Month<-unique(current_week_data$Month)
        month_subset_df<-subset_df[which(subset_df$Month==Month),]
        
        if(length(Month)=="2"){
          month_subset_df<-subset_df[which(subset_df$Month==Month[1] | subset_df$Month==Month[2]),]
          month_missing_values<-month_subset_df[,c(7,9,10,11,12,13)]
          month_imputed_values<-mice(month_missing_values, maxit=50, method="sample",print=F)
          month_full_subset_df<-complete(month_imputed_values,1, include=F)
          
          index<-match(current_week_data$Date,month_full_subset_df$Date)
          
          #merge
          current_week_data[1:7, "Temp_min"]<-month_full_subset_df[index, "Temp_min"]
          current_week_data[1:7, "Temp_max"]<-month_full_subset_df[index, "Temp_max"]
          current_week_data[1:7, "Temp_mean"]<-month_full_subset_df[index, "Temp_mean"]
          current_week_data[1:7, "Humidity_mean"]<-month_full_subset_df[index, "Humidity_mean"]
          #current_week_data[1:7, "Pressure_mean"]<-month_full_subset_df[index, "Pressure_mean"]
          current_week_data[1:7, "Precip_sum"]<-month_full_subset_df[index, "Precip_sum"]
          
          current_week_data$Length_no_rain<-longRun(current_week_data$Precip_sum,0)
          current_week_data$Temp_spread_day<-current_week_data$Temp_max-current_week_data$Temp_min
          subset_df[start_day:(start_day + 6), ]<-current_week_data
          
          #print(current_week_data)
        }
        
        
        month_missing_values<-month_subset_df[,c(7,9,10,11,12,13)]
        month_imputed_values<-mice(month_missing_values, maxit=50, method="sample",print=F)
        
        #combine with 1st of 100 imputed datasets
        month_full_subset_df<-complete(month_imputed_values,1, include=F)
        
        
        #find the right index in the other dataframe
        index<-match(current_week_data$Date,month_full_subset_df$Date)
        
        #merge
        current_week_data[1:7, "Temp_min"]<-month_full_subset_df[index, "Temp_min"]
        current_week_data[1:7, "Temp_max"]<-month_full_subset_df[index, "Temp_max"]
        current_week_data[1:7, "Temp_mean"]<-month_full_subset_df[index, "Temp_mean"]
        current_week_data[1:7, "Humidity_mean"]<-month_full_subset_df[index, "Humidity_mean"]
        #current_week_data[1:7, "Pressure_mean"]<-month_full_subset_df[index, "Pressure_mean"]
        current_week_data[1:7, "Precip_sum"]<-month_full_subset_df[index, "Precip_sum"]
        
        current_week_data$Length_no_rain<-longRun(current_week_data$Precip_sum,0)
        current_week_data$Temp_spread_day<-current_week_data$Temp_max-current_week_data$Temp_min
        
        subset_df[start_day:(start_day + 6), ]<-current_week_data
        
      }
      
      consecutive_days <- 0
      cold_week<-0
      hot_week<-0
      mild_week<-0
      super_cold_week<-0
      tropical_week<-0
      strong_discomfort_humidity<-0
      severe_malaise_humidity<-0
      increased_risk_humidity<-0
      serious_risk_humidity<-0
      
      dry_condition_met <- FALSE
      cold_condition_met<-FALSE
      hot_condition_met<-FALSE
      mild_condition_met<-FALSE
      super_cold_condition_met<-FALSE
      tropical_condition_met<-FALSE
      strong_discomfort_humidity_condition_met<-FALSE
      severe_malaise_humidity_condition_met<-FALSE
      increased_risk_humidity_condition_met<-FALSE
      serious_risk_humidity_condition_met<-FALSE
      
      for (j in 1:nrow(current_week_data)) {
        
        # check conditions for dry_week
        if (!is.na(current_week_data$Precip_sum[j]) && current_week_data$Precip_sum[j] == 0 &&
            ((!is.na(current_week_data$Temp_mean[j]) && current_week_data$Temp_mean[j] > 21.8) ||
             (!is.na(current_week_data$Temp_mean[j]) && current_week_data$Temp_mean[j] < -5))) {
          consecutive_days <- consecutive_days + 1
          if (consecutive_days >= 3) {
            dry_condition_met <- TRUE
            break
          } #nested if end
        } #heat cold wave if end
        else {
          consecutive_days <- 0
        }
        
        
        #check conditions for cold week according to Ages
        if(!is.na(current_week_data$Temp_min[j]) && current_week_data$Temp_min[j] < 0){
          cold_week <- cold_week + 1
          if (cold_week >= 3) {
            cold_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          cold_week <- 0
        }
        
        #check condition for super cold week
        if(!is.na(current_week_data$Temp_min[j]) && current_week_data$Temp_min[j] < -5){
          super_cold_week <- super_cold_week + 1
          if (super_cold_week >= 2) {
            super_cold_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          super_cold_week <- 0
        }
        
        #check conditions for hot week according to Ages
        if(!is.na(current_week_data$Temp_min[j]) && current_week_data$Temp_min[j] > 18){
          hot_week <- hot_week + 1
          if (hot_week >= 3) {
            hot_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          hot_week <- 0
        }
        
        #check conditions for hot week according to Ages
        if(!is.na(current_week_data$Temp_min[j]) && (current_week_data$Temp_mean[j] > 2 && current_week_data$Temp_mean[j] < 9 )){
          mild_week <- mild_week + 1
          if (mild_week >=3 ) {
            mild_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          mild_week <- 0
        }
        
        #check condition for super hot week
        if(!is.na(current_week_data$Temp_max[j]) && current_week_data$Temp_max[j] > 29){
          tropical_week <- tropical_week + 1
          if (tropical_week >= 2) {
            tropical_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          tropical_week <- 0
        }
        
        #check condition for strong discomfort with humidity
        if(!is.na(current_week_data$Temp_max[j]) && 
           (current_week_data$Temp_max[j]>33 && current_week_data$Temp_max[j] < 37 &&  #34-36° <30%
            current_week_data$Humidity_mean[j]<30)  ||
           (current_week_data$Temp_max[j]>32 && current_week_data$Temp_max[j] < 36 &&  #33-35° 30-34%
            current_week_data$Humidity_mean[j]>29 && current_week_data$Humidity_mean[j]<35) ||
           (current_week_data$Temp_max[j]>31 && current_week_data$Temp_max[j] < 35 &&  #32-34° 35-39%
            current_week_data$Humidity_mean[j]>34 && current_week_data$Humidity_mean[j]<40) ||
           (current_week_data$Temp_max[j]>30 && current_week_data$Temp_max[j] < 34 &&  #31-33° 40-44%
            current_week_data$Humidity_mean[j]>39 && current_week_data$Humidity_mean[j]<45) ||
           (current_week_data$Temp_max[j]>29 && current_week_data$Temp_max[j] < 33 &&  #30-32° 45-49%
            current_week_data$Humidity_mean[j]>44 && current_week_data$Humidity_mean[j]<50) ||
           (current_week_data$Temp_max[j]>28 && current_week_data$Temp_max[j] < 32 &&  #29-31° 50-54%
            current_week_data$Humidity_mean[j]>49 && current_week_data$Humidity_mean[j]<55) ||
           (current_week_data$Temp_max[j]>28 && current_week_data$Temp_max[j] < 31 &&  #29-30° 55-59%
            current_week_data$Humidity_mean[j]>54 && current_week_data$Humidity_mean[j]<60) ||
           (current_week_data$Temp_max[j]>27 && current_week_data$Temp_max[j] < 31 &&  #28-30° 60-64%
            current_week_data$Humidity_mean[j]>59 && current_week_data$Humidity_mean[j]<65) ||
           (current_week_data$Temp_max[j]>27 && current_week_data$Temp_max[j] < 30 &&  #28-29° 65-69%
            current_week_data$Humidity_mean[j]>64 && current_week_data$Humidity_mean[j]<70) ||
           (current_week_data$Temp_max[j]>26 && current_week_data$Temp_max[j] < 30 &&  #27-29° 70-74%
            current_week_data$Humidity_mean[j]>69 && current_week_data$Humidity_mean[j]<75) ||
           (current_week_data$Temp_max[j]>26 && current_week_data$Temp_max[j] < 29 &&  #27-28° 75-79%
            current_week_data$Humidity_mean[j]>74 && current_week_data$Humidity_mean[j]<80) ||
           (current_week_data$Temp_max[j]>25 && current_week_data$Temp_max[j] < 29 &&  #26-28° 80-84%
            current_week_data$Humidity_mean[j]>79 && current_week_data$Humidity_mean[j]<85) ||
           (current_week_data$Temp_max[j]>25 && current_week_data$Temp_max[j] < 28 &&  #26-27° 85-89%
            current_week_data$Humidity_mean[j]>84 && current_week_data$Humidity_mean[j]<90) ||
           (current_week_data$Temp_max[j]>24 && current_week_data$Temp_max[j] < 28 &&  #25-27° 90-94%
            current_week_data$Humidity_mean[j]>89 && current_week_data$Humidity_mean[j]<95) ||
           (current_week_data$Temp_max[j]>23 && current_week_data$Temp_max[j] < 27 &&  #24-26° 95%-100%
            current_week_data$Humidity_mean[j]>94 )){
          strong_discomfort_humidity <- strong_discomfort_humidity + 1
          if (strong_discomfort_humidity >= 1) {
            strong_discomfort_humidity_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          strong_discomfort_humidity <- 0
        }
        
        
        
        #check condition for severe malaise with humidity
        if(!is.na(current_week_data$Temp_max[j]) && 
           (current_week_data$Temp_max[j]>36 && current_week_data$Temp_max[j] < 41 &&  #37-40° <30%
            current_week_data$Humidity_mean[j]<30)  ||
           (current_week_data$Temp_max[j]>35 && current_week_data$Temp_max[j] < 40 &&  #36-39° 30-34%
            current_week_data$Humidity_mean[j]>29 && current_week_data$Humidity_mean[j]<35) ||
           (current_week_data$Temp_max[j]>34 && current_week_data$Temp_max[j] < 39 &&  #35-38° 35-39%
            current_week_data$Humidity_mean[j]>34 && current_week_data$Humidity_mean[j]<40) ||
           (current_week_data$Temp_max[j]>33 && current_week_data$Temp_max[j] < 38 &&  #34-37° 40-44%
            current_week_data$Humidity_mean[j]>39 && current_week_data$Humidity_mean[j]<45) ||
           (current_week_data$Temp_max[j]>32 && current_week_data$Temp_max[j] < 37 &&  #33-36° 45-49%
            current_week_data$Humidity_mean[j]>44 && current_week_data$Humidity_mean[j]<50) ||
           (current_week_data$Temp_max[j]>31 && current_week_data$Temp_max[j] < 36 &&  #32-35° 50-54%
            current_week_data$Humidity_mean[j]>49 && current_week_data$Humidity_mean[j]<55) ||
           (current_week_data$Temp_max[j]>30 && current_week_data$Temp_max[j] < 35 &&  #31-34° 55-59%
            current_week_data$Humidity_mean[j]>54 && current_week_data$Humidity_mean[j]<60) ||
           (current_week_data$Temp_max[j]>30 && current_week_data$Temp_max[j] < 34 &&  #31-33° 60-64%
            current_week_data$Humidity_mean[j]>59 && current_week_data$Humidity_mean[j]<65) ||
           (current_week_data$Temp_max[j]>29 && current_week_data$Temp_max[j] < 33 &&  #30-32° 65-74%
            current_week_data$Humidity_mean[j]>64 && current_week_data$Humidity_mean[j]<75) ||
           (current_week_data$Temp_max[j]>28 && current_week_data$Temp_max[j] < 32 &&  #29-31° 75-84%
            current_week_data$Humidity_mean[j]>74 && current_week_data$Humidity_mean[j]<85) ||
           (current_week_data$Temp_max[j]>27 && current_week_data$Temp_max[j] < 31 &&  #28-30° 85-89%
            current_week_data$Humidity_mean[j]>84 && current_week_data$Humidity_mean[j]<90) ||
           (current_week_data$Temp_max[j]>27 && current_week_data$Temp_max[j] < 30 &&  #28-29° 90-94%
            current_week_data$Humidity_mean[j]>89 && current_week_data$Humidity_mean[j]<95) ||
           (current_week_data$Temp_max[j]>26 && current_week_data$Temp_max[j] < 29 &&  #27-28° 95%-100%
            current_week_data$Humidity_mean[j]>94 ))    {
          severe_malaise_humidity <- severe_malaise_humidity + 1
          if (severe_malaise_humidity >= 1) {
            severe_malaise_humidity_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          severe_malaise_humidity <- 0
        }
        
        
        
        #check condition for increased risk with humidity
        if(!is.na(current_week_data$Temp_max[j]) && 
           (current_week_data$Temp_max[j]>40 && current_week_data$Temp_max[j] < 43 &&  #41-42° <30%
            current_week_data$Humidity_mean[j]<30)  ||
           (current_week_data$Temp_max[j]>39 && current_week_data$Temp_max[j] < 43 &&  #40-42° 30-34%
            current_week_data$Humidity_mean[j]>29 && current_week_data$Humidity_mean[j]<35) ||
           (current_week_data$Temp_max[j]>38 && current_week_data$Temp_max[j] < 43 &&  #39-42° 35-39%
            current_week_data$Humidity_mean[j]>34 && current_week_data$Humidity_mean[j]<40) ||
           (current_week_data$Temp_max[j]>37 && current_week_data$Temp_max[j] < 42 &&  #38-41° 40-44%
            current_week_data$Humidity_mean[j]>39 && current_week_data$Humidity_mean[j]<45) ||
           (current_week_data$Temp_max[j]>36 && current_week_data$Temp_max[j] < 41 &&  #37-40° 45-49%
            current_week_data$Humidity_mean[j]>44 && current_week_data$Humidity_mean[j]<50) ||
           (current_week_data$Temp_max[j]>35 && current_week_data$Temp_max[j] < 40 &&  #36-39° 50-54%
            current_week_data$Humidity_mean[j]>49 && current_week_data$Humidity_mean[j]<55) ||
           (current_week_data$Temp_max[j]>34 && current_week_data$Temp_max[j] < 39 &&  #35-38° 55-59%
            current_week_data$Humidity_mean[j]>54 && current_week_data$Humidity_mean[j]<60) ||
           (current_week_data$Temp_max[j]>33 && current_week_data$Temp_max[j] < 38 &&  #34-37° 60-64%
            current_week_data$Humidity_mean[j]>59 && current_week_data$Humidity_mean[j]<65) ||
           (current_week_data$Temp_max[j]>32 && current_week_data$Temp_max[j] < 37 &&  #33-36° 65-69%
            current_week_data$Humidity_mean[j]>64 && current_week_data$Humidity_mean[j]<70) ||
           (current_week_data$Temp_max[j]>32 && current_week_data$Temp_max[j] < 36 &&  #33-36° 70-74%
            current_week_data$Humidity_mean[j]>69 && current_week_data$Humidity_mean[j]<75) ||
           (current_week_data$Temp_max[j]>31 && current_week_data$Temp_max[j] < 36 &&  #32-35° 75-79%
            current_week_data$Humidity_mean[j]>74 && current_week_data$Humidity_mean[j]<80) ||
           (current_week_data$Temp_max[j]>31 && current_week_data$Temp_max[j] < 35 &&  #32-34° 80-84%
            current_week_data$Humidity_mean[j]>79 && current_week_data$Humidity_mean[j]<85) ||
           (current_week_data$Temp_max[j]>30 && current_week_data$Temp_max[j] < 34 &&  #31-33° 85-89%
            current_week_data$Humidity_mean[j]>84 && current_week_data$Humidity_mean[j]<90) ||
           (current_week_data$Temp_max[j]>29 && current_week_data$Temp_max[j] < 33 &&  #28-29° 90-94%
            current_week_data$Humidity_mean[j]>89 && current_week_data$Humidity_mean[j]<95) ||
           (current_week_data$Temp_max[j]>28 && current_week_data$Temp_max[j] < 23 &&  #27-28° 95%-100%
            current_week_data$Humidity_mean[j]>94 )  ){
          increased_risk_humidity <- increased_risk_humidity + 1
          if (increased_risk_humidity >= 1) {
            increased_risk_humidity_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          increased_risk_humidity <- 0
        }
        
        #check condition for serious risk with humidity
        if(!is.na(current_week_data$Temp_max[j]) && 
           (current_week_data$Temp_max[j]>39 && current_week_data$Temp_max[j] < 43 &&  #40-42° 40-54%
            current_week_data$Humidity_mean[j]>39 && current_week_data$Humidity_mean[j]<55)  ||
           (current_week_data$Temp_max[j]>38 && current_week_data$Temp_max[j] < 43 &&  #39-42° 55-59%
            current_week_data$Humidity_mean[j]>54 && current_week_data$Humidity_mean[j]<60) ||
           (current_week_data$Temp_max[j]>37 && current_week_data$Temp_max[j] < 43 &&  #38-42° 60-64%
            current_week_data$Humidity_mean[j]>59 && current_week_data$Humidity_mean[j]<65) ||
           (current_week_data$Temp_max[j]>36 && current_week_data$Temp_max[j] < 43 &&  #37-42° 65-69%
            current_week_data$Humidity_mean[j]>64 && current_week_data$Humidity_mean[j]<70) ||
           (current_week_data$Temp_max[j]>35 && current_week_data$Temp_max[j] < 43 &&  #36-42° 70-79%
            current_week_data$Humidity_mean[j]>69 && current_week_data$Humidity_mean[j]<80) ||
           (current_week_data$Temp_max[j]>34 && current_week_data$Temp_max[j] < 43 &&  #35-42° 80-84%
            current_week_data$Humidity_mean[j]>79 && current_week_data$Humidity_mean[j]<85) ||
           (current_week_data$Temp_max[j]>33 && current_week_data$Temp_max[j] < 43 &&  #34-42° 85-94%
            current_week_data$Humidity_mean[j]>84 && current_week_data$Humidity_mean[j]<95) ||
           (current_week_data$Temp_max[j]>32 && current_week_data$Temp_max[j] < 43 &&  #33-42° >95%
            current_week_data$Humidity_mean[j]>94) ){
          serious_risk_humidity <- serious_risk_humidity + 1
          if (serious_risk_humidity >= 1) {
            serious_risk_humidity_condition_met <- TRUE
            break
          }  #nested if end
        }  #tropical night if end
        else {
          serious_risk_humidity <- 0
        }
        
        
      } #for loop over current week end
      
      
      
      if (dry_condition_met && (start_day + 7) <= total_days) {
        # Label the next 7 days as Dry Week
        subset_df[(start_day+7):(start_day+7+6), "Last_week_was_dry"] <- TRUE
      }
      
      if (cold_condition_met ) {
        # Label THIS week as cold_nights
        current_week_data[c(1:7),"Cold_week"] <- TRUE
      }
      
      if (super_cold_condition_met ) {
        # Label THIS week as cold_nights
        current_week_data[c(1:7),"Super_cold_week"] <- TRUE
      }
      
      if (hot_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Hot_week"] <- TRUE
      }
      #} #else
      
      if (mild_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Mild_week"] <- TRUE
      }
      #} #else
      
      if (tropical_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Tropical_week"] <- TRUE
      }
      
      if (strong_discomfort_humidity_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Strong_discomfort_humidity"] <- TRUE
      }
      
      
      if (severe_malaise_humidity_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Severe_malaise_humidity"] <- TRUE
      }
      
      
      if (increased_risk_humidity_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Increased_risk_humidity"] <- TRUE
      }
      
      if (serious_risk_humidity_condition_met ) {
        # Label THIS week as Tropical_cold_nights
        current_week_data[c(1:7),"Serious_risk_humidity"] <- TRUE
      }
      
      
      summary_current_week<-  group_by(current_week_data,Station, Week) %>% reframe(Year=isoyear(Date), Date=Date,
                                                                                    Week=isoweek(Date),
                                                                                    Longitude=mean(Longitude), Latitude=mean(Latitude), Elevation=mean(Elevation)/1000, 
                                                                                    #Temp_min_night_mean=round(mean(Temp_min_night),3),
                                                                                    Temp_mean=round(mean(Temp_mean),3), 
                                                                                    Temp_min_mean=round(mean(Temp_min),3), 
                                                                                    Temp_max_mean=round(mean(Temp_max),3), 
                                                                                    Temp_min=round(min(Temp_min),3),  
                                                                                    Temp_max=round(max(Temp_max),3),
                                                                                    Precip_mean=round(mean(Precip_sum),3),
                                                                                    Humidity_mean=round(mean(Humidity_mean),3),
                                                                                    #Pressure_mean=round(mean(Pressure_mean),3),
                                                                                    Temp_spread_day=round(mean(Temp_spread_day),3),
                                                                                    Length_no_rain=mean(Length_no_rain), 
                                                                                    Last_week_was_dry=mean(Last_week_was_dry),
                                                                                    Cold_week=mean(Cold_week),
                                                                                    Hot_week=mean(Hot_week) ,
                                                                                    Mild_week=mean(Mild_week),
                                                                                    Tropical_week=mean(Tropical_week),
                                                                                    Super_cold_week=mean(Super_cold_week),
                                                                                    Strong_discomfort_humidity=mean(Strong_discomfort_humidity),
                                                                                    Severe_malaise_humidity=mean(Severe_malaise_humidity),
                                                                                    Increased_risk_humidity=mean(Increased_risk_humidity),
                                                                                    Serious_risk_humidity=mean(Serious_risk_humidity))
      
      
      
      summary_data_frame<-rbind(summary_data_frame, summary_current_week)
      
      start_day<-start_day+7
      
    }
    
  }
  
  return(summary_data_frame)
}


# safe maximum threshold is 5% for large datasets (omit Pressure and Temp_min_night)

#too much missing values in Pressure_mean and Temp_min_night
weather_data_all<-weather_data_all[,-c(9,13)] 

colnames(weather_data_all)[7]<-"Date"


weather_data_all$Day<-day(weather_data_all$Date)
weather_data_all$Week<-isoweek(weather_data_all$Date)
weather_data_all$Month<-month(weather_data_all$Date)
weather_data_all$Year<-isoyear(weather_data_all$Date)

#later we only have data from 2002 onwards
weather_data_all<-weather_data_all[weather_data_all$Year>="2002",]
weather_data_all<-weather_data_all[weather_data_all$Year<"2020",]

pMiss<-function(x){sum(is.na(x))/length(x)*100}
apply(weather_data_all, 2, pMiss)

station_out<-weather_data_all %>% group_by(Station) %>% summarise(count=n())
station_out<-station_out[station_out$count<6573,] #leap years 04, 08, 12, 16 (starting 2002, ending 2019)

double_entries<-weather_data_all %>% group_by(Station,Year) %>% summarise(count=n())
#double_entries_less<-double_entries[which(double_entries$count<364),]

missing_per_station<-weather_data_all %>% group_by(Station,Year) %>% summarise_all(~sum(is.na(.)))


weather_data_all_dry_weeks_districts<-label_hot_weeks(weather_data_all)


setwd("~/Documents/Mortality & heat")
save(weather_data_all_dry_weeks_districts, file="01_weather_data_districts.R")

