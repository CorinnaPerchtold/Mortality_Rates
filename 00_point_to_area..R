library(sf)
library(INLA)
library(Matrix)
library(foreach)
library(parallel)
library(reshape2)
library(viridis)
library(viridisLite)
library(LaplacesDemon)
library(ggplot2)
library(dplyr)

load("01_weather_data_districts.R")


#district map 
states<-read_sf("gadm41_AUT_1.shp",)
districts<-read_sf("gadm41_AUT_2.shp",)
Austria_districts<-st_geometry(districts)

coords.stations=data.frame(Longitude=weather_data_all_dry_weeks_districts$Longitude, Latitude=weather_data_all_dry_weeks_districts$Latitude)

##################### first step: assign correct district names to map #######################
st_intersection(districts, states$geometry[1])->districts_BGL
st_intersection(districts, states$geometry[2])->districts_Carinthia
st_intersection(districts, states$geometry[3])->districts_Lower_Austria
st_intersection(districts, states$geometry[4])->districts_Upper_Austria
st_intersection(districts, states$geometry[5])->districts_Salzburg
st_intersection(districts, states$geometry[6])->districts_Styria
st_intersection(districts, states$geometry[7])->districts_Tyrol
st_intersection(districts, states$geometry[8])->districts_Vorarlberg
st_intersection(districts, states$geometry[9])->districts_Vienna

weather_data_all_dry_weeks_districts$District<-"Zero" #the entries that stay 0 is the border

#if we want to impute for one station/district, we need length of one station and multiply it with 12 (missing districts)
station<-weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Abtenau",]
length_station<-length(station$Station)

missing_data<- data.frame(matrix(data=NA, ncol=length(unique(colnames(station))), nrow=length_station*12))
colnames(missing_data)<-colnames(station)

missing_data$Station[1:length_station]<-"Eferding"
missing_data$District[1:length_station]<-"Eferding"
missing_data$Week[1:length_station]<-station$Week
missing_data$Year[1:length_station]<-station$Year
missing_data$Longitude[1:length_station]<-13.938488
missing_data$Latitude[1:length_station]<-48.361313
missing_data$Elevation[1:length_station]<-0.271

eferding<-missing_data[missing_data$Station=="Eferding",]
eferding.coord<-data.frame(Longitude=eferding$Longitude,Latitude=eferding$Latitude)

missing_data$Station[(length_station+1):(2*length_station)]<-"Jennersdorf"
missing_data$District[(length_station+1):(2*length_station)]<-"Jennersdorf"
missing_data$Week[(length_station+1):(2*length_station)]<-station$Week
missing_data$Year[(length_station+1):(2*length_station)]<-station$Year
missing_data$Longitude[(length_station+1):(2*length_station)]<-16.141184
missing_data$Latitude[(length_station+1):(2*length_station)]<-46.940926
missing_data$Elevation[(length_station+1):(2*length_station)]<-0.242

jenn<-missing_data[missing_data$Station=="Jennersdorf",]
jenn.coord<-data.frame(Longitude=jenn$Longitude,Latitude=jenn$Latitude)

missing_data$Station[(2*length_station+1):(3*length_station)]<-"Eisenstadt-Umgebung"
missing_data$District[(2*length_station+1):(3*length_station)]<-"Eisenstadt-Umgebung"
missing_data$Week[(2*length_station+1):(3*length_station)]<-station$Week
missing_data$Year[(2*length_station+1):(3*length_station)]<-station$Year
missing_data$Longitude[(2*length_station+1):(3*length_station)]<- 16.689366     
missing_data$Latitude[(2*length_station+1):(3*length_station)]<- 47.934552
missing_data$Elevation[(2*length_station+1):(3*length_station)]<-0.193

eisen<-missing_data[missing_data$Station=="Eisenstadt-Umgebung",]
eisen.coord<-data.frame(Longitude=eisen$Longitude,Latitude=eisen$Latitude)

missing_data$Station[(3*length_station+1):(4*length_station)]<-"Rust"
missing_data$District[(3*length_station+1):(4*length_station)]<-"Rust"
missing_data$Week[(3*length_station+1):(4*length_station)]<-station$Week
missing_data$Year[(3*length_station+1):(4*length_station)]<-station$Year
missing_data$Longitude[(3*length_station+1):(4*length_station)]<-16.669787
missing_data$Latitude[(3*length_station+1):(4*length_station)]<-47.798013
missing_data$Elevation[(3*length_station+1):(4*length_station)]<-0.127

rust<-missing_data[missing_data$Station=="Rust",]
rust.coord<-data.frame(Longitude=rust$Longitude,Latitude=rust$Latitude)

missing_data$Station[(4*length_station+1):(5*length_station)]<-"Leibnitz"
missing_data$District[(4*length_station+1):(5*length_station)]<-"Leibnitz"
missing_data$Week[(4*length_station+1):(5*length_station)]<-station$Week
missing_data$Year[(4*length_station+1):(5*length_station)]<-station$Year
missing_data$Longitude[(4*length_station+1):(5*length_station)]<-15.494707
missing_data$Latitude[(4*length_station+1):(5*length_station)]<- 46.725321
missing_data$Elevation[(4*length_station+1):(5*length_station)]<-0.275

leib<-missing_data[missing_data$Station=="Leibnitz",]
leib.coord<-data.frame(Longitude=leib$Longitude,Latitude=leib$Latitude)

missing_data$Station[(5*length_station+1):(6*length_station)]<-"Wels"
missing_data$District[(5*length_station+1):(6*length_station)]<-"Wels"
missing_data$Week[(5*length_station+1):(6*length_station)]<-station$Week
missing_data$Year[(5*length_station+1):(6*length_station)]<-station$Year
missing_data$Longitude[(5*length_station+1):(6*length_station)]<-14.015313
missing_data$Latitude[(5*length_station+1):(6*length_station)]<- 48.170823
missing_data$Elevation[(5*length_station+1):(6*length_station)]<-0.317

wels<-missing_data[missing_data$Station=="Wels",]
wels.coord<-data.frame(Longitude=wels$Longitude,Latitude=wels$Latitude)

missing_data$Station[(6*length_station+1):(7*length_station)]<-"Mattersburg"
missing_data$District[(6*length_station+1):(7*length_station)]<-"Mattersburg"
missing_data$Week[(6*length_station+1):(7*length_station)]<-station$Week
missing_data$Year[(6*length_station+1):(7*length_station)]<-station$Year
missing_data$Longitude[(6*length_station+1):(7*length_station)]<-16.365168
missing_data$Latitude[(6*length_station+1):(7*length_station)]<- 47.734484
missing_data$Elevation[(6*length_station+1):(7*length_station)]<-0.256

matter<-missing_data[missing_data$Station=="Mattersburg",]
matter.coord<-data.frame(Longitude=matter$Longitude,Latitude=matter$Latitude)

missing_data$Station[(7*length_station+1):(8*length_station)]<-"Perg"
missing_data$District[(7*length_station+1):(8*length_station)]<-"Perg"
missing_data$Week[(7*length_station+1):(8*length_station)]<-station$Week
missing_data$Year[(7*length_station+1):(8*length_station)]<-station$Year
missing_data$Longitude[(7*length_station+1):(8*length_station)]<-14.64444
missing_data$Latitude[(7*length_station+1):(8*length_station)]<- 48.254431
missing_data$Elevation[(7*length_station+1):(8*length_station)]<-0.250

perg<-missing_data[missing_data$Station=="Perg",]
perg.coord<-data.frame(Longitude=perg$Longitude,Latitude=perg$Latitude)

missing_data$Station[(8*length_station+1):(9*length_station)]<-"Steyr"
missing_data$District[(8*length_station+1):(9*length_station)]<-"Steyr"
missing_data$Week[(8*length_station+1):(9*length_station)]<-station$Week
missing_data$Year[(8*length_station+1):(9*length_station)]<-station$Year
missing_data$Longitude[(8*length_station+1):(9*length_station)]<-14.421026
missing_data$Latitude[(8*length_station+1):(9*length_station)]<- 48.051772
missing_data$Elevation[(8*length_station+1):(9*length_station)]<-0.310

steyr<-missing_data[missing_data$Station=="Steyr",]
steyr.coord<-data.frame(Longitude=steyr$Longitude,Latitude=steyr$Latitude)

missing_data$Station[(9*length_station+1):(10*length_station)]<-"Waidhofen an der Thaya"
missing_data$District[(9*length_station+1):(10*length_station)]<-"Waidhofen an der Thaya"
missing_data$Week[(9*length_station+1):(10*length_station)]<-station$Week
missing_data$Year[(9*length_station+1):(10*length_station)]<-station$Year
missing_data$Longitude[(9*length_station+1):(10*length_station)]<-15.324516
missing_data$Latitude[(9*length_station+1):(10*length_station)]<- 48.826264
missing_data$Elevation[(9*length_station+1):(10*length_station)]<-0.510

thaya<-missing_data[missing_data$Station=="Waidhofen an der Thaya",]
thaya.coord<-data.frame(Longitude=thaya$Longitude,Latitude=thaya$Latitude)

missing_data$Station[(10*length_station+1):(11*length_station)]<-"Voitsberg"
missing_data$District[(10*length_station+1):(11*length_station)]<-"Voitsberg"
missing_data$Week[(10*length_station+1):(11*length_station)]<-station$Week
missing_data$Year[(10*length_station+1):(11*length_station)]<-station$Year
missing_data$Longitude[(10*length_station+1):(11*length_station)]<-15.164156
missing_data$Latitude[(10*length_station+1):(11*length_station)]<- 47.043139
missing_data$Elevation[(10*length_station+1):(11*length_station)]<-0.394

voit<-missing_data[missing_data$Station=="Voitsberg",]
voit.coord<-data.frame(Longitude=voit$Longitude,Latitude=voit$Latitude)

missing_data$Station[(11*length_station+1):(12*length_station)]<-"St Poelten-Land"
missing_data$District[(11*length_station+1):(12*length_station)]<-"St Poelten-Land"
missing_data$Week[(11*length_station+1):(12*length_station)]<-station$Week
missing_data$Year[(11*length_station+1):(12*length_station)]<-station$Year
missing_data$Longitude[(11*length_station+1):(12*length_station)]<-15.600158
missing_data$Latitude[(11*length_station+1):(12*length_station)]<- 48.099632
missing_data$Elevation[(11*length_station+1):(12*length_station)]<-0.321

st<-missing_data[missing_data$Station=="St Poelten-Land",]
st.coord<-data.frame(Longitude=st$Longitude,Latitude=st$Latitude)

weather_data_all_dry_weeks_districts<-rbind(weather_data_all_dry_weeks_districts, missing_data)
coords.stations<-data.frame(Longitude=weather_data_all_dry_weeks_districts$Longitude, Latitude=weather_data_all_dry_weeks_districts$Latitude)



#9 districts Burgenland
districts_BGL$geometry[1]->"Eisenstadt-Umgebung"

districts_BGL$geometry[2]->"Eisenstadt"
id_eisenstadt<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Eisenstadt`[[1]][[1]][,1], `Eisenstadt`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_eisenstadt]<-"Eisenstadt"

districts_BGL$geometry[3]->"Guessing"
id_guessing<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Guessing`[[1]][[1]][,1], `Guessing`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_guessing]<-"Guessing"

districts_BGL$geometry[4]->"Jennersdorf"
id_jennersdorf<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Jennersdorf`[[1]][[1]][,1], `Jennersdorf`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_jennersdorf]<-"Jennersdorf"

districts_BGL$geometry[5]->"Mattersburg"
id_mattersburg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Mattersburg`[[1]][[1]][,1], `Mattersburg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_mattersburg]<-"Mattersburg"

districts_BGL$geometry[6]->"Neusiedl am See"
id_neusiedl<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Neusiedl am See`[[1]][[1]][,1], `Neusiedl am See`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_neusiedl]<-"Neusiedl am See"

districts_BGL$geometry[7]->"Oberpullendorf"
id_oberpullendorf<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Oberpullendorf`[[1]][[1]][,1], `Oberpullendorf`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_oberpullendorf]<-"Oberpullendorf"

districts_BGL$geometry[8]->"Oberwart"
id_oberwart<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Oberwart`[[1]][[1]][,1], `Oberwart`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_oberwart]<-"Oberwart"

districts_BGL$geometry[9]->"Rust"
id_rust<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Rust`[[1]][[1]][,1], `Rust`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_rust]<-"Rust"

BGL_districts_df<-st_as_sf(data.frame(District=c("Eisenstadt-Umgebung","Eisenstadt", "Guessing", "Jennersdorf",    "Mattersburg", "Neusiedl am See", "Oberpullendorf", "Oberwart", "Rust"), geometry=districts_BGL))


#10 districts Carinthia
districts_Carinthia$geometry[1]->"Feldkirchen"
id_feldkirchen<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Feldkirchen`[[1]][[1]][,1], `Feldkirchen`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_feldkirchen]<-"Feldkirchen"

districts_Carinthia$geometry[2]->"Hermagor"
id_hermagor<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Hermagor`[[1]][[1]][,1], `Hermagor`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_hermagor]<-"Hermagor"

districts_Carinthia$geometry[3]->"Klagenfurt-Land"
id_klagenfurt_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Klagenfurt-Land`[[1]][[1]][[1]][,1], `Klagenfurt-Land`[[1]][[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_klagenfurt_land]<-"Klagenfurt-Land"

districts_Carinthia$geometry[4]->"Klagenfurt"
id_klagenfurt<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Klagenfurt`[[1]][[1]][,1], `Klagenfurt`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_klagenfurt]<-"Klagenfurt"

districts_Carinthia$geometry[5]->"St Veit an der Glan"
id_st_veit<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `St Veit an der Glan`[[1]][[1]][,1], `St Veit an der Glan`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_st_veit]<-"St Veit an der Glan"

districts_Carinthia$geometry[6]->"Spittal an der Drau"
id_spittal<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Spittal an der Drau`[[1]][[1]][,1], `Spittal an der Drau`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_spittal]<-"Spittal an der Drau"

districts_Carinthia$geometry[7]->"Villach-Land"
id_villach_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Villach-Land`[[1]][[1]][,1], `Villach-Land`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_villach_land]<-"Villach-Land"

districts_Carinthia$geometry[8]->"Villach"
id_villach<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Villach`[[1]][[1]][,1], `Villach`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_villach]<-"Villach"

districts_Carinthia$geometry[9]->"Voelkermarkt"
id_voelkermarkt<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Voelkermarkt`[[1]][[1]][,1], `Voelkermarkt`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_voelkermarkt]<-"Voelkermarkt"

districts_Carinthia$geometry[10]->"Wolfsberg"
id_wolfsberg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Wolfsberg`[[1]][[1]][,1], `Wolfsberg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_wolfsberg]<-"Wolfsberg"

Carinthia_districts_df<-st_as_sf(data.frame(District=c("Feldkirchen","Hermagor", "Klagenfurt-Land", "Klagenfurt",  "St Veit an der Glan",  "Spittal an der Drau", "Villach-Land", "Villach", "Voelkermarkt", "Wolfsberg"), geometry=districts_Carinthia))

#24 districts Lower Austria
districts_Lower_Austria$geometry[1]->"Amstetten"
id_amstetten<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Amstetten`[[1]][[1]][[1]][,1], `Amstetten`[[1]][[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_amstetten]<-"Amstetten"

districts_Lower_Austria$geometry[2]->"Baden"
id_baden<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Baden`[[1]][[1]][,1], `Baden`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_baden]<-"Baden"

districts_Lower_Austria$geometry[3]->"Bruck an der Leitha"
id_bruck<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Bruck an der Leitha`[[1]][[1]][,1], `Bruck an der Leitha`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_bruck]<-"Bruck an der Leitha"

districts_Lower_Austria$geometry[4]->"Gaenserndorf"
id_gaenserndorf<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Gaenserndorf`[[1]][[1]][,1], `Gaenserndorf`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_gaenserndorf]<-"Gaenserndorf"

districts_Lower_Austria$geometry[5]->"Gmuend"
id_gmuend<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Gmuend`[[1]][[1]][,1], `Gmuend`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_gmuend]<-"Gmuend"

districts_Lower_Austria$geometry[6]->"Hollabrunn"
id_hollabrunn<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Hollabrunn`[[1]][[1]][,1], `Hollabrunn`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_hollabrunn]<-"Hollabrunn"

districts_Lower_Austria$geometry[7]->"Horn"
id_horn<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Horn`[[1]][[1]][,1], `Horn`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_horn]<-"Horn"

districts_Lower_Austria$geometry[8]->"Korneuburg"
id_korneuburg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Korneuburg`[[1]][[1]][[1]][,1], `Korneuburg`[[1]][[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_korneuburg]<-"Korneuburg"

districts_Lower_Austria$geometry[9]->"Krems"
id_krems<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Krems`[[1]][[1]][,1], `Krems`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_krems]<-"Krems"

districts_Lower_Austria$geometry[10]->"Krems-Land"
id_krems_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Krems-Land`[[1]][[1]][,1], `Krems-Land`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_krems_land]<-"Krems-Land"

districts_Lower_Austria$geometry[11]->"Lilienfeld"
id_lilienfeld<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Lilienfeld`[[1]][[1]][,1], `Lilienfeld`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_lilienfeld]<-"Lilienfeld"

districts_Lower_Austria$geometry[12]->"Melk"
id_melk<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Melk`[[1]][[1]][,1], `Melk`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_melk]<-"Melk"

districts_Lower_Austria$geometry[13]->"Mistelbach"
id_mistelbach<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Mistelbach`[[1]][[1]][,1], `Mistelbach`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_mistelbach]<-"Mistelbach"

districts_Lower_Austria$geometry[14]->"Moedling"
id_moedling<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Moedling`[[1]][[1]][,1], `Moedling`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_moedling]<-"Moedling"

districts_Lower_Austria$geometry[15]->"Neunkirchen"
id_neunkirchen<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Neunkirchen`[[1]][[1]][,1], `Neunkirchen`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_neunkirchen]<-"Neunkirchen"

districts_Lower_Austria$geometry[16]->"St Poelten-Land"

districts_Lower_Austria$geometry[17]->"St Poelten"
id_st_poelten<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `St Poelten`[[1]][[1]][,1], `St Poelten`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_st_poelten]<-"St Poelten"

districts_Lower_Austria$geometry[18]->"Scheibbs"
id_scheibbs<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Scheibbs`[[1]][[1]][,1], `Scheibbs`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_scheibbs]<-"Scheibbs"

districts_Lower_Austria$geometry[19]->"Tulln"
id_tulln<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Tulln`[[1]][[1]][,1], `Tulln`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_tulln]<-"Tulln"

districts_Lower_Austria$geometry[20]->"Waidhofen an der Thaya"
id_thaya<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Waidhofen an der Thaya`[[1]][[1]][,1], `Waidhofen an der Thaya`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_thaya]<-"Waidhofen an der Thaya"

districts_Lower_Austria$geometry[21]->"Waidhofen an der Ybbs"
id_ybbs<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Waidhofen an der Ybbs`[[1]][[1]][,1], `Waidhofen an der Ybbs`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_ybbs]<-"Waidhofen an der Ybbs"

districts_Lower_Austria$geometry[22]->"Wiener Neustadt-Land"
id_wiener_neustadt_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Wiener Neustadt-Land`[[1]][[1]][[1]][,1], `Wiener Neustadt-Land`[[1]][[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_wiener_neustadt_land]<-"Wiener Neustadt-Land"

districts_Lower_Austria$geometry[23]->"Wiener Neustadt"
id_wiener_neustadt<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Wiener Neustadt`[[1]][[1]][,1], `Wiener Neustadt`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_wiener_neustadt]<-"Wiener Neustadt"

districts_Lower_Austria$geometry[24]->"Zwettl"
id_zwettl<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Zwettl`[[1]][[1]][,1], `Zwettl`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_zwettl]<-"Zwettl"

Lower_Austria_districts_df<-st_as_sf(data.frame(District=c("Amstetten","Baden", "Bruck an der Leitha", "Gaenserndorf",  "Gmuend",  "Hollabrunn", "Horn", "Korneuburg", "Krems", "Krems-Land","Lilienfeld", "Melk","Mistelbach","Moedling", "Neunkirchen", "St Poelten-Land", "St Poelten", "Scheibbs", "Tulln", "Waidhofen an der Thaya", "Waidhofen an der Ybbs", "Wiener Neustadt-Land", "Wiener Neustadt", "Zwettl"), geometry=districts_Lower_Austria))

#18 districts Upper Austria
districts_Upper_Austria$geometry[1]->"Braunau am Inn"
id_braunau<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Braunau am Inn`[[1]][[1]][,1], `Braunau am Inn`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_braunau]<-"Braunau am Inn"

districts_Upper_Austria$geometry[2]->"Eferding"
id_eferding<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Eferding`[[1]][[1]][,1], `Eferding`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_eferding]<-"Eferding"

districts_Upper_Austria$geometry[3]->"Freistadt"
id_freistadt<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Freistadt`[[1]][[1]][,1], `Freistadt`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_freistadt]<-"Freistadt"

districts_Upper_Austria$geometry[4]->"Gmunden"
id_gmunden<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Gmunden`[[1]][[1]][,1], `Gmunden`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_gmunden]<-"Gmunden"

districts_Upper_Austria$geometry[5]->"Grieskirchen"
id_grieskirchen<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Grieskirchen`[[1]][[1]][,1], `Grieskirchen`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_grieskirchen]<-"Grieskirchen"

districts_Upper_Austria$geometry[6]->"Kirchdorf an der Krems"
id_kirchdorf<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Kirchdorf an der Krems`[[1]][[1]][,1], `Kirchdorf an der Krems`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_kirchdorf]<-"Kirchdorf an der Krems"

districts_Upper_Austria$geometry[7]->"Linz-Land"
id_linz_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Linz-Land`[[1]][[1]][,1], `Linz-Land`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_linz_land]<-"Linz-Land"

districts_Upper_Austria$geometry[8]->"Linz"
id_linz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Linz`[[1]][[1]][[1]][,1], `Linz`[[1]][[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_linz]<-"Linz"

districts_Upper_Austria$geometry[9]->"Perg"
id_perg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Perg`[[1]][[1]][,1], `Perg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_perg]<-"Perg"

districts_Upper_Austria$geometry[10]->"Ried im Innkreis"
id_ried<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Ried im Innkreis`[[1]][[1]][,1], `Ried im Innkreis`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_ried]<-"Ried im Innkreis"

districts_Upper_Austria$geometry[11]->"Rohrbach"
id_rohrbach<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Rohrbach`[[1]][[1]][,1], `Rohrbach`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_rohrbach]<-"Rohrbach"

districts_Upper_Austria$geometry[12]->"Schaerding"
id_schaerding<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Schaerding`[[1]][[1]][,1], `Schaerding`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_schaerding]<-"Schaerding"

districts_Upper_Austria$geometry[13]->"Steyr-Land"
id_steyr_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Steyr-Land`[[1]][[1]][,1], `Steyr-Land`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_steyr_land]<-"Steyr-Land"

districts_Upper_Austria$geometry[14]->"Steyr"
id_steyr<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Steyr`[[1]][[1]][,1], `Steyr`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_steyr]<-"Steyr"

districts_Upper_Austria$geometry[15]->"Urfahr-Umgebung"
id_urfahr_umgebung<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Urfahr-Umgebung`[[1]][[1]][,1], `Urfahr-Umgebung`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_urfahr_umgebung]<-"Urfahr-Umgebung"

districts_Upper_Austria$geometry[16]->"Voecklabruck"
id_voecklabruck<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Voecklabruck`[[1]][[1]][,1], `Voecklabruck`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_voecklabruck]<-"Voecklabruck"

districts_Upper_Austria$geometry[17]->"Wels-Land"
id_wels_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Wels-Land`[[1]][[1]][,1], `Wels-Land`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_wels_land]<-"Wels-Land"

districts_Upper_Austria$geometry[18]->"Wels"
id_wels<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Wels`[[1]][[1]][,1], `Wels`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_wels]<-"Wels"

Upper_Austria_districts_df<-st_as_sf(data.frame(District=c("Braunau am Inn","Eferding", "Freistadt",  "Gmunden",  "Grieskirchen", "Kirchdorf an der Krems", "Linz-Land", "Linz", "Perg","Ried im Innkreis", "Rohrbach","Schaerding","Steyr-Land", "Steyr", "Urfahr-Umgebung", "Voecklabruck", "Wels-Land", "Wels"), geometry=districts_Upper_Austria))

#6 districts Salzburg
districts_Salzburg$geometry[1]->"Hallein"
id_hallein<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Hallein`[[1]][[1]][,1], `Hallein`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_hallein]<-"Hallein"

districts_Salzburg$geometry[2]->"Salzburg-Umgebung"
id_salzburg_umgebung<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Salzburg-Umgebung`[[1]][[1]][,1], `Salzburg-Umgebung`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_salzburg_umgebung]<-"Salzburg-Umgebung"

districts_Salzburg$geometry[3]->"Salzburg"
id_salzburg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Salzburg`[[1]][[1]][,1], `Salzburg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_salzburg]<-"Salzburg"

districts_Salzburg$geometry[4]->"St Johann im Pongau"
id_pongau<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `St Johann im Pongau`[[1]][[1]][,1], `St Johann im Pongau`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_pongau]<-"St Johann im Pongau"

districts_Salzburg$geometry[5]->"Tamsweg"
id_tamsweg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Tamsweg`[[1]][[1]][,1], `Tamsweg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_tamsweg]<-"Tamsweg"

districts_Salzburg$geometry[6]->"Zell am See"
id_zell<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Zell am See`[[1]][[1]][,1], `Zell am See`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_zell]<-"Zell am See"

Salzburg_districts_df<-st_as_sf(data.frame(District=c("Hallein","Salzburg-Umgebung", "Salzburg",  "St Johann im Pongau",  "Tamsweg", "Zell am See"), geometry=districts_Salzburg))

#13 districts Styria
districts_Styria$geometry[1]->"Bruck-Muerzzuschlag"
id_bruck_muerz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Bruck-Muerzzuschlag`[[1]][[1]][,1], `Bruck-Muerzzuschlag`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_bruck_muerz]<-"Bruck-Muerzzuschlag"

districts_Styria$geometry[2]->"Deutschlandsberg"
id_deutschlandsberg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Deutschlandsberg`[[1]][[1]][,1], `Deutschlandsberg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_deutschlandsberg]<-"Deutschlandsberg"

districts_Styria$geometry[3]->"Graz-Umgebung"
id_graz_umgebung<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Graz-Umgebung`[[1]][[1]][,1], `Graz-Umgebung`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_graz_umgebung]<-"Graz-Umgebung"

districts_Styria$geometry[4]->"Graz"
id_graz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Graz`[[1]][[1]][,1], `Graz`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_graz]<-"Graz"

districts_Styria$geometry[5]->"Hartberg-Fuerstenfeld"
id_hartberg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Hartberg-Fuerstenfeld`[[1]][[1]][,1], `Hartberg-Fuerstenfeld`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_hartberg]<-"Hartberg-Fuerstenfeld"

districts_Styria$geometry[6]->"Leibnitz"
id_leibnitz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Leibnitz`[[1]][[1]][,1], `Leibnitz`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_leibnitz]<-"Leibnitz"

districts_Styria$geometry[7]->"Leoben"
id_leoben<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Leoben`[[1]][[1]][,1], `Leoben`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_leoben]<-"Leoben"

districts_Styria$geometry[8]->"Liezen"
id_liezen<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Liezen`[[1]][[1]][,1], `Liezen`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_liezen]<-"Liezen"

districts_Styria$geometry[9]->"Murau"
id_murau<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Murau`[[1]][[1]][,1], `Murau`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_murau]<-"Murau"

districts_Styria$geometry[10]->"Murtal"
id_murtal<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Murtal`[[1]][[1]][,1], `Murtal`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_murtal]<-"Murtal"

districts_Styria$geometry[11]->"Suedoststeiermark"
id_suedoststeiermark<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Suedoststeiermark`[[1]][[1]][,1], `Suedoststeiermark`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_suedoststeiermark]<-"Suedoststeiermark"

districts_Styria$geometry[12]->"Voitsberg"
id_voitsberg<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Voitsberg`[[1]][[1]][,1], `Voitsberg`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_voitsberg]<-"Voitsberg"

districts_Styria$geometry[13]->"Weiz"
id_weiz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Weiz`[[1]][[1]][,1], `Weiz`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_weiz]<-"Weiz"

Styria_districts_df<-st_as_sf(data.frame(District=c("Bruck-Muerzzuschlag","Deutschlandsberg", "Graz-Umgebung",  "Graz",  "Hartberg-Fuerstenfeld", "Leibnitz", "Leoben", "Liezen", "Murau", "Murtal", "Suedoststeiermark", "Voitsberg", "Weiz" ), geometry=districts_Styria))

#9 districts Tyrol
districts_Tyrol$geometry[1]->"Imst"
id_imst<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Imst`[[1]][[1]][,1], `Imst`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_imst]<-"Imst"

districts_Tyrol$geometry[2]->"Innsbruck-Land"
id_innsbruck_land<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Innsbruck-Land`[[1]][[1]][,1], `Innsbruck-Land`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_innsbruck_land]<-"Innsbruck-Land"

districts_Tyrol$geometry[3]->"Innsbruck"
id_innsbruck<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Innsbruck`[[1]][[1]][,1], `Innsbruck`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_innsbruck]<-"Innsbruck"

districts_Tyrol$geometry[4]->"Kitzbuehl"
id_kitzbuehl<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Kitzbuehl`[[1]][[1]][,1], `Kitzbuehl`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_kitzbuehl]<-"Kitzbuehl"

districts_Tyrol$geometry[5]->"Kufstein"
id_kufstein<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Kufstein`[[1]][[1]][,1], `Kufstein`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_kufstein]<-"Kufstein"

districts_Tyrol$geometry[6]->"Landeck"
id_landeck<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Landeck`[[1]][[1]][,1], `Landeck`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_landeck]<-"Landeck"

districts_Tyrol$geometry[7]->"Osttirol"
id_osttirol<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Osttirol`[[1]][[1]][,1], `Osttirol`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_osttirol]<-"Osttirol"

districts_Tyrol$geometry[8]->"Reutte"
id_reutte<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Reutte`[[1]][[1]][[1]][,1], `Reutte`[[1]][[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_reutte]<-"Reutte"

districts_Tyrol$geometry[9]->"Schwaz"
id_schwaz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Schwaz`[[1]][[1]][,1], `Schwaz`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_schwaz]<-"Schwaz"

Tyrol_districts_df<-st_as_sf(data.frame(District=c("Imst","Innsbruck-Land", "Innsbruck",  "Kitzbuehl",  "Kufstein", "Landeck", "Osttirol", "Reutte", "Schwaz" ), geometry=districts_Tyrol))

#4 districts Vorarlberg
districts_Vorarlberg$geometry[1]->"Bludenz"
id_bludenz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Bludenz`[[1]][[1]][,1], `Bludenz`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_bludenz]<-"Bludenz"

districts_Vorarlberg$geometry[2]->"Bregenz"
id_bregenz<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Bregenz`[[1]][[1]][,1], `Bregenz`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_bregenz]<-"Bregenz"

districts_Vorarlberg$geometry[3]->"Dornbirn"
id_dornbirn<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Dornbirn`[[1]][[1]][,1], `Dornbirn`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_dornbirn]<-"Dornbirn"

districts_Vorarlberg$geometry[4]->"Feldkirch"
id_feldkirch<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Feldkirch`[[1]][[1]][,1], `Feldkirch`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_feldkirch]<-"Feldkirch"

VBG_districts_df<-st_as_sf(data.frame(District=c("Bludenz","Bregenz", "Dornbirn",  "Feldkirch"), geometry=districts_Vorarlberg))

#1 district Vienna
districts_Vienna$geometry[1]->"Wien"
id_wien<-which(point.in.polygon(weather_data_all_dry_weeks_districts$Longitude, weather_data_all_dry_weeks_districts$Latitude, `Wien`[[1]][[1]][,1], `Wien`[[1]][[1]][,2])=="1") 
weather_data_all_dry_weeks_districts$District[id_wien]<-"Wien"

Vienna_districts_df<-st_as_sf(data.frame(District=c("Wien"), geometry=districts_Vienna))

#now the exeptions
weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Ferlach","District"]<-"Klagenfurt-Land"
weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Wachtberg bei Steyr","District"]<-"Amstetten"
weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Pörtschach","District"]<-"Klagenfurt-Land"
weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Loibl Tunnel","District"]<-"Klagenfurt-Land"
weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Amstetten","District"]<-"Amstetten"
weather_data_all_dry_weeks_districts[weather_data_all_dry_weeks_districts$Station=="Rust","District"]<-"Rust"

Austrian_districts_combined<-rbind(BGL_districts_df, Carinthia_districts_df, Upper_Austria_districts_df, Lower_Austria_districts_df,
                                   Tyrol_districts_df, Salzburg_districts_df, Styria_districts_df, VBG_districts_df, Vienna_districts_df)


save(weather_data_all_dry_weeks_districts, Austrian_districts_combined, file="01_next_step_kNN.R")


##################### second step: continue after kNN is done #######################
load("01_kNN_done.R")

#only date missing
pMiss<-function(x){sum(is.na(x))/length(x)*100}
apply(weather_data_all_dry_weeks_districts_kNN, 2, pMiss)

weather_data_all_dry_weeks_districts_kNN<-weather_data_all_dry_weeks_districts_kNN[,c(1,27,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]

#order data
weather_data_all_dry_weeks_districts_kNN<-weather_data_all_dry_weeks_districts_kNN[order(weather_data_all_dry_weeks_districts_kNN$District),]
weather_data_all_dry_weeks_districts_kNN<-weather_data_all_dry_weeks_districts_kNN[order(weather_data_all_dry_weeks_districts_kNN$Week),]
weather_data_all_dry_weeks_districts_kNN<-weather_data_all_dry_weeks_districts_kNN[order(weather_data_all_dry_weeks_districts_kNN$Year),]

#fill in missing date on value in row before
library(zoo)
weather_data_all_dry_weeks_districts_kNN$Date<-na.locf(weather_data_all_dry_weeks_districts_kNN$Date)

#53*3+52*15=939 weeks
weather_data_all_dry_weeks_districts_kNN$Time<-dense_rank(weather_data_all_dry_weeks_districts_kNN$Date)


weather_data_all_dry_weeks_districts.aggregated<-aggregate(weather_data_all_dry_weeks_districts_kNN, by=list(weather_data_all_dry_weeks_districts_kNN$District, weather_data_all_dry_weeks_districts_kNN$Time), FUN=mean, na.rm=T) #drop means that any groups with zero counts are removed

colnames(weather_data_all_dry_weeks_districts.aggregated)[1]<-"District"
colnames(weather_data_all_dry_weeks_districts.aggregated)[2]<-"Time"

weather_data_all_dry_weeks_districts.aggregated<-weather_data_all_dry_weeks_districts.aggregated[,c(1,2,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)]


#round columns
weather_data_all_dry_weeks_districts.aggregated<-weather_data_all_dry_weeks_districts.aggregated %>% mutate(across(where(is.numeric), round, digits=3))


Austrian_districts_aggregated<-merge(Austrian_districts_combined, weather_data_all_dry_weeks_districts.aggregated, by="District")


setwd("~/Documents/Mortality & heat")
save(Austrian_districts_aggregated,weather_data_all_dry_weeks_districts.aggregated,  file="01_aggregated_temp_by_district.R")

