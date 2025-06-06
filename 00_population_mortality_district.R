library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

#data can be freely downloaded from the STATcube

################# first: population data from 2002-2023 on district level ############
#read in excel sheet for BGL (9 districts)
pop_BGL_male<-read_excel("Population district BGL male.xlsx")
pop_BGL_female<-read_excel("Population district BGL female.xlsx")

pop_BGL_male<-pop_BGL_male[,c(1,2,8,11,14,19)]
pop_BGL_female<-pop_BGL_female[,c(1,2,8,11,14,19)]

colnames(pop_BGL_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_BGL_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")


pop_BGL_male<-pop_BGL_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_BGL_female<-pop_BGL_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))


#read in excel sheet for Carinthia (10 districts)
pop_Carinthia_male<-read_excel("Population district Carinthia male.xlsx")
pop_Carinthia_female<-read_excel("Population district Carinthia female.xlsx")

pop_Carinthia_male<-pop_Carinthia_male[,c(1,2,8,11,14,19)]
pop_Carinthia_female<-pop_Carinthia_female[,c(1,2,8,11,14,19)]

colnames(pop_Carinthia_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_Carinthia_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_Carinthia_male<-pop_Carinthia_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_Carinthia_female<-pop_Carinthia_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))


#read in excel sheet for SBG (6 districts)
pop_SBG_male<-read_excel("Population district SBG male.xlsx")
pop_SBG_female<-read_excel("Population district SBG female.xlsx")

pop_SBG_male<-pop_SBG_male[,c(1,2,8,11,14,19)]
pop_SBG_female<-pop_SBG_female[,c(1,2,8,11,14,19)]

colnames(pop_SBG_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_SBG_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_SBG_male<-pop_SBG_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_SBG_female<-pop_SBG_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))


#read in excel sheet for Upper Austria (18 districts)
pop_UA_male<-read_excel("Population district UA male.xlsx")
pop_UA_female<-read_excel("Population district UA female.xlsx")

pop_UA_male<-pop_UA_male[,c(1,2,8,11,14,19)]
pop_UA_female<-pop_UA_female[,c(1,2,8,11,14,19)]

colnames(pop_UA_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_UA_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_UA_male<-pop_UA_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_UA_female<-pop_UA_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))

#read in excel sheet for Vorarlberg (4 districts)
pop_VBG_male<-read_excel("Population district VBG male.xlsx")
pop_VBG_female<-read_excel("Population district VBG female.xlsx")

pop_VBG_male<-pop_VBG_male[,c(1,2,8,11,14,19)]
pop_VBG_female<-pop_VBG_female[,c(1,2,8,11,14,19)]

colnames(pop_VBG_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_VBG_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_VBG_male<-pop_VBG_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_VBG_female<-pop_VBG_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))

#read in excel sheet for Vienna (1 district)
pop_Vienna_male<-read_excel("Population district Vienna male.xlsx")
pop_Vienna_female<-read_excel("Population district Vienna female.xlsx")

pop_Vienna_male<-pop_Vienna_male[,c(1,2,8,11,14,19)]
pop_Vienna_female<-pop_Vienna_female[,c(1,2,8,11,14,19)]

colnames(pop_Vienna_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_Vienna_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_Vienna_male<-pop_Vienna_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_Vienna_female<-pop_Vienna_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))


#read in excel sheet for Tyrol (9 districts)
pop_Tyrol_male<-read_excel("Population district Tyrol male.xlsx")
pop_Tyrol_female<-read_excel("Population district Tyrol female.xlsx")

pop_Tyrol_male<-pop_Tyrol_male[,c(1,2,8,11,14,19)]
pop_Tyrol_female<-pop_Tyrol_female[,c(1,2,8,11,14,19)]

colnames(pop_Tyrol_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_Tyrol_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")


pop_Tyrol_male<-pop_Tyrol_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_Tyrol_female<-pop_Tyrol_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))


#read in excel sheet for Lower Austria 1, (24 districts)
pop_LA_male<-read_excel("Population district LA male.xlsx")
pop_LA_female<-read_excel("Population district LA female.xlsx")

pop_LA_male<-pop_LA_male[,c(1,2,8,11,14,19)]
pop_LA_female<-pop_LA_female[,c(1,2,8,11,14,19)]


colnames(pop_LA_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_LA_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_LA_male<-pop_LA_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_LA_female<-pop_LA_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))

#read in Styria 1, all districts fine (13 districts)
pop_Styria_male<-read_excel("Population district Styria male.xlsx")
pop_Styria_female<-read_excel("Population district Styria female.xlsx")

pop_Styria_male<-pop_Styria_male[,c(1,2,8,11,14,19)]
pop_Styria_female<-pop_Styria_female[,c(1,2,8,11,14,19)]

colnames(pop_Styria_male)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")
colnames(pop_Styria_female)<-c("Year", "District", "0-64", "65-74", "75-84", "85+")

pop_Styria_male<-pop_Styria_male %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))
pop_Styria_female<-pop_Styria_female %>% fill(Year, .direction="down") %>% mutate(Year=as.integer(Year))


#summarise populations
pop_districts_male<-bind_rows(pop_BGL_male, pop_Carinthia_male, pop_Vienna_male, pop_VBG_male, pop_LA_male,
                              pop_UA_male, pop_Styria_male, pop_Tyrol_male, pop_SBG_male)

pop_districts_female<-bind_rows(pop_BGL_female, pop_Carinthia_female, pop_Vienna_female, pop_VBG_female,
                                pop_LA_female, pop_UA_female, pop_Styria_female, pop_Tyrol_female, pop_SBG_female)


save(pop_districts_male, pop_districts_female, file="01_population_districts.R")


################# second: paid mortality data from 2000-2023 on district level ############
data_district<-read.csv(file="GESTORBENE_abKW200001_bisKW202352_AT_Daten.csv",sep=";", header=TRUE)

data_district$Jahr<-as.integer(substr(data_district$Todeswoche,1,4))
data_district$Todeswoche<-as.integer(substr(data_district$Todeswoche,5,6))

data_district<-  mutate(data_district,GESCHL=case_when(
  GESCHL==1 ~"Male",
  GESCHL==2 ~"Female",
  TRUE ~ as.character(GESCHL)
))

data_district$GESCHL<-as.factor(data_district$GESCHL)

data_district<-mutate(data_district,ALTER4GR=case_when(
  ALTER4GR==1~"0-64",
  ALTER4GR==2~"65-74",
  ALTER4GR==3~"75-84",
  ALTER4GR==4~"85+",
  TRUE~ as.character(ALTER4GR)
))

data_district<-mutate(data_district, POLBEZ=case_when(
  POLBEZ==101~"Eisenstadt",
  POLBEZ==102~"Rust",
  POLBEZ==103~"Eisenstadt-Umgebung",
  POLBEZ==104~"Guessing",
  POLBEZ==105~"Jennersdorf",
  POLBEZ==106~"Mattersburg",
  POLBEZ==107~"Neusiedl am See",
  POLBEZ==108~"Oberpullendorf",
  POLBEZ==109~"Oberwart",
  POLBEZ==201~"Klagenfurt",
  POLBEZ==202~"Villach",
  POLBEZ==203~"Hermagor",
  POLBEZ==204~"Klagenfurt-Land",
  POLBEZ==205~"St Veit an der Glan",
  POLBEZ==206~"Spittal an der Drau",
  POLBEZ==207~"Villach-Land",
  POLBEZ==208~"Voelkermarkt",
  POLBEZ==209~"Wolfsberg",
  POLBEZ==210~"Feldkirchen",
  POLBEZ==301~"Krems",
  POLBEZ==302~"St Poelten",
  POLBEZ==303~"Waidhofen an der Ybbs",
  POLBEZ==304~"Wiener Neustadt",
  POLBEZ==305~"Amstetten",
  POLBEZ==306~"Baden",
  POLBEZ==307~"Bruck an der Leitha",
  POLBEZ==308~"Gaenserndorf",
  POLBEZ==309~"Gmuend",
  POLBEZ==310~"Hollabrunn",
  POLBEZ==311~"Horn",
  POLBEZ==312~"Korneuburg",
  POLBEZ==313~"Krems-Land",
  POLBEZ==314~"Lilienfeld",
  POLBEZ==315~"Melk",
  POLBEZ==316~"Mistelbach",
  POLBEZ==317~"Moedling",
  POLBEZ==318~"Neunkirchen",
  POLBEZ==319~"St Poelten-Land",
  POLBEZ==320~"Scheibbs",
  POLBEZ==321~"Tulln",
  POLBEZ==322~"Waidhofen an der Thaya",
  POLBEZ==323~"Wiener Neustadt-Land",
  POLBEZ==325~"Zwettl",
  POLBEZ==401~"Linz",
  POLBEZ==402~"Steyr",
  POLBEZ==403~"Wels",
  POLBEZ==404~"Braunau am Inn",
  POLBEZ==405~"Eferding",
  POLBEZ==406~"Freistadt",
  POLBEZ==407~"Gmunden",
  POLBEZ==408~"Grieskirchen",
  POLBEZ==409~"Kirchdorf an der Krems",
  POLBEZ==410~"Linz-Land",
  POLBEZ==411~"Perg",
  POLBEZ==412~"Ried im Innkreis",
  POLBEZ==413~"Rohrbach",
  POLBEZ==414~"Schaerding",
  POLBEZ==415~"Steyr-Land",
  POLBEZ==416~"Urfahr-Umgebung",
  POLBEZ==417~"Voecklabruck",
  POLBEZ==418~"Wels-Land",
  POLBEZ==501~"Salzburg",
  POLBEZ==502~"Hallein",
  POLBEZ==503~"Salzburg-Umgebung",
  POLBEZ==504~"St Johann im Pongau",
  POLBEZ==505~"Tamsweg",
  POLBEZ==506~"Zell am See",
  POLBEZ==601~"Graz",
  POLBEZ==603~"Deutschlandsberg",
  POLBEZ==606~"Graz-Umgebung",
  POLBEZ==610~"Leibnitz",
  POLBEZ==611~"Leoben",
  POLBEZ==612~"Liezen",
  POLBEZ==614~"Murau",
  POLBEZ==616~"Voitsberg",
  POLBEZ==617~"Weiz",
  POLBEZ==620~"Murtal",
  POLBEZ==621~"Bruck-Muerzzuschlag",
  POLBEZ==622~"Hartberg-Fuerstenfeld",
  POLBEZ==623~"Suedoststeiermark",
  POLBEZ==701~"Innsbruck",
  POLBEZ==702~"Imst",
  POLBEZ==703~"Innsbruck-Land",
  POLBEZ==704~"Kitzbuehl",
  POLBEZ==705~"Kufstein",
  POLBEZ==706~"Landeck",
  POLBEZ==707~"Osttirol",
  POLBEZ==708~"Reutte",
  POLBEZ==709~"Schwaz",
  POLBEZ==801~"Bludenz",
  POLBEZ==802~"Bregenz",
  POLBEZ==803~"Dornbirn",
  POLBEZ==804~"Feldkirch",
  POLBEZ==900~"Wien",
  TRUE~ as.character(POLBEZ)
))

colnames(data_district)<-c("Week", "District", "Age", "Gender", "Deaths", "Year")
data_district<-data_district[,c(1,6,2,4,3,5)]

data_district<-data_district[order(data_district$District),]
data_district<-data_district[order(data_district$Week),]

#we only have population data from 2002 onwards and we remove corona years
#data_district<-data_district[data_district$Year>="2002",]
data_district<-data_district[data_district$Year<"2020",]

Gender<-unique(data_district$Gender)
Age<-unique(data_district$Age)

Time_week<-seq(as.Date("2000-01-03"), as.Date("2019-12-31"), by="week")

Districts<-c( "Eisenstadt", "Rust", "Eisenstadt-Umgebung", "Guessing","Jennersdorf", "Mattersburg", "Neusiedl am See", "Oberpullendorf", 
              "Oberwart", "Klagenfurt", "Villach", "Hermagor", "Klagenfurt-Land","St Veit an der Glan", "Spittal an der Drau", "Villach-Land", 
              "Voelkermarkt", "Wolfsberg", "Feldkirchen", "Krems", "St Poelten", "Waidhofen an der Ybbs", "Wiener Neustadt", "Amstetten", "Baden",
              "Bruck an der Leitha", "Gaenserndorf", "Gmuend", "Hollabrunn", "Horn", "Korneuburg", "Krems-Land", "Lilienfeld", "Melk", "Mistelbach", 
              "Moedling", "Neunkirchen", "St Poelten-Land", "Scheibbs", "Tulln", "Waidhofen an der Thaya", "Wiener Neustadt-Land", "Zwettl",
              "Linz","Steyr","Wels", "Braunau am Inn", "Eferding", "Freistadt", "Gmunden", "Grieskirchen", "Kirchdorf an der Krems", "Linz-Land",
              "Perg", "Ried im Innkreis", "Rohrbach", "Schaerding", "Steyr-Land", "Urfahr-Umgebung", "Voecklabruck", "Wels-Land", "Salzburg", "Hallein",
              "Salzburg-Umgebung", "St Johann im Pongau", "Tamsweg", "Zell am See", "Graz", "Deutschlandsberg", "Graz-Umgebung", "Leibnitz", "Leoben",
              "Liezen", "Murau", "Voitsberg", "Weiz", "Murtal", "Bruck-Muerzzuschlag", "Hartberg-Fuerstenfeld", "Suedoststeiermark","Innsbruck","Imst",
              "Innsbruck-Land", "Kitzbuehl", "Kufstein","Landeck","Osttirol","Reutte","Schwaz", "Bludenz", "Bregenz","Dornbirn","Feldkirch","Wien")


df<-data.frame(Date=rep(Time_week, each=length(Gender)*length(Age)), District=rep(Districts, each=length(Time_week)*length(Gender)*length(Age)), Gender=Gender)
df<-df[order(df$Gender),]
df$Age<-Age

df$Year<-isoyear(df$Date)
df$Week<-isoweek(df$Date)  

data_merged<-left_join( df,data_district, by=c("District", "Week", "Year", "Gender","Age"))

#zero entries for filled up values
data_merged<-mutate(data_merged,Deaths=ifelse(is.na(Deaths),0,Deaths))

data_merged<-data_merged[data_merged$Year>="2002",]
data_district<-data_merged

################# third: join population and mortality data from 2002-2023 on district level ############
load("01_population_districts.R")

pop_districts_female<-pop_districts_female[order(pop_districts_female$Year),]
pop_districts_male<-pop_districts_male[order(pop_districts_male$Year),]

#pivoting from wide to long
pop_districts_female<-pop_districts_female %>% pivot_longer(cols=c("0-64", "65-74", "75-84", "85+"), names_to="Age", values_to = "Population")

pop_districts_female$Gender<-"Female"

pop_districts_male<-pop_districts_male %>% pivot_longer(cols=c("0-64", "65-74", "75-84", "85+"),
                                                        names_to="Age", values_to = "Population")

pop_districts_male$Gender<-"Male"

pop_districts<-rbind(pop_districts_female, pop_districts_male)

data_district<-merge(pop_districts, data_district, by=c("Year", "District", "Gender", "Age"))

setwd("~/Documents/Mortality & heat")
save(data_district, file="01_mortality_data_districts_all_years.R")

