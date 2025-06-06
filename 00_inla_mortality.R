library(spdep)
library(spData)
library(INLA)
library(Matrix)
library(sp)
library(sf)
#library(timetk)
library(gtools) #for mixedsort
library(ggplot2)
library(dplyr)
#library(tidyverse)
library(matrixcalc)
library(viridis)
library(viridisLite)
library(tidyr)

#filled up with zero entries in case of missing data
load("01_mortality_data_districts_all_years.R")
load("01_aggregated_temp_by_district.R")

#from here: https://github.com/spatialstatisticsupna/Discontinuities_and_Clusters_article/blob/master/R/kronecker_nullspace.R
source("kronecker_nullspace.R")

######### first: create adjacency matrix for area data ######

districts<-read_sf("gadm41_AUT_2.shp",)
Austria_districts<-poly2nb(districts)
nb2INLA("Austria_districts.graph",Austria_districts)
Austria.adj_districts<-paste(getwd(), "/Austria_districts.graph", sep="")
inla.setOption(scale.model.default=F)

H<-inla.read.graph(file="Austria_districts.graph")
image(inla.graph2matrix(H))

############second: add time variable to data district and merge with aggregated data######################

data_district<-data_district[order(data_district$Year),]
data_district<-data_district %>% arrange(Date) %>% mutate(Time=dense_rank(Date))

data_district_no_week<-subset(data_district, select=-c(Week,Date))

Austrian_districts_aggregated<- Austrian_districts_aggregated[order(Austrian_districts_aggregated$Year),]
data_district<- data_district[order(data_district$Year),]

df_CAR<-merge( Austrian_districts_aggregated, data_district_no_week, by=c("District","Year","Time"))

df_CAR<-st_as_sf(df_CAR)

df_CAR<- df_CAR[order(df_CAR$Time),]

##### third: implement model ######

districts<-length(unique(df_CAR$District))  #number of areas
timepoints<-length(unique(df_CAR$Time))   #number of weeks
ages<-length(unique(df_CAR$Age))   #number of age groups

df_CAR$ID<-as.integer(factor(df_CAR$District, levels=unique(df_CAR$District)))

#create specific dataframe for CAR model with variables used
new_df<-data.frame(Deaths=df_CAR$Deaths, Gender=df_CAR$Gender,Age=df_CAR$Age, Date=df_CAR$Date,#Death_rate=df_CAR$Death_rate,
                   District=df_CAR$District, Population=df_CAR$Population,
                   ID=df_CAR$ID, Time= df_CAR$Time, Year=df_CAR$Year, Week=df_CAR$Week,
                   Temp_min_mean=df_CAR$Temp_min_mean,
                   Temp_max_mean=df_CAR$Temp_max_mean,
                   Temp_spread_day=df_CAR$Temp_spread_day,
                   Humidity_mean=df_CAR$Humidity_mean,
                   Last_week_was_dry=df_CAR$Last_week_was_dry,
                   Super_cold_week=df_CAR$Super_cold_week,
                   Cold_week=df_CAR$Cold_week,
                   Hot_week=df_CAR$Hot_week, 
                   Tropical_week=df_CAR$Tropical_week,
                   Mild_week=df_CAR$Mild_week, 
                   Increased_risk_humidity=df_CAR$Increased_risk_humidity,
                   Serious_risk_humidity=df_CAR$Serious_risk_humidity,
                   Strong_discomfort_humidity=df_CAR$Strong_discomfort_humidity,
                   Severe_malaise_humidity=df_CAR$Severe_malaise_humidity,
                   Elevation=df_CAR$Elevation)

#new_df$Death_rate<-as.numeric(gsub(",", ".", new_df$Death_rate))
new_df$Age<-factor(new_df$Age,levels=c("0-64", "65-74", "75-84", "85+"))

new_df_male<-new_df[which(new_df$Gender=="Male"),]
new_df_female<-new_df[which(new_df$Gender=="Female"),]

#sort Age
new_df_male<-new_df_male[mixedorder(new_df_male$Age),]
new_df_female<-new_df_female[mixedorder(new_df_female$Age),]

new_df_male<-new_df_male[order(new_df_male$District),]
new_df_female<-new_df_female[order(new_df_female$District),]

#order by week and start indexing from the beginning for next week
new_df_male<-new_df_male[order(new_df_male$Time),]
new_df_female<-new_df_female[order(new_df_female$Time),]

new_df_male$ID.prov.age=rep(seq(1,districts*ages),timepoints)# *genders)  
new_df_female$ID.prov.age=rep(seq(1,districts*ages),timepoints)# *genders)  

#every province gets everytime a new index for new week
new_df_male$ID.prov.week=rep(seq(1,districts*timepoints),each=ages)#, times=2)
new_df_female$ID.prov.week=rep(seq(1,districts*timepoints),each=ages)#, times=2)

#20 age groups and indexing new for new week
new_df_male$ID.age.week=as.vector(apply(matrix(seq(1,ages*timepoints),ages,timepoints),2, function(x) rep(x,districts)))
new_df_female$ID.age.week=as.vector(apply(matrix(seq(1,ages*timepoints),ages,timepoints),2, function(x) rep(x,districts)))

new_df<-rbind(new_df_male,new_df_female)

#define covariance of CAR prior by Leroux et.al 2000
Q_space<-matrix(0, H$n, H$n) #neighbourhood matrix
for(i in 1: H$n){
  Q_space[i,i]<-H$nnbs[[i]]
  Q_space[i, H$nbs[[i]]]<--1
}

#define R.Leroux matrix this way or alternatively diag(n)-Qs
R.Leroux<-diag(dim(Q_space)[1])-Q_space

#define structure matrices for time and age
D1<-diff(diag(ages),differences=1)   #first order diff. matrix for age
Q_age<-t(D1) %*% D1    #structure matrix for age random effect

D2<-diff(diag(timepoints),differences=1)   #first order diff. matrix for time
Q_time<-t(D2) %*% D2    #structure matrix for time random effect

null.space<-kronecker.null.space(Q_time,Q_space) # structure matrix for space-time interaction
R.st<-null.space[[1]]
A_delta.st<-as.matrix(null.space[[2]])   

null.space<-kronecker.null.space(Q_space,Q_age) # structure matrix for space-age interaction
R.se<-null.space[[1]]
A_delta.se<-as.matrix(null.space[[2]]) 

null.space<-kronecker.null.space(Q_age, Q_time) # structure matrix for time-age interaction
R.te<-null.space[[1]]
A_delta.te<-as.matrix(null.space[[2]])

#implemented according to paper: identifiability constraints affect INLA results by Goicoa et al 2018
formula4_mortality_CAR<-Deaths ~ scale(Temp_min_mean)+scale(Temp_max_mean)+
  scale(Humidity_mean)+  Last_week_was_dry+
  Hot_week+  Cold_week+Super_cold_week+
  Elevation+  Mild_week+
  Increased_risk_humidity+ Serious_risk_humidity+
  Strong_discomfort_humidity +Severe_malaise_humidity+
  f(ID, model="generic1", Cmatrix=R.Leroux, constr=T,hyper=list(prec=list(prior="loggamma", param=c(1,0.01)),
                                                                beta=list(prior="logitbeta",param=c(1,1))))+
  f(Time, model="rw1", constr=T)+f(Age, model="rw1", constr=T)+#, scale.model = T)+
  f(ID.prov.age, model="generic0",Cmatrix=R.se, constr=T, extraconstr = list(A=A_delta.se, e=rep(0, dim(A_delta.se)[1])))+
  f(ID.prov.week, model="generic0", Cmatrix=R.st, constr=T,  extraconstr = list(A=A_delta.st, e=rep(0, dim(A_delta.st)[1])))+
  f(ID.age.week, model="generic0", Cmatrix=R.te, constr=T,  extraconstr = list(A=A_delta.te, e=rep(0, dim(A_delta.te)[1])))


result4_mortality_CAR_female<-inla(formula4_mortality_CAR, family="poisson", control.family = list(link='log'), 
                                            data=new_df_female, E=Population,
                                            control.compute = list(dic=T,   openmp.strategy="pardiso"), 
                                            control.predictor=list(compute=T),
                                            control.inla=list(strategy="gaussian", int.strategy="eb"),
                                            verbose=T)  


save(result4_mortality_CAR_male,   file="01_male_results_all_years.R")
save(result4_mortality_CAR_female, file="01_female_results_all_years.R")
