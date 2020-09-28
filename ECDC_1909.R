rm(list=ls())
library(plotly)
library(dplyr)
library(reshape)
require(ggplot2)
library(readxl)
library(httr)
library(RCurl)

#######################3
setwd("C:/Users/E6530/Documents/R/COVID")
filePath="~/Documents/Research/Github Local/SEIR_COVID19_Dev/COVID19seir"

# ----------- Load country-level data from the ECDC database --------------------------------- 


ecdc = read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

# add and reorder new columns

db=subset(ecdc,select=-c(day,month,year,countryterritoryCode)) #get rid of some variables
db$dateRep=as.Date(db$date,"%d/%m/%Y") # put in date data type
#data$dateRep<-as.Date(data$dateRep, format="%d/%m/%Y")
db<-db[as.Date(db$dateRep)>as.Date("2020-03-01"),]
db$cumCases<-db$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000
#pikkumaat pois
db<-db[db$popData2019>1500000,]
#####################################

# ----------------------- Repeat wth regular plotting------------------------ 

#RAJAUKSET
db2<-db[as.Date(db$dateRep)>as.Date("2020-08-01"),]
db3<-db2[(db2$cumCases>15 &db2$cumCases<25),]
db3<-db3[db3$continentExp=="Europe",]

tmp<-unique(db3$countriesAndTerritories)

db3<-db2[db2$countriesAndTerritories %in% tmp,]
         
#####################################

otos<-sample(tmp,5)
otos<-db3[db3$countriesAndTerritories %in% otos,]

ggplot(otos, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                colour=factor(countriesAndTerritories))) + 
  geom_line()+
  geom_smooth(method="gam", se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=25, linetype="dashed", color = "red")+
  geom_hline(yintercept=10, linetype="dashed", color = "blue")+
  
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")









#################################################

singleC<-db[db$countriesAndTerritories=="Netherlands",]

###############
ggplot(data=singleC, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                         colour=factor(countriesAndTerritories))) +
  geom_line(colour="black")+
  geom_smooth(method="gam")+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=25, linetype="dashed", color = "red")+
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")

#################################

#################################################

singleC2<-db[db$countriesAndTerritories=="Slovakia",]

###############
ggplot(data=singleC2, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                         colour=factor(countriesAndTerritories))) +
  geom_line(colour="black")+
  geom_smooth(method="gam")+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=25, linetype="dashed", color = "red")+
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")

#################################
maat<-c("Netherlands", "Denmark", "Germany", "Finland", "France")
#Elokuun alusta
#poiminta<-db3[db3$countriesAndTerritories %in% maat,]
#Koko aika
poiminta<-db[db$countriesAndTerritories %in% maat,]

ggplot(poiminta, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                 colour=factor(countriesAndTerritories))) + 
  geom_line()+
  geom_smooth(method="gam", se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=25, linetype="dashed", color = "red")+
#  geom_hline(yintercept=10, linetype="dashed", color = "blue")+
  
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")

#######################################
ggplot(poiminta, aes(dateRep, deaths, 
                     colour=factor(countriesAndTerritories))) + 
  geom_line()+
  geom_smooth(method="gam", se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=25, linetype="dashed", color = "red")+
  #  geom_hline(yintercept=10, linetype="dashed", color = "blue")+
  #ylim(0,250)+
  labs(x = "", y= "Kuolemat", colour="Maa")

