####Homework 2022####

####Transformations####
#Importing the Datasets
library(readxl)
pwt100 <- read_excel("pwt100.xlsx", sheet = "Data")
pwt100<-as.data.frame(pwt100)
str(pwt100)

countrycodes <- read_excel("Countrycodes.xlsx")

library(readr)
wcde_data <- read_csv("wcde_data.csv", col_types = cols(Year = col_character()), skip = 8)
wcde_data<-as.data.frame(wcde_data)
str(wcde_data)

View(pwt100)
View(wcde_data)
View(countrycodes)

library(tidyverse)
library(dplyr)

#Duplicate for messing around
wcde_data1<-wcde_data

#Adjusting countrycodes for wcde


wcde_data1$ISOCode<-ifelse(wcde_data1$ISOCode<10, 
                           paste("00",wcde_data1$ISOCode, sep=""), wcde_data1$ISOCode)
wcde_data1$ISOCode<-ifelse(wcde_data1$ISOCode%in%c(10:99), 
                           paste("0",wcde_data1$ISOCode, sep=""), wcde_data1$ISOCode)
countrycodes$Numeric<-ifelse(countrycodes$Numeric<10, 
                             paste("00",countrycodes$Numeric, sep=""), countrycodes$Numeric)
countrycodes$Numeric<-ifelse(countrycodes$Numeric%in%c(10:99), 
                             paste("0",countrycodes$Numeric, sep=""), countrycodes$Numeric)
countrycodes<-dplyr::rename(countrycodes, ISOCode=Numeric)
countrycodes<-dplyr::rename(countrycodes, ISOCode3=`Alpha-3 code`)

wcde_data2<-merge(wcde_data1, countrycodes, by="ISOCode")
wcde_data2<-wcde_data2[,c("Area", "Year", "Years", "ISOCode3")]

#Selecting the (hopefully?) relevant columns 

pwt101<-pwt100[,c("countrycode", "country", "year", "cgdpe", "cgdpo", "cn", "emp")]
View(pwt101)

#renaming columns from Wittgensteincenter
wcde_data2<-dplyr::rename(wcde_data2, country=Area)
wcde_data2<-dplyr::rename(wcde_data2, year=Year)

#As the data from the Wittgensteincenter is in 5 year intervals, adjust PWT
years_wcde<-c(1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 
         2000, 2005, 2010, 2015)
pwt101<-pwt101[pwt101$year%in%years_wcde,]

#Merging the datasets
pwt101<-dplyr::rename(pwt101, ISOCode3=countrycode)
total<-merge(wcde_data2, pwt101, by=c("ISOCode3", "year"))

####a####

years_a<-c(1960, 1985)
total_a<-total[total$year%in%years_a,]









