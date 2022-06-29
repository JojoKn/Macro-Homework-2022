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

pwt101<-pwt100[,c("countrycode", "country", "year", "cgdpe", "cgdpo", "cn", "emp", "pop")]
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

#creating dummyvariables for Oil, Africa, and America

oilexporters<-c("CAN", "IRQ", "KAZ", "KWT", "NGA", "NOR", "RUS", "SAU", "ARE", "USA")
africancountries<-c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM",
                    "COD", "COG", "CIV", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB",
                    "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT",
                    "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "REU", "RWA", "STP", "SEN",
                    "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO", "TUN", "UGA", "ESH",
                    "ZMB", "ZWE")
laamericancountries<-c("ARG", "BLZ", "BOL", "BRA", "CHL", "COL", "CRI", "ECU", "SLV", "GUF",
                       "GTM", "GUY", "HND", "MEX", "NIC", "PAN", "PRY", "PER", "SUR", "URY", "VEN")


total$oil<-ifelse(total$ISOCode3%in%oilexporters, "1", "0")
total$africa<-ifelse(total$ISOCode3%in%africancountries, "1", "0")
total$laamer<-ifelse(total$ISOCode3%in%laamericancountries, "1", "0")


####a####

#Filter out relevant years for task a
years_a<-c(1965, 1985)
total_a<-total[total$year%in%years_a,]
View(total_a)

#Omit NAs
total_a_clean<-na.omit(total_a)

#Keep duplicated rows only
total_a_clean<-total_a_clean[ ave(1:nrow(total_a_clean), 
                                  total_a_clean$ISOCode3, FUN=length) > 1 , ]

#Check: Length should be half of the number of rows from the data frame
length(unique(total_a_clean$ISOCode3))

#Calculating log differences

data65<-subset(total_a_clean, year==1965)
data85<-subset(total_a_clean, year==1985)

#Get per capita income
data65$cgdpo<-data65$cgdpo/data65$pop
data85$cgdpo<-data85$cgdpo/data85$pop


dy<-log(data85$cgdpo/data65$cgdpo)
dK<-log(data85$cn/data65$cn)
dL<-log(data85$emp/data65$emp)
dH<-log(data85$Years/data65$Years)
logY0<-log(data65$cgdpo)

model1<-summary(lm(dY~dK+dL+dH))

model2<-summary(lm(dY~dK+dL+dH+logY0))

model3<-summary(lm(dY~dK+dL+dH+logY0+data65$oil))

model4<-summary(lm(dY~dK+dL+dH+logY0+data65$africa+data65$laamer))



