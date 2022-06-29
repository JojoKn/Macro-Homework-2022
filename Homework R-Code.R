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


dya<-log(data85$cgdpo/data65$cgdpo)
dKa<-log(data85$cn/data65$cn)
dLa<-log(data85$emp/data65$emp)
dHa<-log(data85$Years/data65$Years)
logY0a<-log(data65$cgdpo)

model1a<-summary(lm(dya~dKa+dLa+dHa))

model2a<-summary(lm(dya~dKa+dLa+dHa+logY0a))

model3a<-summary(lm(dya~dKa+dLa+dHa+logY0a+data65$oil))

model4a<-summary(lm(dya~dKa+dLa+dHa+logY0a+data65$africa+data65$laamer))

####b####

years_b<-c(1990, 2015)
total_b<-total[total$year%in%years_b,]

#Omit NAs
total_b_clean<-na.omit(total_b)

#Keep duplicated rows only
total_b_clean<-total_b_clean[ ave(1:nrow(total_b_clean), 
                                  total_b_clean$ISOCode3, FUN=length) > 1 , ]

#Check: Length should be half of the number of rows from the data frame
length(unique(total_b_clean$ISOCode3))

#Calculating log differences

data90<-subset(total_b_clean, year==1990)
data15<-subset(total_b_clean, year==2015)

#Get per capita income
data90$cgdpo<-data90$cgdpo/data90$pop
data15$cgdpo<-data15$cgdpo/data15$pop


dyb<-log(data15$cgdpo/data90$cgdpo)
dKb<-log(data15$cn/data90$cn)
dLb<-log(data15$emp/data90$emp)
dHb<-log(data15$Years/data90$Years)
logY0b<-log(data90$cgdpo)

model1b<-summary(lm(dyb~dKb+dLb+dHb))

model2b<-summary(lm(dyb~dKb+dLb+dHb+logY0b))

model3b<-summary(lm(dyb~dKb+dLb+dHb+logY0b+data90$oil))

model4b<-summary(lm(dyb~dKb+dLb+dHb+logY0b+data90$africa+data90$laamer))


