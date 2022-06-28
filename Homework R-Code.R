###Homework 2022###

#Importing the Datasets
library(readxl)
pwt100 <- read_excel("pwt100.xlsx", sheet = "Data")
pwt100<-as.data.frame(pwt100)
str(pwt100)

library(readr)
wcde_data <- read_csv("wcde_data.csv", col_types = cols(Year = col_character()), skip = 8)
wcde_data<-as.data.frame(wcde_data)
str(wcde_data)

View(pwt100)
View(wcde_data)

library(tidyverse)
library(dplyr)

#Duplicate for messing around
pwt101<-pwt100

#Selecting the (hopefully?) relevant columns
pwt101<-pwt101[,c("country", "year", "cgdpe", "cgdpo", "cn", "ck", "ctfp")]
View(pwt101)

#renaming column from Wittgensteincenter
wcde_data<-dplyr::rename(wcde_data, country=Area)

#As the data from the Wittgensteincenter is in 5 year intervals, adjust PWT
years<-c(1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 
         2000, 2005, 2010, 2015)
pwt101<-pwt101[pwt101$year%in%years,]

#Check for country matches
unique_pwt<-unique(pwt101$country)
unique_wcde<-unique(wcde_data$country)

match(unique_pwt, unique_wcde)

check1<-unique_pwt%in%unique_wcde
check2<-unique_wcde%in%unique_pwt

countries_pwt<-unique_pwt[check1==TRUE]
countries_wcde<-unique_wcde[check2==TRUE]

#control
countries_pwt%in%countries_wcde
countries_wcde%in%countries_pwt
#Hooray

#Selecting rows based on common countries
pwt101<-pwt101[pwt101$country%in%countries_pwt,]
View(pwt101)
wcde_data<-wcde_data[wcde_data$country%in%countries_wcde,]
View(wcde_data)


