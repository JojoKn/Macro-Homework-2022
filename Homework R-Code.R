###Homework 2022###

#Importing the Datasets
library(readxl)
pwt100 <- read_excel("pwt100.xlsx", sheet = "Data")

library(readr)
wcde_data <- read_csv("wcde_data.csv", col_types = cols(Year = col_character()), skip = 8)

View(pwt100)
View(wcde_data)

library(tidyverse)
pwt100_adj<-select(c(pwt100$country, pwt100$year, pwt100$cgdpe, pwt100$cgdpo,
                     pwt100$rgdpo, pwt100$hc, pwt100$cn, pwt100$ck, pwt100$ctfp))




pwt100_adj<-matrix[data=c(pwt100$country, pwt100$year, pwt100$cgdpe, pwt100$cgdpo,
                          pwt100$rgdpo, pwt100$hc, pwt100$cn, pwt100$ck, pwt100$ctfp)]