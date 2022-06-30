###Homework 2022###

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

pwt101<-pwt100[,c("countrycode", "country", "year", "cgdpe", "cgdpo", "cn", "emp", "pop", "hc", "ctfp")]
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

write.csv(total, file="total_uncleaned.csv")

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

write.csv(total_a_clean, file="total_a_cleaned.csv")


#Check: Length should be half of the number of rows from the data frame
length(unique(total_a_clean$ISOCode3))

#Select subset of relevant years
data65<-subset(total_a_clean, year==1965)
data85<-subset(total_a_clean, year==1985)

#Get per capita income
data65$cgdpo<-data65$cgdpo/data65$pop
data85$cgdpo<-data85$cgdpo/data85$pop

#Calculate the log differences
dya<-log(data85$cgdpo/data65$cgdpo)
dKa<-log(data85$cn/data65$cn)
dLa<-log(data85$emp/data65$emp)
dHa<-log(data85$Years/data65$Years)
logY0a<-log(data65$cgdpo)

#The author uses heteroskedasticity robust standard errord, which is not implemented
#in base R; you need a package called sandwich for it:
#https://thomasleeper.com/Rcourse/Tutorials/olsrobustSEs.html

library(sandwich)

model1a<-lm(dya~dKa+dLa+dHa)
smodel1a<-summary(lm(dya~dKa+dLa+dHa))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel1a$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dya~dKa+dLa+dHa))))
smodel1a

model2a<-lm(dya~dKa+dLa+dHa+logY0a)
smodel2a<-summary(lm(dya~dKa+dLa+dHa+logY0a))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel2a$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dya~dKa+dLa+dHa+logY0a))))
smodel2a

model3a<-lm(dya~dKa+dLa+dHa+logY0a+data65$oil)
smodel3a<-summary(lm(dya~dKa+dLa+dHa+logY0a+data65$oil))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel3a$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dya~dKa+dLa+dHa+logY0a+data65$oil))))
smodel3a

model4a<-lm(dya~dKa+dLa+dHa+logY0a+data65$africa+data65$laamer)
smodel4a<-summary(lm(dya~dKa+dLa+dHa+logY0a+data65$africa+data65$laamer))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel4a$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dya~dKa+dLa+dHa+logY0a+data65$africa+data65$laamer))))
smodel4a

library(stargazer)

stargazer(model1a, model2a, model3a, model4a,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY.",
          dep.var.caption="1965-1985",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0","OIL","AFRICA","LAAMER"),
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"),
          type="latex",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)
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

#Calculate log differences in variables
dyb<-log(data15$cgdpo/data90$cgdpo)
dKb<-log(data15$cn/data90$cn)
dLb<-log(data15$emp/data90$emp)
dHb<-log(data15$Years/data90$Years)
logY0b<-log(data90$cgdpo)

model1b<-lm(dyb~dKb+dLb+dHb)
smodel1b<-summary(lm(dyb~dKb+dLb+dHb))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel1b$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyb~dKb+dLb+dHb))))
smodel1b

model2b<-lm(dyb~dKb+dLb+dHb+logY0b)
smodel2b<-summary(lm(dyb~dKb+dLb+dHb+logY0b))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel2b$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyb~dKb+dLb+dHb+logY0b))))
smodel2b

model3b<-lm(dyb~dKb+dLb+dHb+logY0b+data90$oil)
smodel3b<-summary(lm(dyb~dKb+dLb+dHb+logY0b+data90$oil))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel3b$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyb~dKb+dLb+dHb+logY0b+data90$oil))))
smodel3b

model4b<-lm(dyb~dKb+dLb+dHb+logY0b+data90$africa+data90$laamer)
smodel4b<-summary(lm(dyb~dKb+dLb+dHb+logY0b+data90$africa+data90$laamer))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel4b$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyb~dKb+dLb+dHb+logY0b+data90$africa+data90$laamer))))
smodel4b

stargazer(model1b, model2b, model3b, model4b,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY.",
          dep.var.caption="1990-2015",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0","OIL","AFRICA","LAAMER"),
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"),
          type="latex",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)

####Robustness Checks####
#For the sake of the results being comparable to task a and the paper itself, the following
#time frame is assumed in this part of the code as well: 1965-1985

#####Alternate Measures for Employment#####
#As in the paper, the idea is to not use the employment data but rather the pure population
#to appoximate the labor force

View(data65)
View(data85)

dyc1<-log(data85$cgdpo/data65$cgdpo)
dKc1<-log(data85$cn/data65$cn)
dLc1<-log(data85$pop/data65$pop)
dHc1<-log(data85$Years/data65$Years)
logY0c1<-log(data65$cgdpo)

model1c1<-lm(dyc1~dKc1+dLc1+dHc1)
smodel1c1<-summary(lm(dyc1~dKc1+dLc1+dHc1))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel1c1$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc1~dKc1+dLc1+dHc1))))
smodel1c1

model2c1<-lm(dyc1~dKc1+dLc1+dHc1+logY0c1)
smodel2c1<-summary(lm(dyc1~dKc1+dLc1+dHc1+logY0c1))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel2c1$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc1~dKc1+dLc1+dHc1+logY0c1))))
smodel2c1

model3c1<-lm(dyc1~dKc1+dLc1+dHc1+logY0c1+data65$oil)
smodel3c1<-summary(lm(dyc1~dKc1+dLc1+dHc1+logY0c1+data65$oil))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel3c1$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc1~dKc1+dLc1+dHc1+logY0c1+data65$oil))))
smodel3c1

model4c1<-lm(dyc1~dKc1+dLc1+dHc1+logY0c1+data65$africa+data65$laamer)
smodel4c1<-summary(lm(dyc1~dKc1+dLc1+dHc1+logY0c1+data65$africa+data65$laamer))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel4c1$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc1~dKc1+dLc1+dHc1+logY0c1+data65$africa+data65$laamer))))
smodel4c1

library(stargazer)

stargazer(model1c1, model2c1, model3c1, model4c1,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY. 
          Robustness Check No. 1: Population instead of Employment as measure of labor force",
          dep.var.caption="1965-1985",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0","OIL","AFRICA","LAAMER"),
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"),
          type="latex",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)

#####Alternate Measures for Human Capital######
#The Wittgenstein Center provides data on human capital; the idea is to use the measure
#already included in the Penn World Tables

View(data65)
View(data85)

dyc2<-log(data85$cgdpo/data65$cgdpo)
dKc2<-log(data85$cn/data65$cn)
dLc2<-log(data85$emp/data65$emp)
dHc2<-log(data85$hc/data65$hc)
logY0c2<-log(data65$cgdpo)

model1c2<-lm(dyc2~dKc2+dLc2+dHc2)
smodel1c2<-summary(lm(dyc2~dKc2+dLc2+dHc2))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel1c2$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc2~dKc2+dLc2+dHc2))))
smodel1c2

model2c2<-lm(dyc2~dKc2+dLc2+dHc2+logY0c2)
smodel2c2<-summary(lm(dyc2~dKc2+dLc2+dHc2+logY0c2))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel2c2$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc2~dKc2+dLc2+dHc2+logY0c2))))
smodel2c2

model3c2<-lm(dyc2~dKc2+dLc2+dHc2+logY0c2+data65$oil)
smodel3c2<-summary(lm(dyc2~dKc2+dLc2+dHc2+logY0c2+data65$oil))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel3c2$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc2~dKc2+dLc2+dHc2+logY0c2+data65$oil))))
smodel3c2

model4c2<-lm(dyc2~dKc2+dLc2+dHc2+logY0c2+data65$africa+data65$laamer)
smodel4c2<-summary(lm(dyc2~dKc2+dLc2+dHc2+logY0c2+data65$africa+data65$laamer))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel4c2$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc2~dKc2+dLc2+dHc2+logY0c2+data65$africa+data65$laamer))))
smodel4c2

library(stargazer)

stargazer(model1c2, model2c2, model3c2, model4c2,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY. 
          Robustness Check No. 2: Use Human Capital Measure included in Penn World Tables",
          dep.var.caption="1965-1985",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0","OIL","AFRICA","LAAMER"),
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"),
          type="latex",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)

#####Subsample Analysis for Africa#####
#Instead of just including a dummy, lets run the regression for the subsample of African
#countries in the sample.

#Select new datasubset
data65afr<-subset(total_a_clean, year==1965 & africa==1)
data85afr<-subset(total_a_clean, year==1985 & africa==1)

#Calculate per capita income
data65afr$cgdpo<-data65afr$cgdpo/data65afr$pop
data85afr$cgdpo<-data85afr$cgdpo/data85afr$pop

View(data65afr)
View(data85afr)

#Calculate log changes
dyc3<-log(data85afr$cgdpo/data65afr$cgdpo)
dKc3<-log(data85afr$cn/data65afr$cn)
dLc3<-log(data85afr$emp/data65afr$emp)
dHc3<-log(data85afr$Years/data65afr$Years)
logY0c3<-log(data65afr$cgdpo)

#Run the models; due to the special subsample, only model 1 and model 2 make sense.
model1c3<-lm(dyc3~dKc3+dLc3+dHc3)
smodel1c3<-summary(lm(dyc3~dKc3+dLc3+dHc3))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel1c3$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc3~dKc3+dLc3+dHc3))))
smodel1c3

model2c3<-lm(dyc3~dKc3+dLc3+dHc3+logY0c3)
smodel2c3<-summary(lm(dyc3~dKc3+dLc3+dHc3+logY0c3))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel2c3$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc3~dKc3+dLc3+dHc3+logY0c3))))
smodel2c3

library(stargazer)

stargazer(model1c3, model2c3 ,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY. 
          Robustness Check No. 3: Subsample analysis for African countries.",
          dep.var.caption="1965-1985",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0"),
          column.labels=c("Model 1", "Model 2"),
          type="latex",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)

#####Subsample Analysis for Latin America#####
#Instead of just including a dummy, lets run the regression for the subsample of Latin
#American countries in the sample.

#Select new datasubset
data65laam<-subset(total_a_clean, year==1965 & laamer==1)
data85laam<-subset(total_a_clean, year==1985 & laamer==1)

#Calculate per capita income
data65laam$cgdpo<-data65laam$cgdpo/data65laam$pop
data85laam$cgdpo<-data85laam$cgdpo/data85laam$pop

View(data65laam)
View(data85laam)

#Calculate log changes
dyc4<-log(data85laam$cgdpo/data65laam$cgdpo)
dKc4<-log(data85laam$cn/data65laam$cn)
dLc4<-log(data85laam$emp/data65laam$emp)
dHc4<-log(data85laam$Years/data65laam$Years)
logY0c4<-log(data65laam$cgdpo)

#Run the models; due to the special subsample, only model 1 and model 2 make sense.
model1c4<-lm(dyc4~dKc4+dLc4+dHc4)
smodel1c4<-summary(lm(dyc4~dKc4+dLc4+dHc4))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel1c4$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc4~dKc4+dLc4+dHc4))))
smodel1c4

model2c4<-lm(dyc4~dKc4+dLc4+dHc4+logY0c4)
smodel2c4<-summary(lm(dyc4~dKc4+dLc4+dHc4+logY0c4))
#Adjust the standard errors for the heteroskedasticity robust ones
smodel2c4$coefficients[, 2] <- sqrt(diag(vcovHC(lm(dyc4~dKc4+dLc4+dHc4+logY0c4))))
smodel2c4

library(stargazer)

stargazer(model1c4, model2c4 ,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY. 
          Robustness Check No. 4: Subsample analysis for Latin American countries.",
          dep.var.caption="1965-1985",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0"),
          column.labels=c("Model 1", "Model 2"),
          type="latex",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)

#####Including TFP instead of a constant#####
#Growth Accountig is used to proxy total factor productivity as we usually do not have
#data on it. However, the Penn World tables allow us to actually include TFP as 
#an additional variable in the regession analysis instead of a constant. 

