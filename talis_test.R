### Talis code

rm(list=ls())
library(readr)
library(dplyr)
pwt_all <- read_csv("pwt_all.csv", col_types = cols(year = col_number(), 
                                                      rgdpe = col_number(), rgdpo = col_number(), 
                                                      pop = col_number(), Y_pc = col_number(), 
                                                      emp = col_number(), cn = col_number(), 
                                                      H = col_number(), DY = col_number(), 
                                                      DK = col_number(), DL = col_number(), 
                                                      DH = col_number(), Y0 = col_number()))

### Question 1 (a)

#filter for 1985 and remove missing values
data <- pwt_all %>%
        filter(year == 1985)


#plot correlation
plot(DY ~ DK, data = data)
plot(DY ~ DL, data = data)
plot(DY ~ DH, data = data)
plot(DY ~ Y0, data = data)

#regression models using
model1 <- lm(DY ~ DK + DL + DH, data = data)
summary(model1)

model2 <- lm(DY ~ DK + DL + DH + Y0, data = data)
summary(model2)

model3 <- lm(DY ~ DK + DL + DH + Y0 + OIL, data = data)
summary(model3)

model4 <- lm(DY ~ DK + DL + DH + Y0 + AFRICA + LAAMER, data = data)
summary(model4)

library(stargazer)
stargazer(model1, model2, model3, model4,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY.",
          dep.var.caption="1965-1985",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0","OIL","AFRICA","LAAMER"),
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"),
          type="latex",
          out="table_1.htm",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)


### Question 1 (b)
#filter for 1985 and remove missing values
data_b <- pwt_all %>%
        filter(year == 2015)


#plot correlation
plot(DY_b ~ DK_b, data = data_b)
plot(DY_b ~ DL_b, data = data_b)
plot(DY_b ~ DH_b, data = data_b)
plot(DY_b ~ Y0_b, data = data_b)

#regression models using
model1b <- lm(DY_b ~ DK_b + DL_b + DH_b, data = data_b)
summary(model1b)

model2b <- lm(DY_b ~ DK_b + DL_b + DH_b + Y0_b, data = data_b)
summary(model2b)

model3b <- lm(DY_b ~ DK_b + DL_b + DH_b + Y0_b + OIL, data = data_b)
summary(model3b)

model4b <- lm(DY_b ~ DK_b + DL_b + DH_b + Y0_b + AFRICA + LAAMER, data = data_b)
summary(model4b)


stargazer(model1b, model2b, model3b, model4b,
          title="Cross-country growth accounting results. Standard Specification - dependent variable: DY.",
          dep.var.caption="1990-2015",
          dep.var.labels="",
          covariate.labels=c("Const.","DK","DL","DH","Log Y0","OIL","AFRICA","LAAMER"),
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"),
          type="latex",
          out="table_1.htm",
          omit.stat = c("rsq","adj.rsq","ser"),
          model.numbers=FALSE,
          intercept.bottom=FALSE,
          column.sep.width="10pt",
          df=FALSE)


### ROBUSTNESS CHECKS

## Using Expenditure-side Real GDP
#plot correlations
plot(DY2 ~ DK, data = data)
plot(DY2 ~ DL, data = data)
plot(DY2 ~ DH, data = data)
plot(DY2 ~ Y02, data = data)

#regression models using
model5 <- lm(DY2 ~ DK + DL + DH, data = data)
summary(model5)

model6 <- lm(DY2 ~ DK + DL + DH + Y02, data = data)
summary(model6)

model7 <- lm(DY2 ~ DK + DL + DH + Y02 + OIL, data = data)
summary(model7)

model8 <- lm(DY2 ~ DK + DL + DH + Y02 + AFRICA + LAAMER, data = data)
summary(model8)

