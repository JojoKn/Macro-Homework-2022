### Talis code

library(readr)
library(dplyr)
pwt_all <- read_csv("pwt_all.csv", col_types = cols(year = col_number(), 
                                                      rgdpe = col_number(), rgdpo = col_number(), 
                                                      pop = col_number(), Y_pc = col_number(), 
                                                      emp = col_number(), cn = col_number(), 
                                                      H = col_number(), DY = col_number(), 
                                                      DK = col_number(), DL = col_number(), 
                                                      DH = col_number(), Y0 = col_number()))

#filter for 1985 and remove missing values
data <- pwt_all %>%
        filter(year == 1985)


#plot correlation
plot(DY ~ DK, data = data)
plot(DY ~ DL, data = data)
plot(DY ~ DH, data = data)

model1 <- lm(DY ~ DK + DL + DH, data = data)
summary(model1)

model2 <- lm(DY ~ DK + DL + DH + Y0, data = data)
summary(model2)

model3 <- lm(DY ~ DK + DL + DH + Y0 + OIL, data = data)
summary(model3)

model4 <- lm(DY ~ DK + DL + DH + Y0 + AFRICA + LAAMER, data = data)
summary(model4)
