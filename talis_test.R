### Talis code

library(readr)
library(dplyr)
pwt_all <- read_csv("pwt_all.csv", col_types = cols(country_ISO = col_number(), 
                                                    id = col_number(), year = col_number(), 
                                                    lnY = col_number(), lnK = col_number(), 
                                                    lnL = col_number(), lnH = col_number(), 
                                                    dY = col_number(), dK = col_number(), 
                                                    dL = col_number(), dH = col_number(), 
                                                    Y0 = col_number(), missing_data_85 = col_character()))

#filter for 1985 and remove missing values
data <- pwt_all %>%
        filter(year == 1985, missing_data_85 == "N")


#plot correlation
plot(dY ~ dK, data = data)
plot(dY ~ dL, data = data)
plot(dY ~ dH, data = data)

model1 <- lm(dY ~ dK + dL + dH, data = data)
summary(model1)

model2 <- lm(dY ~ dK + dL + dH + Y0, data = data)
summary(model2)

model3 <- lm(dY ~ dK + dL + dH + Y0 + OIL, data = data)
summary(model3)

model4 <- lm(dY ~ dK + dL + dH + Y0 + AFRICA + LAAMER, data = data)
summary(model4)
