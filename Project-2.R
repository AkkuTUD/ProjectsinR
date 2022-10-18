library(skimr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(olsrr)


data <- data.frame(read.csv(file = 'E:/Subjects/ICS3/vwcars.csv'))
head(data)

summary(data$price)


group_by(data, model) %>%
  summarise(count = n(),mean = mean(price, na.rm = TRUE),median = median(price,na.rm = TRUE),sd = sd(price, na.rm = TRUE), var = var(price, na.rm=TRUE)) 

#Data processing
#1
logprice <- log(data$price)
head(data)
#2
lp100 <- 282.48/(data$mpg)
head(data)
#3
cars_age <- 2020-(data$year)
head(data)

data = cbind(data,logprice,lp100,cars_age)

head(data)

#, data$mileage, data$logprice, data$lp100, data$cars_age


#Estimate model

model_1 = lm(price~model+cars_age+mileage+lp100+fuelType+engineSize+tax+
               transmission,data = data)
summary(model_1)

model_2 = lm(logprice~model+cars_age+mileage+lp100+fuelType+engineSize+tax+
               transmission,data = data)

summary(model_2)

#Check assumptions
par(mfrow = c(2, 2))

plot(model_1)
plot(model_2)

a <- plot(model_1,1)

b <- plot(model_2,1)

c <- plot(model_1,2)

d <- plot(model_2,2)


#Best subset selection
lmod = lm(logprice~model+cars_age+mileage+lp100+fuelType+engineSize+tax+
            transmission,data = data)


###Best Subset Selection for all the subsets of the predicates
best_data<-ols_step_all_possible(lmod)
head(best_data)

nrow(best_data)
##255

## Tasks 2 compare using AIC and BIC as model selection criteria

#see which models were chosen as best by both methods AIC and then BIC
AIC_index = which.min(best_data$aic) #247
BIC_index = which.min(best_data$sbc) ##219


head(best_data)
names(best_data)

AIC<-min(best_data$aic)
AIC
#-704.4099
BIC<-min(best_data$sbc)
BIC
#-659.3426

variables_in_aic<-best_data[AIC_index,3]
variables_in_aic
"model cars_age mileage lp100 fuelType engineSize transmission"

variables_in_bic<-best_data[BIC_index,3]
variables_in_bic
"model cars_age mileage fuelType engineSize transmission"

lm_bic = lm(logprice ~ model+cars_age+mileage+fuelType+engineSize+transmission , data = data)

plot(lm_bic)

#goodness of fit, statistical significance value 
summary(lm_bic)

#confidence interval
confint(lm_bic)

