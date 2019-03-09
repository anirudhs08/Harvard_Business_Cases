# Analysis of Airline Ticket Pricing
# NAME: Anirudh Satyam
# EMAIL: anirudhs08@gmail.com
# COLLEGE / COMPANY: PES University, Bangalore.

library(readr)
airlines <- read_csv("D:/r/Internship/w4_day1_day2_mini_project/SixAirlinesDataV2.csv")
View(airlines)

#to check for the mean, median and distribution of each of the columns in the data set
summary(airlines)

#using boxplots to analyse the behavious of all variables
boxplot(airlines$FlightDuration)
boxplot(airlines$PitchEconomy)
boxplot(airlines$PitchPremium)

#to analyse the corrgram of the data set
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
#HYPOTHESIS 1
#ECONOMY PRICES AND THE DURATION OF FLIGHT ARE LINEARLY CORRELATED

#to find the correlation value between the flight duration and the economy prices
cor(airlines$FlightDuration,airlines$PriceEconomy)

#to build a regression model for the above correlation
fit <- lm(PriceEconomy~FlightDuration,data=airlines)
summary(fit)

#to construct a scatter plot to furthur visually analyze the correlation
plot(airlines$FlightDuration,airlines$PriceEconomy,main="There is a correlation 
     between the Price of Economy tickets and the flight duration",xlab="Flight duration"
     ,ylab="Price of economy tickets")

abline(fit)

#the economy prices as seen in the data set
airlines$PriceEconomy

#the predicted prices according to the model using the regression formula
fitted(fit)

#the residual values of the fitted model
residuals(fit)

#provides confidence interval for the model parameters
confint(fit)

#to run a t test
t.test(airlines$PriceEconomy,airlines$FlightDuration)

#HYPOTHESIS 2
#PREMIUM PRICES AND THE DURATION OF FLIGHT ARE LINEARLY CORRELATED

#to find the correlation value between the flight duration and the premium prices
cor(airlines$FlightDuration,airlines$PricePremium)

#to build a regression model for the above correlation
fit <- lm(PricePremium~FlightDuration,data=airlines)
summary(fit)

#to construct a scatter plot to furthur visually analyze the correlation
plot(airlines$FlightDuration,airlines$PricePremium,main="There is a correlation 
     between the Price of Premium tickets and the flight duration",xlab="Flight duration"
     ,ylab="Price of premium tickets")

abline(fit)

#the premium prices as seen in the data set
airlines$PricePremium

#the predicted prices according to the model using the regression formula
fitted(fit)

#the residual values of the fitted model
residuals(fit)

#provides confidence interval for the model parameters
confint(fit)

#to run a t test
t.test(airlines$PricePremium,airlines$FlightDuration)

