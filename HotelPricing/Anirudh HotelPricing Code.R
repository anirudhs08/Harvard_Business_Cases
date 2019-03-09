# Project Title: Hotel Room Pricing In The Indian Market
# NAME: Anirudh S
# EMAIL: anirudhs08@gmail.com
# COLLEGE / COMPANY: PES University, Bangalore 

#The data set is imported in R using appropriate commands.
library(readr)
cities <- read_csv("D:/r/Internship/w4_day4_day5_final_project/Cities42.csv")
View(cities)

#All the variables are analysed.
describe(cities)


#our dependent variable Y is the RoomRent

#Independent variables are a combination of continuous and categorical variables.

#independent categorical variables.
#1. IsMetroCity

#number of cities which are metro cities and which are not metro cities
table(cities$IsMetroCity)

#mean room rent in metro cities and mean room rent in other cities
aggregate(cities$RoomRent, by=list(cities$IsMetroCity), mean)

#t test 
t.test(RoomRent~IsMetroCity, data=cities)

#scatterplot for relationship between room rent and metro cities
scatterplot(cities$IsMetroCity, cities$RoomRent, xlab= "Metro City(1) Non-metro city(0)", ylab=" Room Rent", main=" Room rent vs Metro city")

#2. IsTouristDestination

#to find out number of cities which are tourist destinations and number of cities
#which are not tourist destinations
table(cities$IsTouristDestination)

#to find out the mean of room rent of all hotels in cities which are tourist 
#destinations and mean of room rent of hotels in cities which are not tourist
#destinations
aggregate(cities$RoomRent, by=list(cities$IsTouristDestination), mean)

#t test
t.test(RoomRent~IsTouristDestination, data=cities)

#to visually see the relationship pair-wise
scatterplot(cities$IsTouristDestination, cities$RoomRent, xlab= "Tourist Destination(1) Not a tourist destination(0)", ylab=" Room Rent", main=" Room rent vs TouristDestination or Not")

#3. HasSwimmingPool

#to see number of hotels which had swimming pool and number of hotels that did not
#have swimming pool
table(cities$HasSwimmingPool)

#to find out mean of room rent of all hotels with and without swimming pool
aggregate(cities$RoomRent, by=list(cities$HasSwimmingPool), mean)

#t test
t.test(RoomRent~HasSwimmingPool, data=cities)

#to visually see the relationship pair-wise
scatterplot(cities$HasSwimmingPool, cities$RoomRent, xlab= "Has a Swimming Pool(1) Does not have a Swimming Pool(0)", ylab=" Room Rent", main=" Room rent vs Having and Not having a Swimming Pool")


#4. IsNewYearEve

#number of days which were newyear eve and number of days which were not 
#not new year eve for the days on which the room rent was taken
table(cities$IsNewYearEve)

#to find the mean of all the room rents on new year eve and on other days
aggregate(cities$RoomRent, by=list(cities$IsNewYearEve), mean)

#t test
t.test(RoomRent~IsNewYearEve, data=cities)

#to visually see the relationship pair-wise
scatterplot(cities$IsNewYearEve, cities$RoomRent, xlab= "New Year Eve(1) Other days(0)", ylab=" Room Rent", main=" Room rent vs New Year's Eve or Not")





#Independent continuous variables
#1. StarRating

#to measure correlation between room rent and the star rating
cor(cities$RoomRent, cities$StarRating)

#to examine the variable Starrating, check the median and outliers
boxplot(cities$StarRating)

#to visually see the relationship pair-wise
scatterplot(cities$StarRating, cities$RoomRent, xlab= "Star Rating", ylab=" Room Rent", main=" Room rent vs Star Rating")

#2.Hotel Capacity

#to measure the correlation between room rent and hotel capacity
cor(cities$RoomRent, cities$HotelCapacity)

#to examine hotel capacity, and check its mean and outliers
boxplot(cities$HotelCapacity)

#to visually see the relationship pair-wise
scatterplot(cities$HotelCapacity, cities$RoomRent, xlab= "Hotel Capacity", ylab=" Room Rent", main=" Room rent vs Hotel Capacity")

#3.Airport

#to measure the correlation between room rent and nearest distance to airport
cor(cities$RoomRent, cities$Airport)

#to examine Airport variable, and check its mean and outliers
boxplot(cities$HotelCapacity)

#to visually see the relationship pair-wise
scatterplot(cities$Airport, cities$RoomRent, xlab= "Airport distance", ylab=" Room Rent", main=" Room rent vs Airport distance")

#corrgram
library(Hmisc)
library(car)
library(corrgram)
colcities <- c("RoomRent","StarRating","HotelCapacity","HasSwimmingPool")
corrgram(cities[,colcities], order=TRUE, main="Dependency of RoomRent",lower.panel=panel.pts,
upper.panel=panel.pie, diag.panel=panel.minmax, text.panel=panel.txt)

#variance-covariance matrix
vcov(lm(cities$RoomRent~cities$StarRating+cities$HotelCapacity + cities$HasSwimmingPool))



#correlation matrix
library(Hmisc)
colcities <- c("RoomRent","StarRating","HotelCapacity","HasSwimmingPool")
corMatrix <- rcorr(as.matrix(cities[,colcities]))
corMatrix


#t tests for the categorical variables along with the aggregate command

#1. IsWeekend
aggregate(cities$RoomRent, by=list(cities$IsWeekend), mean)
t.test(RoomRent~IsWeekend, data=cities)

#2.IsNewYearEve
aggregate(cities$RoomRent, by=list(cities$IsNewYearEve), mean)
t.test(RoomRent~IsNewYearEve, data=cities)

#3.FreeBreakfast
aggregate(cities$RoomRent, by=list(cities$FreeBreakfast), mean)
t.test(RoomRent~FreeBreakfast, data=cities)

#4.FreeWifi
aggregate(cities$RoomRent, by=list(cities$FreeWifi), mean)
t.test(RoomRent~FreeWifi, data=cities)

#5.IsTouristDestination
aggregate(cities$RoomRent, by=list(cities$IsTouristDestination), mean)
t.test(RoomRent~IsTouristDestination, data=cities)

#6.IsMetroCity
aggregate(cities$RoomRent, by=list(cities$IsMetroCity), mean)
t.test(RoomRent~IsMetroCity, data=cities)

#7.HasSwimmingPool
aggregate(cities$RoomRent, by=list(cities$HasSwimmingPool), mean)
t.test(RoomRent~HasSwimmingPool, data=cities)

#regression analysis

#model 1
model1 <- RoomRent ~ CityName + Population + IsMetroCity + IsTouristDestination +
IsWeekend + IsNewYearEve + StarRating + Airport + FreeWifi + FreeBreakfast +
HotelCapacity + HasSwimmingPool

fit <- lm(model1, data=cities)

summary(fit)

#model 2

model2 <- RoomRent ~  IsNewYearEve + StarRating + Airport + FreeWifi + HotelCapacity
+ HasSwimmingPool
fit <- lm(model2, data=cities)
summary(fit)

#model 3
model3 <- RoomRent ~  IsNewYearEve + StarRating + Airport +  HotelCapacity +
  HasSwimmingPool
fit <- lm(model3, data=cities)
summary(fit)

