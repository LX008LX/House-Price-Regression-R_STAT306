#1. Preparation
#obtaining necessary packages and libraries
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("GGally")
library("readxl")
library("ggplot2")
library("dplyr")
library("GGally")
library("leaps")
#for cleaning
data <-c(1)


#2. Reading and cleaning the data
data <- read_xlsx("C:\Users\lily xie\Documents\Real estate valuation data set.xlsx")
#A brief overview of the data
head(data)

#After reading it, we decided to give the column names more descriptive titles
colnames(data)[2] <- "transactionDate"
colnames(data)[3] <- "age"
colnames(data)[4] <- "distanceToStation"
colnames(data)[5] <- "numberOfStores"
colnames(data)[6] <- "latitude"
colnames(data)[7] <- "longtitude"
colnames(data)[8] <- "price"
#Since the “No” Column is essentially the row number, we would like to remove it
data <- subset(data, select = -No)
head(data)

#It is also a good practice to check and remove any missing values 
#in the data by column wise. Fortunately, no missing observations has been 
#found in this dataset.
print("Count of missing values by column wise") 
sapply(data, function(x) sum(is.na(x)))
nrow(data)

#We have noticed that the transaction date in this dataset is encoded in a way 
#that the year and month belong to the same column, separated by decimal points 
#and month is encoded specially. We therefore would convert the transaction date
#into a more readable format.
# Wrangling date into year and month
data$year <- as.integer(floor(data$transactionDate))
month_lookup <- c(0, 83, 167, 250, 333, 417, 500, 583, 667, 750, 833, 917)
data$month <- sapply((data$transactionDate - data$year) * 1000, function(x) which.min(abs(x - month_lookup)))
data <- subset(data, select = -transactionDate)
head(data)


#3. preliminary data analyses

#class and pairwise relationship
sapply(data, class)
ggpairs(data)

#price by year
distinct_year <- unique(data$year)
print(distinct_year)
data$year <-as.factor(data$year)
year12 <- data |> filter(year ==2012)
mean12 <- year12|> select(price) |> summarize(mean = mean(price))
mean12
year13 <- data |> filter(year ==2013)
mean13<- year13 |> select(price) |> summarize(mean=mean(price))
mean13

#price vs. distance
plot(data$distanceToStation, data$price, main="The Relationship between Distance to Nearest MRT Station and Price", xlab="Distance to Nearest MRT Station (m)", ylab="House Price per Unit Area (New Taiwan Dollar/Ping)")
data$price <- data$price

#price vs. longtitude or latitude
long <- (data$longtitude)^2
plot(data$longtitude, data$price, main="The Relationship between Longtitude and Price", xlab="Lattitude", ylab="House Price per Unit Area (New Taiwan Dollar/Ping)")
plot(long, data$price, main="The Relationship between Squared Longtitude and Price", xlab="Lattitude", ylab="House Price per Unit Area (New Taiwan Dollar/Ping)")

lat <- (data$latitude)^2
plot(data$latitude, data$price, main="The Relationship between Latitude and Price", xlab="Lattitude", ylab="House Price per Unit Area (New Taiwan Dollar/Ping)")
plot(lat, data$price, main="The Relationship between Squared Latitude and Price", xlab="Lattitude", ylab="House Price per Unit Area (New Taiwan Dollar/Ping)")


#4. model fitting

#model 1, full model with interaction between distance and longtitude
head(data)
lm_modelFull <- lm(price~age+distanceToStation+numberOfStores+latitude+longtitude+year+month+distanceToStation*longtitude, data=data)
summary(lm_modelFull)

plot(fitted(lm_modelFull), lm_modelFull$residuals, main="Residual Plot of Full Model", xlab = "Fitted Full Model", ylab="Residuals")
abline(0,0)

#model 2, removing month, still with interaction
lm_modelMonthless <- lm(price~age+distanceToStation+numberOfStores+latitude+longtitude+year+distanceToStation*longtitude, data=data)
summary(lm_modelMonthless)

plot(fitted(lm_modelMonthless), lm_modelMonthless$residuals, main="Residual Plot of Model 2 (without Month)", xlab = "Fitted Model 2 (without Month)", ylab="Residuals")
abline(0,0)

#model 3, Cp statistics
s <-regsubsets(price~age+distanceToStation+numberOfStores+latitude+longtitude+year+month+distanceToStation*longtitude, data=data)
ss <-summary(s)
ss$which

plot(2:10, ss$cp, xlab="No. of Linear Parameters", ylab="Cp", ylim=c(0, 200), main="Cp plot: Mallows' Cp Statistics Against No. of Parameters")
abline(0,1)

#Cp gives the same model as model 1


