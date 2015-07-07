
#############################################################
# PREDICT 454 Section 55 Team Project
# Allstate Purchase Predication Challenge
#############################################################

# Libaries Used
library(ggplot2)
library(lattice)
library(vcd)
library(corrplot)
library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(caret)
library(klaR)
library(e1071) #naiveBayes

# the entire program may be executed by typing
#  source("FletcherAllstate.R")

#############################################################
# Load Data Frames and check row counts
#############################################################

allstate.train.data.frame <- read.csv(file = "train.csv")
nrow(allstate.train.data.frame)

# Create product data types consisting of the 7 options
allstate.train.data.frame$productstr <- paste(allstate.train.data.frame$A, allstate.train.data.frame$B, allstate.train.data.frame$C,allstate.train.data.frame$D, allstate.train.data.frame$E, allstate.train.data.frame$F, allstate.train.data.frame$G, sep="")
allstate.train.data.frame$productnum <- as.numeric(allstate.train.data.frame$productstr)
allstate.train.data.frame$productfactor <- as.factor(allstate.train.data.frame$productstr)

length(unique(allstate.train.data.frame$productstr))
length(unique(allstate.train.data.frame$productnum))

#############################################################
# Data Quality Check
#############################################################

# Look at the data frame
head(allstate.train.data.frame)

# Summary Statistics on the complete data frame
summary(allstate.train.data.frame)

# Check counts
table(allstate.train.data.frame$shopping_pt)
table(allstate.train.data.frame$record_type)
table(allstate.train.data.frame$day)
table(allstate.train.data.frame$time)
table(allstate.train.data.frame$state)
table(allstate.train.data.frame$group_size)
table(allstate.train.data.frame$homeowner)
table(allstate.train.data.frame$car_value)
table(allstate.train.data.frame$risk_factor)
table(allstate.train.data.frame$married_couple)
table(allstate.train.data.frame$C_previous)
table(allstate.train.data.frame$duration_previous)
table(allstate.train.data.frame$A)
table(allstate.train.data.frame$B)
table(allstate.train.data.frame$C)
table(allstate.train.data.frame$D)
table(allstate.train.data.frame$E)
table(allstate.train.data.frame$F)
table(allstate.train.data.frame$G)

sort(table(allstate.train.data.frame$productstr), decreasing=TRUE)

#############################################################
# Respone Variable - Categorical
#############################################################

ggplot(data = allstate.train.data.frame) +
  aes(x = productfactor) + 
  geom_bar(colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of Product Options") +
  ggtitle("Product Option Counts")

#############################################################
# Predictors - Categorical and Continuous
#############################################################

## Customers
length(unique(allstate.train.data.frame$customer_ID))

## Shopping Point
ggplot(data = allstate.train.data.frame) +
  aes(x = shopping_pt) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of Shoppint Point") +
  ggtitle("Shopping Point Counts")

## Record Type
ggplot(data = allstate.train.data.frame) +
  aes(x = record_type) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of Record Type") +
  ggtitle("Record Type Counts")

## day
ggplot(data = allstate.train.data.frame) +
  aes(x = day) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of Day") +
  ggtitle("Day Counts")

## location
ggplot(data = allstate.train.data.frame) +
  aes(x = location) + 
  geom_histogram(binwidth = 100, colour = "white", fill = "blue", origin = 10) +
  labs(x = "Location") +
  labs(y = "Frequency") +
  ggtitle("Histogram of Location")

## car_age
ggplot(data = allstate.train.data.frame) +
  aes(x = car_age) + 
  geom_histogram(binwidth = 10, colour = "white", fill = "blue", origin = 0) +
  labs(x = "Car Age") +
  labs(y = "Frequency") +
  ggtitle("Histogram of Car Age")

## car_value
ggplot(data = allstate.train.data.frame) +
  aes(x = car_value) + 
  geom_histogram(binwidth = 10, colour = "white", fill = "blue", origin = 0) +
  labs(x = "Car Value") +
  labs(y = "Frequency") +
  ggtitle("Histogram of Car Value")

## risk_factor
ggplot(data = allstate.train.data.frame) +
  aes(x = risk_factor) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of Risk Factor") +
  ggtitle("Risk Factor Counts")

## age_oldest
ggplot(data = allstate.train.data.frame) +
  aes(x = age_oldest) + 
  geom_histogram(binwidth = 10, colour = "white", fill = "blue", origin = 0) +
  labs(x = "Age Oldest") +
  labs(y = "Frequency") +
  ggtitle("Histogram of Age Oldest")

## age_youngest
ggplot(data = allstate.train.data.frame) +
  aes(x = age_youngest) + 
  geom_histogram(binwidth = 10, colour = "white", fill = "blue", origin = 0) +
  labs(x = "Age Youngest") +
  labs(y = "Frequency") +
  ggtitle("Histogram of Age Youngest")

## married_couple
ggplot(data = allstate.train.data.frame) +
  aes(x = married_couple) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of Married Couple") +
  ggtitle("Married Couple Counts")

## C_previous
ggplot(data = allstate.train.data.frame) +
  aes(x = C_previous) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of C_previous") +
  ggtitle("C_previous Counts")

## duration_previous
ggplot(data = allstate.train.data.frame) +
  aes(x = duration_previous) + 
  geom_bar(binwidth=1, colour = "black", fill = "blue", origin = 0) +
  labs(x = "") +
  labs(y = "Counts of duration_previous") +
  ggtitle("duration_previous Counts")

## cost
ggplot(data = allstate.train.data.frame) +
  aes(x = cost) + 
  geom_histogram(binwidth = 10, colour = "white", fill = "blue", origin = 0) +
  labs(x = "Cost") +
  labs(y = "Frequency") +
  ggtitle("Histogram of Cost")

#############################################################
## Combo Density Checks
#############################################################

ggplot(allstate.train.data.frame, aes(x=cost, fill=A)) + labs(x="State") + geom_density(alpha=.3)

############################################################
## 2D Density Plots
############################################################

ggplot(wine.data.frame,aes(x=Alcohol, y=Flavanoids, colour=WineType)) + geom_point() + stat_density2d() 

