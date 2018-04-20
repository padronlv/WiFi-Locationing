
#------------------------libraries, wd and seed-------------------------------------------
#libraries
library(rpart.plot)
library(caret)
library(forecast)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
#set wd and seed
setwd("C:/Users/VPL/Desktop/Data Science/Ubiqum/Module 4/Task 3")
set.seed(123)

#-----------------import data----------------------------------
wifidata <- read.csv("trainingData.csv", stringsAsFactors = FALSE)
wifidatatest <- read.csv("validationData.csv", stringsAsFactor = FALSE)

#--------------------------------Data exploration--------------------
#data exploration
summary(wifidata)
head(wifidata)
str(wifidata)
summary(wifidatatest)
head(wifidatatest)
str(wifidatatest)

#-------------------------------NA-----------------------
length(which(is.na(wifidata)))
length(which(wifidata == "?"))
length(which(wifidata == ""))
length(which(wifidata == " "))
length(which(wifidata == "none"))

which(wifidata == 100)
head(wifidata)

length(which(is.na(wifidatatest)))
length(which(wifidatatest == "?"))
length(which(wifidatatest == ""))
length(which(wifidatatest == " "))
length(which(wifidatatest == "none"))
head(wifidatatest)



#--------------------------------preprocess------------------------------------
wifidata <- wifidata[ , c(521:529, 1:520)]
head(wifidata)
wifidata$BUILDINGID <- factor(wifidata$BUILDINGID)
str(wifidata$BUILDINGID)
wifidata$FLOOR <- factor(wifidata$FLOOR)
str(wifidata$FLOOR)
head(wifidata)
wifidata[10:529][wifidata[10:529] == 100] <- -110

wifidatatest <- wifidatatest[ , c(521:529, 1:520)]
head(wifidatatest)
wifidatatest$BUILDINGID <- factor(wifidatatest$BUILDINGID)
str(wifidatatest$BUILDINGID)
wifidatatest$FLOOR <- factor(wifidatatest$FLOOR)
str(wifidatatest$FLOOR)
head(wifidata)
wifidatatest[10:529][wifidatatest[10:529] == 100] <- -120



ggplot(wifidata) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) + facet_grid(BUILDINGID~FLOOR)
ggplot(wifidatatest) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) + facet_grid(BUILDINGID~FLOOR)
#
samplewifidata <- sample_frac(wifidata, 0.2, replace=FALSE)
samplewifidata1 <- samplewifidata
samplewifidata1$LATITUDE <- NULL
samplewifidata1$FLOOR <- NULL
samplewifidata1$BUILDINGID <- NULL
samplewifidata1$SPACEID <- NULL
samplewifidata1$RELATIVEPOSITION <- NULL
samplewifidata1$USERID <- NULL
samplewifidata1$PHONEID <- NULL
samplewifidata1$TIMESTAMP <- NULL

databuild0 = filter(wifidata, BUILDINGID == 0)
databuild1 = filter(wifidata, BUILDINGID == 1)
databuild2 = filter(wifidata, BUILDINGID == 2)



#---------PreTrain-----------
#Datapartition
#Data_Partition <- createDataPartition(wifidata$LONGITUDE, p = .75, list = FALSE)
#training <- wifidata[Data_Partition,]
#testing <- wifidata[-Data_Partition,]

#10 fold cross validation
Control_CV <- trainControl(method = "cv", number = 3)



#------------------------------------KNN----------------------------------------
#-----------------KNN BUILDINGID
#TRAIN KNN
KNNBID <- train(BUILDINGID~.-LONGITUDE -LATITUDE -FLOOR -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = samplewifidata, method = "knn", trControl=Control_CV, tuneLength = 10)
KNNBID

predictors(KNNBID)

#make predictions
testPredKNNBID <- predict(KNNBID, wifidatatest)

#performace measurment
postResample(testPredKNNBID, wifidatatest$BUILDINGID)

#plot predicted verses actual
plot(testPredKNNBID,wifidatatest$BUILDINGID)


#---------------------KNN Longitude
#train KNN
KNNlon <- train(LONGITUDE~.-LATITUDE -FLOOR  -RELATIVEPOSITION -BUILDINGID -SPACEID -USERID -PHONEID -TIMESTAMP, data = samplewifidata, method = "knn", trControl=Control_CV, tuneLength = 10)
KNNlon
#KNNlon <- train(LONGITUDE~., data = samplewifidata1, method = "knn", trControl=Control_RepeatedCV, tuneLength = 10)
#KNNlon

#predictor variables
predictors(KNNlon)


#make predictions
testPredKNNlon <- predict(KNNlon, wifidatatest)

#performace measurment
postResample(testPredKNNlon, wifidatatest$LONGITUDE)

#plot predicted verses actual
plot(testPredKNNlon,wifidatatest$LONGITUDE)



KNN
#------------------------------------DT----------------------------------------
#train DT
RF <- train(Global_active_power~.-DateTime1h, data = training, method = "rf", importance = TRUE, trControl=Control_RepeatedCV, tuneLength =10)
RF
varImp(RF)

#predictor variables
predictors(RF)

#make predictions
testPredRF <- predict(RF, testing)

#performace measurment
postResample(testPredRF, testing$Global_active_power)

#plot predicted verses actual
plot(testPredRF,testing$Global_active_power)

#------------------------------------XGBM----------------------------------------
#train DT
XGBM <- train(Volume~ PositiveServiceReview + x4StarReviews + x1StarReviews, data = training, method = "xgbTree",
              preprocess = c("center","scale"), trControl=Control_RepeatedCV, tuneLength = 20)
XGBM <- train(Global_active_power~.-DateTime1h, data = training, method = "svmLinear", importance = TRUE, trControl=Control_RepeatedCV, tuneLength =10)
XGBM
varImp(XGBM)

#predictor variables
predictors(XGBM)

#make predictions
testPredXGBM <- predict(XGBM, testing)

#performace measurment
postResample(testPredXGBM, testing$Global_active_power)

#plot predicted verses actual
plot(testPredXGBM,testing$Global_active_power)
