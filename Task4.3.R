
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
which(apply(wifidata, 2, var) == 0)


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
wifidata[10:529][wifidata[10:529] == 100] <- -120

wifidatatest <- wifidatatest[ , c(521:529, 1:520)]
head(wifidatatest)
wifidatatest$BUILDINGID <- factor(wifidatatest$BUILDINGID)
str(wifidatatest$BUILDINGID)
wifidatatest$FLOOR <- factor(wifidatatest$FLOOR)
str(wifidatatest$FLOOR)
head(wifidata)
wifidatatest[10:529][wifidatatest[10:529] == 100] <- -120

#----data visualization
ggplot(wifidata) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) #+ facet_grid(BUILDINGID~FLOOR)
ggplot(wifidatatest) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) #+ facet_grid(BUILDINGID~FLOOR)

#----join tables
wifi <- bind_rows(wifidata, wifidatatest)
head(wifi)

#Removing zero variance columns
remove_this <- c(which(apply(wifi, 2, var) == 0))
remove_this1 <- c(which(apply(wifi[,10:529], 1, var) == 0))




#remove_this <- c(which(apply(wifidata, 2, var) == 0), which(apply(wifidatatest, 2, var) == 0))
#wifidata0 <- wifidata[,-remove_this]
#wifidatatest0 <- wifidatatest[,-remove_this]

#Distribution of the strenght of the signal in all observations.
onlyWAPs <- wifi[,c(10:529)]
WAPvector <- c(as.matrix(onlyWAPs))
WAPvector <- WAPvector[which(WAPvector != -120)]
str(WAPvector)
hist(WAPvector)
WAPVstrong <- WAPvector[which(WAPvector > -20)]
str(WAPVstrong)
hist(WAPVstrong)

#WAP IN EVERY BUILDING
bwaps <- wifi[, c(4,10:529)]
bwaps_count <- as.data.frame(apply(bwaps[, 2:521], 2, function(x) ifelse(x == -120, 0, 1)))
bwaps_count1 <- bind_cols(bwaps[1], bwaps_count)
bwaps_group <- group_by(bwaps_count1, BUILDINGID)
bwaps_group <- summarise_all(bwaps_group, funs(sum))
head(bwaps_group)


#strong signal
wifistrong <- wifi[which(apply(onlyWAPs, 1, function(x) length(which(x>-30))) != 0),]
wifistrong <- wifistrong[,which(apply(wifistrong, 2, function(x) length(which(x>-30))) != 0)]
head(wifistrong)
plot(wifistrong$LATITUD, wifistrong$LONGITUDE)


#create a vector to create an histogram which allows to see how many WAPs have signal in an observation
#onlyWAPs <- wifidata0[,c(7:318)]
#my_signals <- apply(onlyWAPs, 1, function(x) length(which(x!= -120)))
#hist(my_signals)


#normalization by rows (use t to transform the table)(I decided not to apply it)
#wifitrain <- as.data.frame(t(apply(wifidata0[ ,7:318], 1, function(x) (x - min(x))/(max(x) -min(x)))))
#wifitest <- as.data.frame(t(apply(wifidatatest0[ ,7:318], 1, function(x) (x - min(x))/(max(x) -min(x)))))

#normalization by colums
#wifitrain <- as.data.frame(apply(wifidata0[ ,7:318], 2, function(x) (x - min(x))/(max(x) -min(x))))
#wifitest <- as.data.frame(apply(wifidatatest0[ ,7:318], 2, function(x) (x - min(x))/(max(x) -min(x))))
wifi[ ,10:529] <- as.data.frame(apply(wifi[ ,10:529], 2, function(x) (x - min(x))/(max(x) -min(x))))




#wifivector <- c(as.matrix(wifidata0[,7:318]))
#wifivector <- wifivector[-which(wifivector == -120)]

#ggplot() + geom_histogram(aes(x = wifivector), bins = 50) + ggtitle("") + 
#  theme(plot.title = element_text(hjust = 0.5))


#samplewifidata <- sample_frac(wifidata0, 0.2, replace=FALSE)
samplewifi <- sample_frac(wifi, 0.3, replace=FALSE)
#samplewifidata1 <- samplewifidata
#samplewifidata1$LATITUDE <- NULL
#samplewifidata1$FLOOR <- NULL
#samplewifidata1$BUILDINGID <- NULL
#samplewifidata1$SPACEID <- NULL
#samplewifidata1$RELATIVEPOSITION <- NULL
#samplewifidata1$USERID <- NULL
#samplewifidata1$PHONEID <- NULL
#samplewifidata1$TIMESTAMP <- NULL

databuild0 = filter(wifidata, BUILDINGID == 0)
databuild1 = filter(wifidata, BUILDINGID == 1)
databuild2 = filter(wifidata, BUILDINGID == 2)



#---------PreTrain-----------
#Datapartition
Data_Partition <- createDataPartition(samplewifi$BUILDINGID, p = .8, list = FALSE)
trainwifi <- samplewifi[Data_Partition,]
testwifi <- samplewifi[-Data_Partition,]

#10 fold cross validation
Control_CV <- trainControl(method = "cv", number = 3)

#------------------------------------KNN----------------------------------------
#-----------------KNN BUILDINGID
#TRAIN KNN
KNNBID <- train(BUILDINGID~.-LONGITUDE -LATITUDE -FLOOR -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = trainwifi,
                method = "knn", trControl=Control_CV, tuneLength = 6)
KNNBID
saveRDS(KNNBID, file = "KNNBIDn.RDS")
KNNBID <- readRDS("KNNBIDn.RDS")

predictors(KNNBID)

#make predictions
trainPredKNNBID <- predict(KNNBID, trainwifi)
testPredKNNBID <- predict(KNNBID, testwifi)

#performace measurment
postResample(testPredKNNBID, testwifi$BUILDINGID)

#plot predicted verses actual
plot(testPredKNNBID, testwifi$BUILDINGID)

#new tables with buildingIDs substituted by the predicted ones
wifitestBIDpred <- testwifi
wifitestBIDpred$BUILDINGID <- testPredKNNBID
wifitrainBIDpred <- trainwifi
wifitrainBIDpred$BUILDINGID <- trainPredKNNBID

#plots for comprobation of acuracy

ggplot(testwifi) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitestBIDpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainBIDpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) + facet_grid(BUILDINGID~FLOOR)


#-----------------KNN Floor
#TRAIN KNN
KNNfloor <- train(FLOOR~. -LONGITUDE -LATITUDE -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = wifitrainBIDpred,
                method = "knn", trControl=Control_CV, tuneLength = 6)
KNNfloor
saveRDS(KNNfloor, file = "KNNfloorn.RDS")
KNNfloor <- readRDS("KNNfloorn.RDS")

predictors(KNNfloor)

#make predictions
trainPredKNNfloor <- predict(KNNfloor, wifitrainBIDpred)
testPredKNNfloor <- predict(KNNfloor, wifitestBIDpred)

#performace measurment
postResample(testPredKNNfloor, wifitestBIDpred$FLOOR)

#plot predicted verses actual
plot(testPredKNNfloor,wifitestBIDpred$FLOOR)

#new tables with buildingIDs substituted by the predicted ones
wifitestfloorpred <- wifitestBIDpred
wifitestfloorpred$floor <- testPredKNNfloor
wifitrainfloorpred <- wifitrainBIDpred
wifitrainfloorpred$floor <- trainPredKNNfloor

#plots for comprobation of acuracy
ggplot(wifitestfloorpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))# + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainfloorpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))# + facet_grid(BUILDINGID~FLOOR)



#---------------------KNN Longitude
#train KNN
KNNlon <- train(LONGITUDE~.-LATITUDE  -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = wifitrainfloorpred, 
     method = "knn", trControl=Control_CV, tuneLength = 6)
KNNlon

saveRDS(KNNlon, file = "KNNlonn.RDS")
KNNlon <- readRDS("KNNlonn.RDS")


predictors(KNNlon)

#make predictions
trainPredKNNlon <- predict(KNNlon, wifitrainfloorpred)
testPredKNNlon <- predict(KNNlon, wifitestfloorpred)

#performace measurment
postResample(testPredKNNlon, wifitestfloorpred$LONGITUDE)

#plot predicted verses actual
plot(testPredKNNlon,wifitestfloorpred$LONGITUDE)

#new tables with buildingIDs substituted by the predicted ones
wifitestlonpred <- wifitestfloorpred
wifitestlonpred$LONGITUDE <- testPredKNNlon
wifitrainlonpred <- wifitrainfloorpred
wifitrainlonpred$LONGITUDE <- trainPredKNNlon

#plots for comprobation of acuracy
ggplot(wifitestlonpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainlonpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)

#---------------------KNN LATITUDE
#train KNN
KNNlat <- train(LATITUDE~. -LONGITUDE -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = wifitrainlonpred, 
                method = "knn", trControl=Control_CV, tuneLength = 6)
KNNlat
saveRDS(KNNlat, file = "KNNlatn.RDS")
KNNlat <- readRDS("KNNlatn.RDS")

predictors(KNNlat)

#make predictions
trainPredKNNlat <- predict(KNNlat, wifitrainlonpred)
testPredKNNlat <- predict(KNNlat, wifitestlonpred)

#performace measurment
postResample(testPredKNNlat, wifitestlonpred$LATITUDE)

#plot predicted verses actual
plot(testPredKNNlat,wifitestlonpred$LATITUDE)

#new tables with buildingIDs substituted by the predicted ones
wifitestlatpred <- wifitestlonpred
wifitestlatpred$LATITUDE <- testPredKNNlat
wifitrainlatpred <- wifitrainlonpred
wifitrainlatpred$LATITUDE <- trainPredKNNlat

#plots for comprobation of acuracy
ggplot(wifitestlatpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))  # + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainlatpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)

#------------------------------------RF----------------------------------------
#-----------------RF
#TRAIN RF
RFBID <- train(BUILDINGID~.-LONGITUDE -LATITUDE -FLOOR -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = trainwifi,
               method = "rf", trControl=Control_CV, tuneLength = 6)
RFBID
saveRDS(RFBID, file = "RFBIDn.RDS")
RFBID <- readRDS("RFBIDn.RDS")

predictors(RFBID)

#make predictions
trainPredRFBID <- predict(RFBID, trainwifi)
testPredRFBID <- predict(RFBID, testwifi)

#performace measurment
postResample(testPredRFBID, testwifi$BUILDINGID)

#plot predicted verses actual
plot(testPredRFBID, testwifi$BUILDINGID)

#new tables with buildingIDs substituted by the predicted ones
wifitestBIDpred <- testwifi
wifitestBIDpred$BUILDINGID <- testPredRFBID
wifitrainBIDpred <- trainwifi
wifitrainBIDpred$BUILDINGID <- trainPredRFBID

#plots for comprobation of acuracy

ggplot(testwifi) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitestBIDpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainBIDpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))# + facet_grid(BUILDINGID~FLOOR)


#-----------------RF Floor
#TRAIN RF
RFfloor <- train(FLOOR~. -LONGITUDE -LATITUDE -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = wifitrainBIDpred,
                method = "rf", trControl=Control_CV, tuneLength = 6)
RFfloor
saveRDS(RFfloor, file = "RFfloorn.RDS")
RFfloor <- readRDS("RFfloorn.RDS")

predictors(RFfloor)

#make predictions
trainPredRFfloor <- predict(RFfloor, wifitrainBIDpred)
testPredRFfloor <- predict(RFfloor, wifitestBIDpred)

#performace measurment
postResample(testPredRFfloor, wifitestBIDpred$FLOOR)

#plot predicted verses actual
plot(testPredRFfloor,wifitestBIDpred$FLOOR)

#new tables with buildingIDs substituted by the predicted ones
wifitestfloorpred <- wifitestBIDpred
wifitestfloorpred$floor <- testPredRFfloor
wifitrainfloorpred <- wifitrainBIDpred
wifitrainfloorpred$floor <- trainPredRFfloor

#plots for comprobation of acuracy
ggplot(wifitestfloorpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))# + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainfloorpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))# + facet_grid(BUILDINGID~FLOOR)



#---------------------RF Longitude
#train RF
RFlon <- train(LONGITUDE~.-LATITUDE  -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = wifitrainfloorpred, 
     method = "rf", trControl=Control_CV, tuneLength = 6)
RFlon

saveRDS(RFlon, file = "RFlonn.RDS")
RFlon <- readRDS("RFlonn.RDS")


predictors(RFlon)

#make predictions
trainPredRFlon <- predict(RFlon, wifitrainfloorpred)
testPredRFlon <- predict(RFlon, wifitestfloorpred)

#performace measurment
postResample(testPredRFlon, wifitestfloorpred$LONGITUDE)

#plot predicted verses actual
plot(testPredRFlon,wifitestfloorpred$LONGITUDE)

#new tables with buildingIDs substituted by the predicted ones
wifitestlonpred <- wifitestfloorpred
wifitestlonpred$LONGITUDE <- testPredRFlon
wifitrainlonpred <- wifitrainfloorpred
wifitrainlonpred$LONGITUDE <- trainPredRFlon

#plots for comprobation of acuracy
ggplot(wifitestlonpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainlonpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)

#---------------------RF LATITUDE
#train RF
RFlat <- train(LATITUDE~. -LONGITUDE -RELATIVEPOSITION -SPACEID -USERID -PHONEID -TIMESTAMP, data = wifitrainlonpred, 
                method = "RF", trControl=Control_CV, tuneLength = 6)
RFlat
saveRDS(KNNlat, file = "RFlatn.RDS")
RFlat <- readRDS("RFlatn.RDS")

predictors(RFlat)

#make predictions
trainPredRFlat <- predict(RFlat, wifitrainlonpred)
testPredRFlat <- predict(RFlat, wifitestlonpred)

#performace measurment
postResample(testPredRFlat, wifitestlonpred$LATITUDE)

#plot predicted verses actual
plot(testPredRFlat,wifitestlonpred$LATITUDE)

#new tables with buildingIDs substituted by the predicted ones
wifitestlatpred <- wifitestlonpred
wifitestlatpred$LATITUDE <- testPredRFlat
wifitrainlatpred <- wifitrainlonpred
wifitrainlatpred$LATITUDE <- trainPredRFlat

#plots for comprobation of acuracy
ggplot(wifitestlatpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR ))  # + facet_grid(BUILDINGID~FLOOR)
ggplot(wifitrainlatpred) + geom_point(aes(x=LONGITUDE, y= LATITUDE, shape = BUILDINGID, color = FLOOR )) # + facet_grid(BUILDINGID~FLOOR)




