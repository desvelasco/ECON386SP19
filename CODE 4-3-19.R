###CODE 4-3-19#####################
###PREDICTION WITH LINEAR MODELS###
###################################

data<-read.table('C:/Users/Steve/Desktop/ECON386SP19/power_plants.txt', header=TRUE)
View(data)

plot(data$SO2~data$CoalHeat, xlim=c(0,150), ylim=c(0,1200))
MODEL_0<-lm(SO2~CoalHeat, data)
abline(MODEL_0, col='red', lwd=2)

PRED_0<-predict(MODEL_0, data)
plot(PRED_0~data$CoalHeat, xlim=c(0,150), ylim=c(0,1200))
abline(MODEL_0, col='red', lwd=2)

plot(MODEL_0$fitted.values~data$CoalHeat)
abline(MODEL_0, col='red', lwd=2)

sum(MODEL_0$fitted.values-PRED_0)

#######################################################
###partitioning the data into training and test sets
###and comuting in-sample and estimated out-of-sample error
###using the test data
#######################################################

trainingData<-subset(data, data$Year<95)  #creates training data from pre-1995 observations
dim(trainingData)
testData<-subset(data, data$Year==95)   #uses the 1995 observations as the test data
dim(testData)
MODEL_1<-lm(SO2~CoalHeat, trainingData)
PRED_1<-predict(MODEL_1, testData)
plot(PRED_1~testData$CoalHeat )
abline(MODEL_1, col='red', lwd=2)

##computing E_IN and estimating E_OUT##
##using the root mean squared error measure 
##with (n-k) degrees of freedom where k=2 in our case

E_IN_1<-(sum(MODEL_1$residuals^2)/(length(MODEL_1$residuals)-2))^(1/2)
E_OUT_1<-(sum(PRED_1-testData$SO2)^2/(length(testData$SO2)-2))^(1/2)
E_IN_1
E_OUT_1

MODEL_2<-lm(SO2~CoalHeat+OilHeat+GasHeat+Energy, trainingData)
PRED_2<-predict(MODEL_2, testData)

E_IN_2<-(sum(MODEL_2$residuals^2)/(length(MODEL_2$residuals)-5))^(1/2)
E_OUT_2<-(sum(PRED_2-testData$SO2)^2/(length(testData$SO2)-5))^(1/2)
E_IN_2
E_OUT_2
summary(MODEL_2)
