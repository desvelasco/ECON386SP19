###################################
###CODE FOR OLD EXAM LAB PORTION###
###################################

##########
##TASK I##
##########

fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destination<-"storm.csv.bz2"
##be patient - this takes some time to load
download.file(fileURL, destfile = destination)
storm<-read.csv("storm.csv.bz2") ##takes some time

##renames variables
storm$EventType<-storm$EVTYPE
storm$Fatalities<-storm$FATALITIES
storm$Injuries<-storm$INJURIES
storm$PropertyDamage<-storm$PROPDMG
storm$CropDamage<-storm$CROPDMG

##aggregates by mean for each event type
aggpropdmg<-aggregate(PropertyDamage~EventType, storm, mean)
aggcropdmg<-aggregate(CropDamage~EventType, storm, mean)
aggfatalities<-aggregate(Fatalities~EventType, storm, mean)
agginjuries<-aggregate(Injuries~EventType, storm, mean)

##sorts event types by descending order
toppropdmg<-aggpropdmg[order(-aggpropdmg$PropertyDamage),]
topcropdmg<-aggcropdmg[order(-aggcropdmg$CropDamage),]
topfatalities<-aggfatalities[order(-aggfatalities$Fatalities),]
topinjuries<-agginjuries[order(-agginjuries$Injuries),]

##aggregates across property and crop damages to estimate total economic consequence and sorts in descending aggecondmg<-aggpropdmg
aggecondmg<-aggpropdmg
aggecondmg$CropDamage<-aggcropdmg$CropDamage

aggecondmg$TotalEconomicDamages<-aggpropdmg$PropertyDamage+aggcropdmg$CropDamage
topecondmg<-aggecondmg[order(-aggecondmg$TotalEconomicDamages),]


#### question #1 ####
sum(is.na(storm$F))/dim(storm)[1]

#### question #2 ####
head(topinjuries)

#### question #3 ####
head(topecondmg)

#### question #4 ####
head(topfatalities)

#############
###TASK II###
#############

data_4<-read.table('powerplants.txt', header=TRUE)

#### question #5 ####
plot(Energy~GrossCons, data_4)
lm1<-lm(Energy~GrossCons, data_4)
summary(lm1)
lm2<-lm(Energy~0+GrossCons, data_4)
summary(lm2)

#### question #6 ####
mean(data_4$SO2[data_4$PhaseI==1])-mean(data_4$SO2[data_4$PhaseI==0])

#### question #7 ####

lm3<-lm(Energy[data_4$Year==91]~0+CoalHeat[data_4$Year==91]+OilHeat[data_4$Year==91]+GasHeat[data_4$Year==91], data_4)

#### question #8 ####
lm4<-lm(NOX~0+CoalHeat+OilHeat+GasHeat, data_4)
mean(lm4$residuals)

##############
###TASK III###
##############

library(datasets)
X<-cbind(as.matrix(rep(1,times=dim(mtcars)[1])), as.matrix(mtcars)[,2:11])  ##creates the X matrix and adds a column of 1's for the intercept term
y<-as.matrix(mtcars[,1])

#### question #9 ####
PseudoInverse<-solve(t(X)%*%X)%*%t(X)
PseudoInverse[5,5]

#### question #10 ####
Beta<-PseudoInverse%*%y
Beta[4]
lm5<-lm(mpg ~., mtcars)  ##to check##
lm5$coefficients[4]  ##to check##
