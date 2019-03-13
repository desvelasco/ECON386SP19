##################################
#####LINEAR REGRESSION MODELS#####
##################################
##########3-11-19#################
##################################

library(datasets)
View(mtcars)
dim(mtcars)
pairs(mtcars[1:4]) #look at only the first 4 variables in the pairwise plot

MODEL0<-lm(mpg~hp, mtcars)  ##simple univariate model: mpg=B_0+B_1*hp+u
summary(MODEL0)  ##summarize model results

##we could also manually compute the coefficients for the univariate input case
B_1<-cov(mtcars$mpg,mtcars$hp)/var(mtcars$hp)
B_0<-mean(mtcars$mpg)-B_1*mean(mtcars$hp)

MODEL1<-lm(mpg~0+hp,mtcars) ##regression through the origin (no intercept term!)
summary(MODEL1)

str(mtcars) ##summarizes structure of the data.frame object
mtcars$mpg  ##grabs the mpg variable/element of the mtcars object
str(MODEL1)  ##summarizes structure of the model object
hist(MODEL1$residuals) ##plots the residuals by pulling them from the model object
summary(MODEL1$residuals) ##summary stats for the residuals

x<-1  ##numeric variab;le
y<-"Steve"  ##character variable
class(x) ##returns object class
class(y) ##returns object class
class(MODEL1) ##returns object class
class(mtcars)  ##returns object class
M1<-as.matrix(mtcars)  ##converts the data.frame object into a matrix object
class(M1)

######################################
##Verifying the pseudoinverse method##
######################################

dim(mtcars) ##checks dimension of mtcars dataset

VecOfOnes<-rep(1,times=dim(mtcars)[1]) ##generates a vector of ones that has same length as rows in dataset

##adds column of ones to the input data and changes the class from data.frame to matrix
X<-cbind(as.matrix(VecOfOnes), as.matrix(mtcars)[,2:11])  ##creates the X matrix and adds a column of 1's for the intercept term
View(X)  
dim(X)

mtcars[1,1] ##pulls the first row and first column elements form the dataset
mtcars[,1] #pulls all rows from the first column of a variable
mtcars[1,]  #pulls all columns from the first raw of data

y<-as.matrix(mtcars[,1]) ##changes the class of the output variable to a matrix class
View(y)

##NOTES: the solve() function is how R computes an inverse matrix
##the t() function is used to take the transpose of a matrix
##the expression %*% is uses to multiply two matrix objects together

PseudoInverse<-solve(t(X)%*%X)%*%t(X)  
dim(PseudoInverse)  ##check dimensions

Beta<-PseudoInverse%*%y  ##recovers the optimal parameter vector
Beta  ##reports the Beta vector

MODEL_A<-lm(mpg ~., mtcars)  ##regresses mpg on ALL variables linearly
MODEL_A$coefficients  ##looks at the coefficient vector (BETA) to compare to the pseudoinverse method

#############################################
###Incorporating nonlinear transformations###
#############################################
MODEL2<-lm(mpg~log(hp), mtcars)  ##level-log model
summary(MODEL2)

MODEL3<-lm(log(mpg)~hp, mtcars) ##log-level model
summary(MODEL3)

MODEL4<-lm(log(mpg)~log(hp), mtcars) ##log-log model
summary(MODEL4)

MODEL5<-lm(mpg~I(hp)+I(hp^2), mtcars)  ##polynomial regression of order 2
summary(MODEL5)

MODEL6<-lm(mpg~poly(hp, 2, raw=TRUE), mtcars) ##alternative way of doing MODEL5
summary(MODEL6)

MODEL7<-lm(mpg~I(hp)+I(hp*cyl)+I(cyl), mtcars)  ##include an interaction term between variables
summary(MODEL7)

MODEL8<-lm(mpg~am, mtcars)  ##includes a dummy variable: am (automatic or manual transmission)
summary(MODEL8)
