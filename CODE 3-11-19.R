#####LINEAR REGRESSION MODELS#####
##################################

library(datasets)
View(mtcars)

pairs(mtcars[1:4])

MODEL1<-lm(mpg~hp, mtcars)
summary(MODEL1)  ##summarize model results
str(MODEL1)  ##summarizes structure of object
class(MODEL1) ##returns class of object

##Verifying the pseudoinverse method
dim(mtcars) ##checks dimension of mtcars dataset

##adds column of ones to the input data and changes the class from data.frame to matrix
X<-cbind(as.matrix(rep(1,times=dim(mtcars)[1])), as.matrix(mtcars)[,2:11])  ##creates the X matrix and adds a column of 1's for the intercept term

##changes the class of the output variable to a matrix class
y<-as.matrix(mtcars[,1])

View(X)  

##NOTE THE solve() function is how R computes an inverse matrix
##the t() function is used to take the transpose of a matrix
##the expression %*% is uses to multiply two matrix objects together
PseudoInverse<-solve(t(X)%*%X)%*%t(X)  
dim(PseudoInverse)  ##check dimensions

Beta<-PseudoInverse%*%y  ##recovers the optimal parameter vector
Beta  ##reports the Beta vector
MODEL1<-lm(mpg ~., mtcars)  ##regresses mpg on ALL variables
MODEL1$coefficients  ##looks at the coefficient vector (BETA) to compare to the pseudoinverse method

MODEL2<-lm(mpg~hp, mtcars)
summary(MODEL2)

MODEL3<-lm(mpg~log(hp), mtcars)
summary(MODEL3)

MODEL4<-lm(log(mpg)~log(hp), mtcars)
summary(MODEL4)

MODEL5<-lm(mpg~I(hp)+I(hp^2), mtcars)
summary(MODEL5)

MODEL6<-lm(mpg~poly(hp, 2, raw=TRUE), mtcars) ##alternatively
summary(MODEL6)
