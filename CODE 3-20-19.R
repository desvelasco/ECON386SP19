##MOBLAB #8 SOLUTIONS##
#1) 
mean(mtcars$mpg[mtcars$am==1])-mean(mtcars$mpg[mtcars$am==0])
#2) mpg=beta*am
Model1<-lm(mpg~0+am,mtcars)
summary(Model1)
#3) mpg=beta_0+beta_1*am
Model2<-lm(mpg~am,mtcars)
summary(Model2)
#4) mpg=beta_0+beta_2*am+beta_3*wt
Model3<-lm(mpg~am+wt,mtcars)
summary(Model3)

##################################
######PART I:  SIMULATION#########
######CODE 3-20-19.R##############
##################################

##to simulate 100 draws from the standard normal distribution:
sample1<-rnorm(n=100, mean=0, sd=1)  
?rnorm
##plot histogram
hist(sample1, breaks = 30, prob = TRUE, xlim=c(-4,4), ylim=c(0,.7))

##add density curve to diagram
curve(dnorm(x, mean = 0, sd = 1), col = "darkblue", lwd = 2, add = TRUE)
##dnorm() is the probability density function

##resampling with a much larger sample and plotting
sample2<-rnorm(n=100000, mean=0, sd=1) 
hist(sample2, breaks = 30, prob = TRUE, xlim=c(-4,4), ylim=c(0,.7))
curve(dnorm(x, mean = 0, sd = 1), col = "darkblue", lwd = 2, add = TRUE)

##pnorm() is the cumulative distribution function, F(x), for the normal distribution
##use this instead of using Z-score table from your stats book!
pnorm(0,mean=0, sd=1)  ## P(X<0)
pnorm(2,0,1)  ##P(X<2)
pnorm(1,0,1)-pnorm(-1,0,1)  ## P(X is within 1 sd of the mean)
pnorm(2,0,1)-pnorm(-2,0,1)  ## P(X is within 2 sd of the mean)
pnorm(3,0,1)-pnorm(-3,0,1)  ## P(X is within 2 sd of the mean)

################################################
###simulating a DGP for regression estimation###
################################################

##sample the error term
set.seed(1234)  ##locks in a particular set of random numbers
n=100 ##sample size
sigma=1  ##error standard deviation
e<-rnorm(n,0,sigma)  ##draws normally distributed error
View(e)
x_data<-runif(n,-1,1)  ##generates synthetic x (input) data
B_0=3  ##population intercept term
B_1=1/2  ##population slope term
y_data<-B_0+B_1*x_data+e  ##generates simulated y values from the DGP
plot(x_data, y_data)  ##plot the data
abline(B_0,B_1, col='red')  ##add the population regression line

data<-cbind(y_data, x_data)  ##bind the columns of the y and x data together
df1<-as.data.frame(data)  ##convert to dataframe object
View(df1)  ##view newly created dataframe

M1<-lm(y_data~x_data, df1)  ##estimate linear model
summary(M1)
abline(M1$coefficients[1],M1$coefficients[2], col='blue')


#################################################################
####Part II:  using the ggplot2 package implementation ##########
#################################################################

##need to install these packages before running libraries
library(plyr)  ##adds the plyr library which is useful for data manipulation
library(ggplot2)  ##adds the ggplot2 library package (different system than the BASE package)
?ggplot  ##pulls up the ggplot function documentation

##generate synthetic data
n1=100
n2=100
d1<-rnorm(n1,-2, 10) ##data from group 1
d2<-rnorm(n2,10, 5)  ##data from group 2
##plot each group
hist(d1, breaks = 30, prob = TRUE, xlim=c(-50,50), ylim=c(0,.1))
hist(d2, breaks = 30, prob = TRUE, xlim=c(-50,50), ylim=c(0,.1))
Z1<-as.matrix(c(d1,d2))  ##stack vectors
ones_vec<-rep(1, n1)  ##creates dummy variables for group 1
zeros_vec<-rep(0,n2)  ##creates dummy variables for group 2
Z2<-as.matrix(c(ones_vec, zeros_vec))  ##stacks vectors
View(Z2)
df2<-as.data.frame(cbind(Z1, Z2))  ##creates data frame
View(df2)
colnames(df2)<-c("var1", "dummy")  ##renames columns
##to rename rows, use rownames()

##histogram plot in the ggplot2 package
plot_a<-ggplot(df2,aes(x=var1)) +
  geom_histogram(aes(y=..density..), position = "identity")
plot_a

##add smooth, nonparametric kernel density estimator
plot_b<-ggplot(df2,aes(x=var1)) +
  geom_histogram(aes(y=..density..), position = "identity") +
  geom_density(alpha = .5) ##adds the density function for each group - the alpha parameter controls how transparent the coloring is
plot_b

##histogram plot colored by group
plot_c<-ggplot(df2,aes(x=var1, fill = factor(df2$dummy))) +
  geom_histogram(aes(y=..density..), position = "identity")
plot_c

##historgram plot by group with smooth density function overlay
plot_d<-ggplot(df2,aes(x=var1, fill = factor(df2$dummy))) +
  geom_histogram(aes(y=..density..), position = "identity") +
  geom_density(alpha = .5) ##adds the density function for each group - the alpha parameter controls how transparent the coloring is
plot_d

##historgram plot by group with smooth density function overlay and vertical lines for means for each group
means <- ddply(df2, "dummy", summarise, meanvar1=mean(var1))
plot_e<-ggplot(df2,aes(x=var1, fill = factor(df2$dummy))) +
  geom_histogram(aes(y=..density..), position = "identity") +
  geom_density(alpha = .5) +
  geom_vline(data=means, aes(xintercept=meanvar1, col=factor(means$dummy)), linetype="dashed", size=1)  ##adds vertical line at the mean of each group
plot_e

