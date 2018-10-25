rm(list=ls())
if(!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}
pacman::p_load(tidyverse,faraway,corrplot, tinytex, dplyr,zoo, 
          knitr, rgl, relaimpo, MuMIn, broom,ggplot2, tidyr, fields, cluster, data.table, reshape2,poLCA, stats, RNetCDF,
          MAss, mclust, nor1mix, coupla, mclust)
library('RNetCDF')
library('MASS', 'mclust', 'nor1mix', 'copula')
library('copula')
library('mclust')
library(nor1mix)
Data <- read.csv('W3CorrelationExample.csv')
plot(Data$X,Data$Y)
hist(Data$X)
hist(Data$Y)

nSample <- nrow(Data)
Gaussian.Copula.0.9<-normalCopula(param=.9,dim=2)



(fittedDistrX<-fitdistr(Data$Y,"exponential"))
ks.test(Data$Y,"qnorm",mean=0.33973523, sd=0.01074337)

lm.Data <- lm(Y~X, Data)
summary(lm.Data)$r.squared

plot(Data$X, lm.Data$residuals)
summary(lm.Data)

#CounterCommonocity 
plot(rank(Data$X)/nSample,rank(Data$Y)/nSample)
cor(rank(Data$X)/nSample,rank(Data$Y)/nSample) #-.5255


#1.1 Example of uncorrelated, but dependent variables
nSample<-1000
set.seed(893075)
Variable.X<-rnorm(nSample,0,1)
Variable.Y<-Variable.X^2
df<-data.frame(X=Variable.X,Y=Variable.Y)
plot(df$X,df$Y)
plot(rank(df$X)/nSample,rank(df$Y)/nSample)

#1.2 The pattern may be explained by marginal distributions rather than dependence between the variables

#Sample Homework Data 
Age.Time.Sample<-read.csv(("FWeek3_Homework_Project_Data.csv"))
Age.Time.Sample<-as.matrix(Age.Time.Sample)
Age.Time.Sample[1:10,]
# Shows that there are higher conentrations of age groups that watch a higher frequency of tv 
# between the .10 and .15 scale for age and .17 to .25 age groups.
hist(Age.Time.Sample[,1])
hist((Age.Time.Sample[,1])/length(Age.Time.Sample[,1]+1))
# Normal distribution with left skew
hist((Age.Time.Sample[,2])/length(Age.Time.Sample[,2]+1))

#Data looks scattered
plot(rank(Age.Time.Sample[,1])/length(Age.Time.Sample[,1]+1),
     rank(Age.Time.Sample[,2])/length(Age.Time.Sample[,1]+1),xlab="Age",ylab="Time")

#Low Correlation
c(Correlation=cor(Age.Time.Sample)[1,2],Determination=cor(Age.Time.Sample)[1,2]^2)


Age.Clusters <- Mclust(Age.Time.Sample[,1], modelName="V")
Time.Clusters <- Mclust(Age.Time.Sample[,2])

#Still need to do nor1mix at home
Age.Clusters.Parameters<-rbind(mu=Age.Clusters$param$mean,
                               sigma=sqrt(Age.Clusters$param$variance$sigmasq),pro=Age.Clusters$param$pro)

norMix(Age.Clusters.Parameters)

#1 clusters because it's a normal with left skew
Time.Clusters$param

Time.Clusters<-Mclust(Age.Time.Sample[,2], G=2,modelNames=c("V"))









##Homework
test_data  <- read.table('Week3_Test_Sample.csv', head= T)

# X is a heavy exponential with slight right skew
hist(test_data$x_sample)
# X when scaled down looks completely random
plot(rank(test_data[,1])/length(test_data[,1]+1))
plot(pobs(test_data[,1]))
plot(qnorm(test_data[,1]),na.rm=T)
(x_mean <- mean(test_data[,1]))
(x_var<- var(test_data[,1]))

# Y is a normal with mea which seams to be at 0
hist(test_data$y_sample)
# Y when scaled down looks completely random
plot(rank(test_data[,2])/length(test_data[,2]+1))
plot(qnorm(test_data[,2]),rm.na=T)
(y_mean <- mean(test_data[,2]))
(y_var<- var(test_data[,2]))


#either Frank or Gaussian
plot(rank(test_data[,1])/length(test_data[,1]+1), rank(test_data[,2])/length(test_data[,2]+1))

#Pearson rho = .1779
(cor(test_data))
#Kendall rho = .5149
(cor(test_data, method = "kendall"))
# Spearmans rho = .7129
(cor(test_data, method = "spearman"))

??pobs
#test for Gaussian Copula
#Why are we setting param = 0??
Fitting.Copula <- cbind(pobs(test_data[,1], ties.method = "average"),pobs(test_data[,2],  ties.method = "average"))
Gaussian.Copula.Object<-normalCopula(dim=2)
Gaussian.Copula.fit<-fitCopula(Gaussian.Copula.Object, 
                               pobs(Fitting.Copula,ties.method = "average"), 
                               method = "ml",
                               optim.method = "BFGS", 
                               optim.control = list(maxit=1000))
Gaussian.Copula.fit


#Test for Frank Copula, and the winner is FRANKKKKKKKKKKKKKKKKKKKK 
Frank.Copula.Object <- frankCopula(dim=2)
Frank.Copula.Fit <- fitCopula(Frank.Copula.Object,
                               pobs(Fitting.Copula, ties.method = "average"), 
                               method = "ml",
                               optim.method = "BFGS", 
                               optim.control = list(maxit=1000))
Frank.Copula.Fit@estimate

#fitCopula(frankCopula(), Fitting.Gaussian.Copula, method="mpl")
copula.type <- "Frank"
parameter <- coef(Frank.Copula.Fit)
test_data_scaled <- pobs(test_data)
plot(lm(test_data_scaled[,2]~test_data_scaled[,1]))


#Test for Clayton
Clayton.Copula.Object <- claytonCopula(dim=2)
Clayton.Copula.Fit <- fitCopula(Clayton.Copula.Object,
                              pobs(Fitting.Copula, ties.method = "average"), 
                              method = "ml",
                              optim.method = "BFGS", 
                              optim.control = list(maxit=1000))
Clayton.Copula.Fit


x_ranks<- rank(test_data[,1])/nrow(test_data[,1] + 1)
length(test_data[,1]+1)+1
copula <- data.frame(rank(test_data[,1])/(length(test_data[,1])+1), rank(test_data[,2])/(length(test_data[,2])+1))
copula[,2]

theta = Frank.Copula.Fit@estimate
alpha=.5
midBound <- sapply(copula[,1], function(z)
  -log(1-alpha*(1-exp(-theta))/(exp(-theta*z)+alpha*(1-exp(-theta*z))))/theta)



midBound <- midBound
midBound

#plot's of data
#plot(rank(test_data[,1])/length(test_data[,1]+1), rank(test_data[,2])/length(test_data[,2]+1))
#points(sort(rank(test_data[,1])/length(test_data[,1])), sort(midBound), col ='green')
copula
ggplot(copula, aes(x=copula[,1], y=copula[,2]))+
  geom_point() + geom_smooth(aes(x= copula[,1], y=midBound, color='green'))

length(copula[,1])
length(midBound)
res

res <- list (copula.type = c("Frank"),
             parameter = c(Frank.Copula.Fit@estimate) ,
             quantile = c(midBound))

res
saveRDS(res, file = paste('result.rds',sep = '/'))

#not so standard deviation
#look up recipe package 
#steps 







