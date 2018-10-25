

set.seed(847337465)
Binomial.sample<-rbinom(500,size=20,prob=.7)
Binomial.sample

supressWarnings(Library(fitdistrplus))
Binomial.fit<-fitdist(Binomial.sample,dist())
Binomial.fit<-fitdist(Binomial.sample,dist="binom",fix.arg=list(size=20), start=list(prob=0.5))
c(Binomial.fit$estimate,sd=Binomial.fit$sd)

pbinom(6923,10000,.7)+pbinom(7077,10000,.7,lower.tail = F)

#try pnorm when you get home

pnorm(6923, mean=7000, sd=sqrt(7000*.3))*2

binom.test(sum(Binomial.sample), 500*20,p=.7)

install.packages("MASS")
library('Mass')
set.seed(847337465)
Poisson.sample<-rpois(500,7)
mean(Poisson.sample)


dataPath <- "C:/Users/u353822/Desktop/Statistical Analysis Class/"
df <- read.table(paste0(dataPath, 'Week3_Test_Sample.csv'), header=T)
SDY<-sd(df$Y)
SDX<-sd(df$X)
r<-cor(df$X,df$Y)
plot(df)
a<-r*(SDY/SDX)
a
b<-mean(df$Y)-a*mean(df$X)
b


lmdf<-lm(df$Y~df$X,data=df)
##summary(lmdf)


mean(summary(lmdf)$residuals) 

##summary(lmdf)$coefficients

lmdf

resid<-(lmdf$residuals-mean(lmdf$residuals))^2
sqrt(sum(resid)/lmdf$df.residual)

hist(lmdf$residuals)


1.0

##?review how to make a function
u<-c(1.5,sqrt(3)/2)
v<-c(2,0)
(Dot.Product.2<-u%*%v)

((u%*%v) / (abs(v)%*%abs(v)))%*%v
((u%*%v) / (abs(v)))

##2.0
M


da<-LognormalSample.csv$x


mean<-mean(da)
median<- median(da)
mu=log(median)
sqrt(2*(log(mean)-mu))

library(MASS)
library(dplyr)



Sample4<-(Week4_Test_Sample)

Sample4

lm_Sample4<-lm(Sample4$Y~Sample4$X,data=Sample4)
lm_Sample4

hist(lm_Sample4$residuals)
summary(lm_Sample4)$sigma^2
var((lm_Sample4$residuals))


residuals_lm4<- lm_Sample4$residuals
plot(density(residuals_lm4))


c(Left.Mean = mean(residuals_lm4[residuals_lm4 < 0]), 
  Right.Mean = mean(residuals_lm4[residuals_lm4 > 0]))

residuals_lm4

Sample4
newframe <- cbind(Sample4$Y,Sample4$X)

Unscrambled.Selection.Sequence<-residuals_lm4>0
Unscrambled.Selection.Sequence<-Unscrambled.Selection.Sequence*1
Unscrambled.Selection.Sequence

res <- list(Unscrambled.Selection.Sequence =  Unscrambled.Selection.Sequence)
write.table(res, file = paste("C:/Users/u353822/Documents/R/Statistical Analysis",'result.csv',sep = '/'), row.names = F)

LinearModel1<-(cbind(Sample4$X*Unscrambled.Selection.Sequence,Sample4$Y*Unscrambled.Selection.Sequence))
LinearModel1[LinearModel1==0]=NA
LinearModel1
LinearModel2<-cbind(Sample4$X*(1-Unscrambled.Selection.Sequence),Sample4$Y*(1-Unscrambled.Selection.Sequence))
LinearModel2[LinearModel2==0]=NA
LinearModel2

cbind(LinearModel1,LinearModel2)

matplot(Sample4$X, cbind(LinearModel1[, 2], LinearModel2[,2]), 
        type = "p", col = c("green", "blue"), pch = 19, ylab = "Separated Subsamples")


res <- list(Unscrambled.Selection.Sequence =  Unscrambled.Selection.Sequence)

Unscrambled.Selection.Sequence
newframe
residuals_lm4

newframe[residuals_lm4<0,]<- c(NA,NA)

cbind(Sample4$X,Sample4$Y,newframe[residuals_lm4<0,])




cbind(LinearModel1.Recovered,LinearModel2.Recovered)


head(cbind(Sample4$Input,Sample4$Input,ifelse(residuals_lm4 <= 0, 1, NA)), n=4)
cbind(Sample4$Input,ifelse(residuals_lm4 <= 0, 1, NA)

matrix(data=c(Sample4$Input,LinearModel1.Recovered),ncol=2)
plot(Sample4)

head(cbind(LinearModel1.Recovered,LinearModel2.Recovered),30)



?matrix
matrix(data=c(LinearModel1.Recovered,LinearModel1.Recovered), ncol=4)



