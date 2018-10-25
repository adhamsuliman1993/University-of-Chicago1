

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

plot(lmdf)
mean(summary(lmdf)$residuals) 

##summary(lmdf)$coefficients

lmdf

resid<-(lmdf$residuals-mean(lmdf$residuals))^2
sqrt(sum(resid)/lmdf$df.residual)

hist(lmdf$residuals)
## ITS NOT NORMAL
