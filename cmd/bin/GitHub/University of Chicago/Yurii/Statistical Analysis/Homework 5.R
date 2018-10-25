dat<- Week5_Test_Sample
plot(dat$Input,dat$Output,type="p", pch=19)

dat<-dat[,c(2,1)]
head(dat,3)

nSample<-length(dat$Input)

m1<-lm(dat$Output~dat$Input)
m1$coefficients

matplot(dat$Input,cbind(dat$Output,m1$fitted.values),type="p",pch=16,ylab="Sample and Fitted Values")

summary(m1)

estimatedResiduals<-m1$residuals
plot(dat$Input,estimatedResiduals)

Probability.Density.Residuals<-density(estimatedResiduals)
plot(Probability.Density.Residuals,ylim=c(0,.7))
lines(Probability.Density.Residuals$x,
      dnorm(Probability.Density.Residuals$x,mean=mean(estimatedResiduals),sd=sd(estimatedResiduals)))

require(dplyr)


Train.Sample<-data.frame(trainInput=dat$Input,trainOutput=rep(NA,nSample))
Train.Sample.Steeper<-data.frame(trainSteepInput=dat$Input,
                                 trainSteepOutput=rep(NA,nSample))  
Train.Sample.Flatter<-data.frame(trainFlatInput=dat$Input,
                                 trainFlatOutput=rep(NA,nSample))  


##Train.Sample.Selector<-filter(dat,Input<=-1.5)
Train.Sample.Selector<-dat$Input<=-1.5
Train.Sample.Steeper.Selector<-Train.Sample.Selector&
  (dat$Output<m1$fitted.values)
Train.Sample.Flatter.Selector<-Train.Sample.Selector&
  (dat$Output>=m1$fitted.values)

Train.Sample[Train.Sample.Selector,2]<-dat[Train.Sample.Selector,2]
Train.Sample.Steeper[Train.Sample.Steeper.Selector,2]<-dat[Train.Sample.Steeper.Selector,2]
Train.Sample.Flatter[Train.Sample.Flatter.Selector,2]<-dat[Train.Sample.Flatter.Selector,2]

head(cbind(dat,
           Train.Sample,
           Train.Sample.Steeper,
           Train.Sample.Flatter),10)

plot(Train.Sample$trainInput,Train.Sample$trainOutput,pch=16,ylab="Training Sample Output",
     xlab="Training Sample Input")
points(Train.Sample.Steeper$trainSteepInput,Train.Sample.Steeper$trainSteepOutput,pch=20,col="green")
points(Train.Sample.Flatter$trainFlatInput,Train.Sample.Flatter$trainFlatOutput,pch=20,col="blue")

Train.Sample.Steep.lm<-lm(Train.Sample.Steeper$trainSteepOutput~Train.Sample.Steeper$trainSteepInput)
Train.Sample.Flat.lm<-lm(Train.Sample.Flatter$trainFlatOutput~Train.Sample.Flatter$trainFlatInput)


plot(dat$Input,dat$Output, type="p",pch=19)
lines(dat$Input,predict(Train.Sample.Steep.lm,
                        data.frame(trainSteepInput=dat$Input),
                        interval="prediction")[,1],col="red",lwd=3)
lines(dat$Input,predict(Train.Sample.Flat.lm,data.frame(trainFlatInput=dat$Input),
                        interval="prediction")[,1],col="green",lwd=3)


rbind(Steeper.Coefficients=Train.Sample.Steep.lm$coefficients,
      Flatter.Coefficients=Train.Sample.Flat.lm$coefficients)


Distances.to.Steeper<-abs(dat$Output-
                            dat$Input*Train.Sample.Steep.lm$coefficients[2]-
                            Train.Sample.Steep.lm$coefficients[1])
Distances.to.Flatter<-abs(dat$Output-
                            dat$Input*Train.Sample.Flat.lm$coefficients[2]-
                            Train.Sample.Flat.lm$coefficients[1])

Unscrambling.Sequence.Steeper<-Distances.to.Steeper<Distances.to.Flatter

Subsample.Steeper<-data.frame(steeperInput=dat$Input,steeperOutput=rep(NA,nSample))
Subsample.Flatter<-data.frame(flatterInput=dat$Input,flatterOutput=rep(NA,nSample))
Subsample.Steeper[Unscrambling.Sequence.Steeper,2]<-dat[Unscrambling.Sequence.Steeper,2]
Subsample.Flatter[!Unscrambling.Sequence.Steeper,2]<-dat[!Unscrambling.Sequence.Steeper,2]
head(Subsample.Flatter,50)
matplot(dat$Input,cbind(dat$Output,
                        Subsample.Steeper$steeperOutput,
                        Subsample.Flatter$flatterOutput),
        type="p",col=c("black","green","blue"),
        pch=16,ylab="Separated Subsamples")

head(Subsample.Steeper,3)
Linear.Model.Steeper.Recovered<-lm(steeperOutput~steeperInput,Subsample.Steeper)
Linear.Model.Flatter.Recovered<-lm(flatterOutput~flatterInput,Subsample.Flatter)

matplot(dat$Input,cbind(c(summary(Linear.Model.Steeper.Recovered)$residuals,
                          summary(Linear.Model.Flatter.Recovered)$residuals),
                        estimatedResiduals),type="p",pch=c(19,16),ylab="Residuals before and after unscrambling")
legend("bottomleft",legend=c("Before","After"),col=c("red","black"),pch=16)


unmixedResiduals<-c(summary(Linear.Model.Steeper.Recovered)$residuals,
                    summary(Linear.Model.Flatter.Recovered)$residuals)
apply(cbind(ResidualsAfter=unmixedResiduals,
            ResidualsBefore=estimatedResiduals),2,sd)

qqnorm(unmixedResiduals)
qqline(unmixedResiduals)

suppressWarnings(library(fitdistrplus))
hist(unmixedResiduals)

plot(dat$Input,(dat$Output-mean(dat$Output))^2, type="p",pch=19,
     ylab="Squared Deviations")

plot(dat$Input,(dat$Output-mean(dat$Output))^2, type="p",pch=19,
     ylab="Squared Deviations")
points(dat$Input,clusteringParabola,pch=19,col="red")

Linear.Model.Flatter.Recovered
res$mFlat
result2$mFlat
Train.Sample.Steep.lm


res<-list(GeneralModel=m1,mSteep=Linear.Model.Steeper.Recovered, mFlat=Linear.Model.Flatter.Recovered )
saveRDS(res, file = paste("C:/Users/u353822/Documents/R/Statistical Analysis",'result.rds',sep = '/'))










head(trebuchetData)






trebuchetData<-`documents%2FMScA.Statistical.Analysis.31007%2FMScA.31007.Lecture.6%2FtrebuchetData`



trebModel<-lm(distance~projectileWt, data=trebuchetData)
anova(trebModel)

SSE<-sum((trebModel$residuals)^2)
SSM<-sum(((trebModel$fitted.values - mean(trebModel$fitted.values)))^2)
F<-((SSM/1)/(SSE/14))
R.sqrd<-(1-(SSE/(SSM+SSE)))
R.sqrd.adj<-(1-(SSE/14)/((SSE+SSM)/15))
  
SST
F
SSM
R.sqrd
R.sqrd.adj


taste.model<-lm(score~scr,data=tastedata)
tastedata$score[1:8]

mean(tastedata$score[1:8])
