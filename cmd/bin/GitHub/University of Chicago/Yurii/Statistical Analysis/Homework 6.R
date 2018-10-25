---
title: "assignment 6 statistical analysis"
output: html_document
---

```{r}
library(dplyr)
LinearModel.Training<-read.table("C:/Users/u353822/Downloads/Week6_Test_Sample_Train.csv",head=T)
LinearModel.Training<- LinearModel.Training %>%
  select(Input, Output, Selection.Sequence)
nSample.Training<-length(LinearModel.Training[,1])
head(LinearModel.Training)
plot(LinearModel.Training[,1],LinearModel.Training[,2], type="p", pch=19)
```


```{r}
LinearModel.Training.1<-cbind(LinearModel.Training[,1],rep(NA,nSample.Training))
LinearModel.Training.2<-cbind(LinearModel.Training[,1],rep(NA,nSample.Training))
LinearModel.Training.1[LinearModel.Training[,3]*(1:nSample.Training),2]<-
  LinearModel.Training[LinearModel.Training[,3]*(1:nSample.Training),2]
LinearModel.Training.2[(1-LinearModel.Training[,3])*(1:nSample.Training),2]<-
  LinearModel.Training[(1-LinearModel.Training[,3])*(1:nSample.Training),2]

head(cbind(LinearModel.Training,
           Trainig1=LinearModel.Training.1[,2],
           Training2=LinearModel.Training.2[,2]))

matplot(LinearModel.Training[,1],cbind(LinearModel.Training.1[,2],LinearModel.Training.2[,2]),
        pch=16,col=c("green","blue"),ylab="Subsamples of the training sample")
```

## Including Plots

You can also embed plots, for example:

```{r }
EstimatedLinearMOdel.Training<-lm(Output~Input,LinearModel.Training)
summary(EstimatedLinearModel.Training)$coefficients
summary(EstimatedLinearModel.Training)$r.squared
summary(EstimatedLinearModel.Training)$sigma
EstimatedResiduals.Training<-EstimatedLinearModel.Training$residuals
plot(LinearModel.Training[,1],EstimatedResiduals.Training)
```
```{r}
EstimatedResiduals.Training.1<-EstimatedResiduals.Training
EstimatedResiduals.Training.2<-EstimatedResiduals.Training
EstimatedResiduals.Training.1[(LinearModel.Training[,3]==0)*(1:nSample.Training)]<-NA
EstimatedResiduals.Training.2[(LinearModel.Training[,3]==1)*(1:nSample.Training)]<-NA
matplot(LinearModel.Training[,1],cbind(EstimatedResiduals.Training.1,
                                       EstimatedResiduals.Training.2),
        pch=16,col=c("green","blue"),ylab="Separated parts of the training sample")
```

```{r}
Logistic.Model.Data<-data.frame(Logistic.Output=LinearModel.Training[,3],
                                Logistic.Input=EstimatedResiduals.Training)
LinearModel.Training.Logistic<-glm(Logistic.Output~Logistic.Input,data=Logistic.Model.Data,
                                   family=binomial(link=logit))
summary(LinearModel.Training.Logistic)
```

```{r}
Predicted.Probabilities.Training<-predict(LinearModel.Training.Logistic,type="response")
plot(LinearModel.Training[,1],Predicted.Probabilities.Training)
```
```{r}
Unscrambling.Sequence.Training.Logistic<-
  (predict(LinearModel.Training.Logistic,type="response")>.5)*1
ClassifiedResiduals.Training.1<-EstimatedResiduals.Training
ClassifiedResiduals.Training.2<-EstimatedResiduals.Training
ClassifiedResiduals.Training.1[(Unscrambling.Sequence.Training.Logistic==0)*
                                 (1:nSample.Training)]<-NA
ClassifiedResiduals.Training.2[(Unscrambling.Sequence.Training.Logistic==1)*
                                 (1:nSample.Training)]<-NA
head(cbind(AllTraining=EstimatedResiduals.Training,
           Training1=ClassifiedResiduals.Training.1,
           Training2=ClassifiedResiduals.Training.2))
```

```{r}
matplot(LinearModel.Training[,1],cbind(ClassifiedResiduals.Training.1,
                                       ClassifiedResiduals.Training.2),
        pch=16,col=c("green","blue"),ylab="Classified residuals, X-axis at 0")
axis(1,pos=0)
```
```{r}
Classification.Rule.Logistic<-summary(LinearModel.Training.Logistic)$coefficients[[1]]/-summary(LinearModel.Training.Logistic)$coefficients[[2]]
Classification.Rule.Logistic
```
```{r}
LinearModel<-read.table("C:/Users/u353822/Downloads/Week6_Test_Sample_Test.csv",head=T)
LinearModel <- LinearModel %>%
  select(Input,Output)
nSample<-length(LinearModel[,1])
EstimatedLinearModel<-lm(LinearModel[,2]~LinearModel[,1])
EstiamtedResiduals<-EstimatedLinearModel$residuals
plot(LinearModel[,1],EstimatedResiduals)
```
```{r}
Unscrambling.Sequence.Logistic<-(predict(LinearModel.Training.Logistic,
                                         newdata=data.frame(Logistic.Output=EstimatedResiduals,
                                                            Logistic.Input=EstimatedResiduals),
                                         type="response")>.5)*1
Probability<-sum(Unscrambling.Sequence.Logistic)/length(Unscrambling.Sequence.Logistic)
Probability
```

```{r}
ClassifiedResiduals.1<-EstimatedResiduals
ClassifiedResiduals.2<-EstimatedResiduals
ClassifiedResiduals.1[(Unscrambling.Sequence.Logistic==0)*(1:nSample)]<-NA
ClassifiedResiduals.2[(Unscrambling.Sequence.Logistic==1)*(1:nSample)]<-NA
```
```{r}
matplot(LinearModel[,1],cbind(ClassifiedResiduals.1,
                              ClassifiedResiduals.2),
        pch=16,col=c("green","blue"),ylab="Classes of the main sample, X-axis at 0")
axis(1,pos=0)
```
```{r}
matplot(LinearModel[,1],cbind(ClassifiedResiduals.1,ClassifiedResiduals.2),
        pch=16,col=c("green","blue"),ylab="Classes of the main sample, X-axis at the rule level")
axis(1,pos=Classification.Rule.Logistic)
res <- list(Unscrambling.Sequence.Logistic =  Unscrambling.Sequence.Logistic)
write.table(res, file = paste('C:/Users/u353822/Documents/R/Statistical Analysis','result.csv',sep = '/'), row.names = F)

```

