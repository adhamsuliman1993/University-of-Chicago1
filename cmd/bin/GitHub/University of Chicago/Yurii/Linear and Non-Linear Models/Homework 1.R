Linear.Model.Data<-read.csv('C:/Users/u353822/Documents/R/Linear and Non-Linear Models/Workshop1.csv')

Linear.Model.Data.Frame<-as.data.frame(Linear.Model.Data)


Linear.Model.Data.lm<-lm(Output~Input,data=Linear.Model.Data.Frame)
Linear.Model.Data.glm<-glm(Output~Input,family=gaussian(link="identity"),data=Linear.Model.Data.Frame)
Linear.Model.Data.Null.lm<-lm(Output~1,data=Linear.Model.Data)

log_likelihood1<- function(Linear.Model.Data){
  (-1*(nrow(Linear.Model.Data)/2)*log(2*pi*(summary(lm(Output~Input,data=Linear.Model.Data))$sigma)^2))- 
    (1/(2*(summary(lm(Output~Input,data=Linear.Model.Data))$sigma)^2))*
    sum((lm(Output~Input,data=Linear.Model.Data))$residuals^2)
}

New_AIC<- -2*log_likelihood1(Linear.Model.Data)+6
New_AIC



cbind(summary(Linear.Model.Data.glm)$dispersion,summary(Linear.Model.Data.lm)$sigma,var(Linear.Model.Data.lm$residuals))

Linear.Model.Data<-read.table(('C:/Users/u353822/Documents/R/Linear and Non-Linear Models/Week1_Test_Sample.csv'),header=T)
Linear.Model.Data<-as.data.frame(Linear.Model.Data)
lm.fit<-lm(Output~Input1+Input2+Input3,data=Linear.Model.Data)
Linear.Model.Data.lm<-lm.fit

log_likelihood<- function(Linear.Model.Data){
  (-1*(nrow(Linear.Model.Data)/2)*log(2*pi*(summary(lm(Output~Input1+Input2+Input3,data=Linear.Model.Data))$sigma)^2))- 
    (1/(2*(summary(lm(Output~Input1+Input2+Input3,data=Linear.Model.Data))$sigma)^2))*
    sum((lm(Output~Input1+Input2+Input3,data=Linear.Model.Data))$residuals^2)
}


AIC(lm.fit)

coefficients<-c(lm.fit$coefficients)
residuals<-c(lm.fit$residuals)
fitted.values<-lm.fit$fitted.values
linear.predictors<-fitted.values
deviance<-sum((lm.fit$residuals)^2)
aic<--2*log_likelihood(Linear.Model.Data)+6
y<-Linear.Model.Data$Output
null.deviance<-deviance(lm(Output~1,data=Linear.Model.Data))
dispersion<-summary(lm.fit)$sigma


lm.fit
coefficients
residuals
fitted.values
linear.predictors
deviance
aic
y
null.deviance 
dispersion


res<- list(Linear.Model.Data.lm = lm.fit,
           coefficients = coefficients,
           residuals = residuals,
           fitted.values = fitted.values,
           linear.predictors = linear.predictors,
           deviance = deviance,
           aic = aic,
           y = y,
           null.deviance = null.deviance,
           dispersion = dispersion
)

saveRDS(res, file = paste('C:/Users/u353822/Documents/R/Linear and Non-Linear Models','result.rds',sep = '/'))
