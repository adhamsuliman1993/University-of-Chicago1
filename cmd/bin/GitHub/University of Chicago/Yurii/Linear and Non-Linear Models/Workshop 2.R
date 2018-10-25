Norm.sample.vector<-read.csv('Workshop2.csv')
Norm.sample.vector <- as.data.frame(Norm.sample.vector)
head(Norm.sample.vector)

Negative.LL.Normal <- function(mu.sig.parameters, Sample.Vector){
  mu <- mu.sig.parameters[1]
  sigma_squared <- mu.sig.parameters[2]
  ((nrow(Sample.Vector)/2)*log(2*pi*sigma_squared)+(1/(2*sigma_squared))*sum((Sample.Vector-mu)^2))
}

plot(Norm.sample.vector)
Negative.LL.Normal(c(3,1),Norm.sample.vector)

Optimized.Negative.Log.Likelihood.optim<-optim(c(7,4),
                                               Negative.LL.Normal,
                                               Sample.Vector=Norm.sample.vector,
                                               method="L-BFGS-B",
                                               hessian=TRUE,
                                               lower=c(-Inf,0),
                                               control=list(trace=1))

??Optimized.Negative.Log.Likelihood.optim$value
mean(Norm.sample.vector)

dnorm(Norm.sample.vector[1],mean(Norm.sample.vector[,1]),sd = sd(Norm.sample.vector[,1])^2)
lmodel <- lm(dnorm(Norm.sample.vector[1],mean(Norm.sample.vector[,1]),sd = sd(Norm.sample.vector[,1])^2)~Norm.sample.vector[1])

#Create a linear model
linModLL<- function(Parameters,regSample){
  mu <- Parameters[1]
  sd <- Parameters[2]
  lmodel <- lm(dnorm(regSample,mu,sd,)~regSample)
  beta
  sigmaEps <- Paramaters[3]
  dtf <- regSample
  ((nrow(dnorm(dtf,Beta1,sqrt(Sigma)))/2)*log(2*pi*sigma_squared)+(1/(2*sigma_squared))*sum((dnorm(dtf,Beta1,sqrt(Sigma))-mu)^2))
  
}

linModLL(c(Beta0=beta0+1,Beta1=beta1+1,Sigma=sigmaEps),dtf)



Optimized.Negative.Log.linModLL.optim<-optim(c(7,4),
                                               Negative.LL.Normal,
                                               Sample.Vector=Norm.Sample.Vector,
                                               method="L-BFGS-B",
                                               hessian=TRUE,
                                               lower=c(-Inf,0),
                                               control=list(trace=1))



