set.seed(1029384)
Total.sample.size<-1000000
One.run.size<-1000
Slope<-1.7
Intercept<-3
Input<-rnorm(One.run.size,1,1)


set.seed(102938)
Eps<-rnorm(Total.sample.size,0,1)
dim(Eps)<-c(1000,1000)

Output.matrix<-Slope*Input+Intercept+Eps
Fits<-apply(Output.matrix,2,function(z) lm(z~Input))
unlist(lapply(Fits,function(z) z$coefficients[1]))




?par
















nSample<-1000
b0<- 3
b1<- -1.5
sigma<- 2.2
eps<- rnorm(nSample,0,sigma)
x<-rnorm(nSample,7,2)
y<-b0+b1*x+eps
plot(x,y)

dta<-data.frame(Y=y,X=x)
m1<-lm(Y~X,dta)

names(m1)

yHat<-m1$fitted.values
Err<-dta$Y-yHat
yHat%*%Err
yHat_Length<-sqrt(yHat%*%yHat)
where is the project of Y onto the model space: c((dta$Y%*%yHat)/yHat_Length,yHat_Length)

what is the projection of y in the direction of y bar

yBar=rep((mean(dta$Y)),nSample)
yBar_Length<-sqrt(yBar%*%yBar)

This ithe length of the projection of Y onto YBar
c(dta$Y%*%yBar/yBar_Length, yBar_Length)

. We expect to see 0. (blue and orenge)
yVar<-dta$Y-yHat
yVar%*%yBar 

What is the projection of y in the direction of Yhat-ybar
Green Bar:
Effect<-yHat-yBar
yBar%*%Effect
Effect_Length<-sqrt(Effect%*%Effect)
c(dta$Y%*%Effect/Effect_Length,Effect_Length)




set.seed(1029384)
total.sample.size<-100000000
one.run