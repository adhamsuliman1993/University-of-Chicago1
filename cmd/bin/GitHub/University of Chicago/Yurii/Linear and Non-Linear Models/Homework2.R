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


sqrt_func <- function(Function.To.Optomize){
    .5(Function.To.Optomize)**-.5
  }


my.Optimizer<-function(Start.Value, Function.To.Optimize, Epsilon, projectID){
      Start.Value.old = 0
      while (abs(Start.Value-Start.Value.old) > Epsilon) {
        Function.To.Optimize <- function(Start.Value){
          #derivative of x^.5
          (.5*(Start.Value)**-.5)
      }
        Start.Value.old = Start.Value
        Start.Value = .5*(Start.Value)**-.5
      }
      print(Start.Value)
}

Norm.sample.vector
(my.Optimizer(-3,Norm.sample.vector,Epsilon=.00001))


?deriv
deriv((nrow(Sample.Vector)/2)*log(2*pi*sigma_squared)+(1/(2*sigma_squared))*sum((Sample.Vector-mu)^2))
f(1,672)
g <- function(x) {f(x,672 )}

my.Optimizer <- function(Start.Value, Function.To.Optimize, Epsilon=.00001, projectID){
  f <- function(x) {
    Function.To.Optimize(x, projectID)
  }
  while (abs(Start.Value - Start.Value.old) > Epsilon){
    Start.Value.Old
    Start.Value <- Start.Value.Old - (f(Start.Value)/(deriv(f(Start.Value))))
    Epsilon=.00001
  }
  return(Start.Value)
}


#workshop: dnorm(yi, b1*x + sigma)
#x+delta, x-delta / sum of delta 
#try using optim to determine pararmeters
#or you can use anthony's way of manually calculating for slope. 


my.Optimizer(3,Danny,672)
#none of this will work because you don't have the equation
3-(Danny(3,672))/(D(Danny(3,672)))
?deriv
?uniroot
uniroot(herro(3,672),lower=-5,upper=+1)



my.Optimizer <- function(Start.Value, Function.To.Optimize, Epsilon=.00001, projectID= 672){
  f <- function(x) {
    Function.To.Optimize(x=3, projectID=672)
  }
  slope <- ((f(Start.Value)-f(Start.Value+Epsilon))/(Start.Value-(Start.Value+Epsilon)))
  intercept <- f(0)
  answer <-  -intercept/slope
  return(answer)
}
a<-seq(from=-.3, to=.1,by=.00000001)
b<-(Danny(a,672))
plot(a,b)

for (x in a){
  print(unique(a,min(b)))
}
test<-cbind(a,b)
test


plot(,Danny(3,672))
X<-seq(from=-5,to=5,by=.1)
Y<-Danny(X,672)
plot(X,Y,type="l")
abline(h=0)

my.Optimizer(-3,Danny)

uniroot(Danny(3,672),lower =- 5, upper =+1)$root

testFunction<-readRDS(file=paste("C:/Users/u353822/Downloads","documents%2FMScA Linear and Non-Linear Models 31010%2FMScA 31010 Lecture 2%2FMScA_Nonlinear_Models_Week2_TestFunction (7).rds",sep="/"))$Week2_Test_Function
testFunction(2,672)

functions <- function(x, projectid){
  abs(testFunction(x,672))
}
optim(-3,functions,gr="L-BFGS-B")      
my.Optimizer <- function(Start.Value = -3,
                         functions,
                         Epsilon = .0001,
                         projectID=672)
  ?optim
my.root <- uniroot(functions, lower=-5, upper =-1) 
unitroot <- uniroot(functions, lower=-5, upper=0, tol = .0001) 
my.root
unitroot
functions(-1.094633)
functions(-0.1664063)
uniroot.lower <- -5
uniroot.upper <- -1


optim(-3,fn=functions,gr="L-BFGS-B")

unitroot$root

my.Optimizer <- function (x=-3,functions,Epsilon=.0001){
  optim(x,fn=functions(x,672),gr="L-BFGS-B")
}

my.Optimizer(x=-3,functions,Epsilon=.0001)
?optim
res <- list(Start.Value = -3,
              my.Optimizer.root = -1.094632,
              uniroot.root = uniroot$root,
              uniroot.lower = uniroot.lower,
              uniroot.upper = uniroot.upper)


uniroot(my.Function,lower=-5,upper=+1)
  
write.table(res, file = paste('C:/Users/u353822/Documents/R/Linear and Non-Linear Models','result.csv',sep = '/'), row.names = F)

x <- seq(-2,1, by=.1)
y <- testFunction(seq(-2,1, by=.1), 672)
xy <- as.data.frame(cbind(x,y))
library(ggplot2)
xy
ggplot(xy, aes(x=x,y=y)) + geom_line()
min(y)
