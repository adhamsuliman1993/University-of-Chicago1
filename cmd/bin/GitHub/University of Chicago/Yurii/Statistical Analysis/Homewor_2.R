library(randtests)
x <- c(1,1,1,1,1,2,2,2,2,2)
supressWarning(library(randtests))
runsTest<-runs.test(x,threshold=1.5)
runsTest



a<-1
b<-2.5
set.seed(8402)
nSample <- 500
Eps <- rnorm(nSample, 0, 1.5)

set.seed(2048)
X<-rnorm(nSample,3,2.5)
Y<- a*X+b+Eps
Model<- as.data.frame(cbind(Y=Y,X=X))
head(Model)

plot(Model$X,Model$Y)



#1.1) Use runif()
library("random")
set.seed(15)
Sample<-runif(1000,0,1)

#1.2) Simulate uniform random sample on [0,1] Using random.org
suppressWarnings(library(random))
nFlips<-1000
dataFromRandom<-randomNumbers(n=nFlips, min=0, max=1, col=1, base=2, check=TRUE)
head(dataFromRandom)

rm(dataFromRandom)
#1.3) Downloading data from Random.org
dataFromRandom<-read.table(paste("C:/Users/u353822/Documents/R/Statistical Analysis","randbyte.txt",sep="/"))
dataFromRandom<-na.omit(unname(unlist(dataFromRandom)))
dataFromRandom<-(as.vector(sapply(dataFromRandom,function(z) head(intToBits(z),8)))==1)*1
head(dataFromRandom)

#1.4) Turning binary sequence to uniform random numbers 
set.seed(15)
bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}
bitsToInt(c(1,1,1,1,1,0))
Binary.matrix<-matrix(dataFromRandom,ncol=10)
head(Binary.matrix)
dataFromRandom.dec<-apply(Binary.matrix,1,bitsToInt)/2^10
head(dataFromRandom.dec)

#2.1)Test uniformity of distribution of both random number generators. 
library("random")
set.seed(15)
Sample<-runif(1000,0,1)
Sample.histogram<-hist(Sample)
Sample.histogram

#What does the histogram tell you about the distribution? Is it consistent with the goal of simulation?
#The histogram seems to have a relatively equal distribution except from .3 to 1 where there seems to be a slight positive increase in frequencies. 
#Other than that, it is consistent with the goal of this simulation. 

(1.02 +0.90+ 0.87+ 0.91 +1.00+ 0.99+ 1.08+ 1.05+ 1.03+ 1.15)/10=1
abc<-c(1.02,0.90,0.870,.91,1.00,0.99,1.08,1.05,1.03,1.15)
sqrt(sum((abc-1)^2)/9)=.0868
(Sample.histogram.mean<-mean(Sample.histogram$density))
(Sample.histogram.sd<-sd(Sample.histogram$density))
plot(Sample.histogram,freq=FALSE,ylim=c(0,Sample.histogram.mean+2*Sample.histogram.sd))
abline(h=Sample.histogram.mean)
abline(h=Sample.histogram.mean+1.96*Sample.histogram.sd,col="red",lty=2)
abline(h=Sample.histogram.mean-1.96*Sample.histogram.sd,col="red",lty=2)

#What does the graph tell you about the observed distribution?
#This Graph tells me that the distribution is random because we fail to reject the null that it is random. 
#The area between the red lines represents 95% of the normal distribution for randomness and all the data falls in between these two lines.

(Sample.mean<-mean(Sample))
(Sample.variance<-var(Sample))

#What do you conclude about the estimated distribution from the moments?
#I would conclude that it is randomly distriubted due to the min, 1st quartile, mean, 3rd quartile, and max respectively falling nearly on 0,.25,.5,.75,1 which shows a random distribution from 0 to 1.
#What do you think is the best way of estimating uniform distribution over unknown interval? If you can find the quartiles as was done above, you can then check the equivalency of .25*max = 1st quartile. You would then compare the rest of the quartiles in the same fashion. If the numbers are near one another, you can assume the distribution is uniform.


#2.1.2) Repeat the same steps to test uniformity of the sample from Random.org
Sample.histogram<-hist(dataFromRandom.dec)
(Sample.histogram.mean<-mean(Sample.histogram$density))
(Sample.histogram.sd<-sd(Sample.histogram$density))
plot(Sample.histogram,freq=FALSE,ylim=c(0,Sample.histogram.mean+2*Sample.histogram.sd))
abline(h=Sample.histogram.mean)
abline(h=Sample.histogram.mean+1.96*Sample.histogram.sd,col="red",lty=2)
abline(h=Sample.histogram.mean-1.96*Sample.histogram.sd,col="red",lty=2)

#2.2.2 Test frequency by Monobit test
dataFromRandom.plusminus1<-(dataFromRandom-.5)*2
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)

#3.1) My random number generator looks at sample of students in college and checks if they have financial support from their parents. A 1 means they do have financial support while a 0 means no financial support.
#3.2)
student_mat$support <- ifelse(student_mat$famsup=='yes',1,0)
Support<-student_mat$support


#3.2) Uniformity test
matrix_Support <-matrix(Support,ncol=5)
Nmatrix_Support<-apply(matrix_Support,1,bitsToInt)/2^5
hist_Support <- hist(Nmatrix_Support)
mean_Support <- mean(hist_Support$density)
sd_Support <- sd(hist_Support$density)


plot(hist_Support,freq=FALSE,ylim=c(0,mean_Support+2*sd_Support))
abline(h=mean_Support)
abline(h=mean_Support+1.96*sd_Support,col="red",lty=2)
abline(h=mean_Support-1.96*sd_Support,col="red",lty=2)

#3.4)Frequency Test
Support<- c(student_mat$support)
matrix_Support <-matrix(Support,ncol=5)
datafromsupport.plusminus1<-(Nmatrix_Support-.5)*2
erfc(abs(sum(dataFromRandom.plusminus1)/sqrt(2*395)))=.919
#Passes frequency test due to .919>.05

#3.5) Turning Point Test
suppressWarnings(library(randtests))
turning.point.test(Nmatrix_Support)

#4)By changing nSample and my.seed try to make the quote of the day readable with minimum sample size.
#It took me around 27,000 samples to be able to read the quote.

nSample<-27000
set.seed(9999999)
xy<-runif(2*nSample,0,100)
xy<-matrix(xy,ncol=2)
ScratchOffMonteCarlo(xy)

#What percent you needed to scratch off to make the quote readable? I need around 93.4% for it to be readable with the runif function

#4.3)
library('randtoolbox')
suppressWarnings(library(randtoolbox))
set.seed(100)
nSample<-23000
xy<-sobol(nSample,dim=2,init=T)*100
xy<-sobol(nSample,dim=2,init=F,scrambling = T,seed=my.seed)*100
ScratchOffMonteCarlo(xy)

#I need around 94.6% of it to be uncovered for it to be readable with the sobol sequence.
#I only had to take 20,000 samples with Sobol to uncover 94% compared to taking 27,000 samples with the runif function above to uncover 91.86% of the image.
#I personally find the Halton sequence to be the best for this given set of text. 






dataPath<- "C:/Users/u353822/Documents/R/Statistical Analysis"
dat <- read.table(paste(dataPath,'Week2_Test_Sample.csv',sep="/"),header=TRUE)
sd_dat<-c(dat$x[2])
mean_dat <- dat$x[1]

datNorm <- qnorm(p=dat$x[4:503],mean=mean_dat,sd=sd_dat)
datExp <- qexp(p=dat$x[4:503],rate=dat$x[3])

res<-cbind(datNorm=datNorm,datExp=datExp)
res

write.table(res, file = paste('C:/Users/u353822/Documents/R/Statistical Analysis','result.csv',sep = '/'), row.names = F)



set.seed(15)
Sample<-runif(1000,0,1)
Sample
Sample.histogram<-hist(Sample)