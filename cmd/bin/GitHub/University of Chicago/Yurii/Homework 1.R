case1.varianceX<-.1
case1.varianceEps<-6
case1.slopeA<-2
case1.interceptB<-1
case1.sampleX<-rnorm(500,mean=0,sd=sqrt(case1.varianceX))
case1.sampleEps<-rnorm(500,mean=0,sd=sqrt(case1.varianceEps))
case1.sampleY<-case1.slopeA*case1.sampleX+case1.interceptB+case1.sampleEps

case1.sampleY

dataPath <- "C:/Users/u353822/Desktop/Statistical Analysis Class/"
dat <- read.table(paste0(dataPath, 'Week1_Test_Sample.csv'),header=TRUE)
dat

sdX <-sd(dat$x)= 2.356 
sdY <-sd(dat$y)= 2.336
cXY <-cor(dat$x,dat$y)= -.919
b<- cXY*(sdY/sdX)= .0258
a<-mean(dat$y)-b*(mean(dat$x))=-.911

result <- data.frame(sdX=2.356,sdY=2.336,cXY=-.919,a=-.911)
result

write.table(result, file = paste(dataPath,'result.csv',sep='/'), row.names = F)

#1.1
#What does this say about fairness of the coin? If it converges to .5, then the coin can be considered fair which it does as we see on the plot below. 
nFlips <-100000
set.seed(12345)
Flips<-sample(0:1,nFlips,repl=T)
Trajectory<-cumsum(Flips) 
freq<-Trajectory/(1:nFlips)

#The plot below shows a longer sample length and shows that the frequency visually averages at 50% earlier than the second graph. The second graph
#shows how the average frequency begins to remain at 50% after 2000 flips. 
plot(1:length(freq),freq, ylim=c(.4,1),type="l",ylab="Frequency",xlab="Sample Length")
lines(c(0,nFlips),c(.5,.5))
plot(1:4000,freq[1:4000], ylim=c(.4,1),type="l",ylab="Frequency",xlab="Sample Length") 
lines(c(0,4000),c(.5,.5))

#2.1
#Find at least one alternative way of simulating variable Flips.
nFlips=10000000
Flips_alt<-rbinom(nFlips,1,.5)
trajectory_alt<-cumsum(Flips_alt)
plot(trajectory_alt, ylim=c(-1000,1000),type="l")
lines(c(0,nFlips),c(0,0))

#how much do you expect the trajectory of wealth to deviate from zero? It will deviate initially highly, but it well then average out to 0.
#How long do you expect it to stay on one side above or below zero? I believe it will average out to 0 at around the 2000 mark as we saw in the second graph
#for the previous problem. 
nFlips<-1000000
Flips<-(sample(0:1,nFlips,repl=T)-.5)*2
oneTrajectory<-cumsum(Flips)
plot(oneTrajectory, ylim=c(-1000,1000),type="l")
lines(c(0,nFlips),c(0,0))
#How do the observations match your prior expectations? I was not expecting the summation to stay away from 0 for the entirety of the analysis.


#2.2
#What do you expect the probabilities of the following events to be? I expect the probablitiy of the first to be between 15% and 20% and 
#the probability of the second to be lower than 5%
#For P(Nh-Nt)<5 
pbinom(255,500,prob=.5)-pbinom(245,500,prob=.5)= 34.5%
#For P(Nh-Nt)>25
pbinom(274,500,prob=.5,lower.tail=FALSE)*2= 2.83%


#Turn the sample Flips of 1,000,000 coin flips into 2000 random walk samples, each is 500 long. Calculate 2000 cumulative trajectories???
set.seed(12345)
Trajectories2000by500<-t(apply(matrix(Flips,nrow=2000, ncol=500),1,cumsum))
dim(Trajectories2000by500)
(probability.less.than.5<-sum(abs(Trajectories2000by500[,500])<5)/2000)=.18
(probability.greater.than.25<-sum(abs(Trajectories2000by500[,500])>=25)/2000)=.25

?matrix
Trajectories2000by500

#Interpret the results. How do they correspond to your intuition?
#For the first, I would expect a probability between 10 to 15% due to the large amoutn of samples taken.....
#For the second, I would expect a probability between 25% to 35%....
#I didn't expect the probabilities to be so low for both distributions. 


#2.3
#How long do you expect trajectory of random walk to spend on one side from zero, below or above? (Answer before doing calculations)
#I would expect it to change every 100 samples?
timeAbove<-apply(Trajectories2000by500,1,function(z) sum(z>0))
hist(timeAbove)

#Interpret the results. Was your intuition correct?
#Explain the observed distribution.



