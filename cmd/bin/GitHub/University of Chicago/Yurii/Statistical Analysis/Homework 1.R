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
