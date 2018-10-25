pollutionData<-read.csv(paste('C:/Users/u353822/Downloads',"airpollution.csv",sep="/"))
pollutionData %>% pollutionData
  group_by (location) %>%
  summarise(group_mean = mean(pollution))
  
pollutionData1<-(aggregate(pollutionData[,1], list(pollutionData$location), mean))
pollutionData2<-c(117,117,111,111,132, 132)
pollutionData2


airp.model<-lm(pollution~location,data=pollutionData)
summary(airp.model)
names(airp.model)

Grand.mean<-mean(pollutionData$pollution)
Within.SS<- sum((pollutionData$pollution-pollutionData2)^2)
Within.SS
Within.SS.one.line<-sum((pollutionData$pollution-airp.model$fitted.values)^2)
Within.SS.one.line

total_sum_of_squares<-sum((pollutionData$pollution-Grand.mean)^2)
total_sum_of_squares

plot(pollutionData$location,pollutionData$data)

sum((airp.model$residuals-Grand.mean)^2)
anova(airp.model)

airp<-pollutionData
airp$loc<-as.numeric(airp$location)
airp.model2<- lm(pollution~1+(loc==3),data=airp)
summary(airp.model2)

means<-c(rep(114,4),132,132)

sum((pollutionData$pollution-means)^2)

airp$x1<-with(airp,(loc==1)+0.5*(loc==3))
airp$x2<-with(airp,(loc==2)+0.5*(loc==3))
airp

