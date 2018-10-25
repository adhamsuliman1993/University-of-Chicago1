plot(weight$TIME[weight$WEIGHT==1],weight$TIME[weight$WEIGHT==2])
head(angry_moods)
plot (angry_moods$`Anger-Out`,angry_moods$`Anger-In`)

cor(angry_moods$`Anger-Out`,angry_moods$`Anger-In`)
8742/31333
Is there a difference in how much males and females use aggressive behavior to improve an angry mood? 
  For the "Anger-Out" scores, compute a 99% confidence interval on the difference between gender means.


Year12_30min <- c(66,68,59,72,46)
Year12_60min <- c(69,61,69,73,61)
Year16_30min <- c(74,71,67,82,76)
Year16_60min <- c(95,92,95,98,94)

Mean_Year12_30min <- mean(c(66,68,59,72,46))
Mean_Year12_60min <- mean(c(69,61,69,73,61))
Mean_Year16_30min <- mean(c(74,71,67,82,76))
Mean_Year16_60min <- mean(c(95,92,95,98,94))

Mean_30min<- mean(c(66,68,59,72,46,74,71,67,82,76))
Mean_60min<- mean(c(69,61,69,73,61,95,92,95,98,94))
Mean_12Year<- mean(c(66,68,59,72,46,69,61,69,73,61))
Mean_16Year<- mean(c(74,71,67,82,76,95,92,95,98,94))

Year <- c(66,68,59,72,46,69,61,69,73,61,74,71,67,82,76,95,92,95,98,94)
Mean_Year <- mean(Year)
Mean20_Year <- c(rep(Mean_Year,20))
Mean10_Year <- mean(c(rep(Mean_Year,10)))

SST<- sum((Year-Mean20_Year)**2) = 3806.8
SSA<- 10*( (Mean_30min-Mean_Year)**2 + (Mean_60min-Mean_Year)**2) = 793.8
SSB<- 10*((Mean_12Year-Mean_Year)**2+(Mean_16Year-Mean_Year)**2)= 2000
SSE<-sum((Year12_30min-mean(Year12_30min))**2+(Year12_60min-mean(Year12_60min))**2+
           (Year16_30min-mean(Year16_30min))**2+(Year16_60min-mean(Year16_60min))**2)=676.8

SSAB<- SST-(SSA+SSB+SSE)=336.2

MSA<- (SSA)/1 = 793.8
MSB<- (SSB)/1 = 2000
MSAB<- (SSAB)/1= 336.2
MSE<- (SSE)/16 = 42.3


FA<-MSA/MSE=18.765
FB<-MSB/MSE=47.281
FAB<-MSAB/MSE= 7.948




?plot
