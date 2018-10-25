nrow(subset(angry_moods,Gender==1))
head(angry_moods)

Anger<- angry_moods$Anger_Expression
Mean_Anger <- mean(Anger)= 37

Male_Anger<-(angry_moods$Anger_Expression[angry_moods$Gender==1])
Female_Anger<-(angry_moods$Anger_Expression[angry_moods$Gender==2])
Sports_Anger<-(angry_moods$Anger_Expression[angry_moods$Sports==1])
NonSports_Anger<-(angry_moods$Anger_Expression[angry_moods$Sports==2])

Mean_Male_Anger<-mean(Male_Anger)= 37.1667
Mean_Female_Anger<-mean(Female_Anger)= 36.896
Mean_Sports_Anger<-mean(Sports_Anger)=30.96
Mean_NonSports_Anger<-mean(NonSports_Anger)=39.849

Male_Sports_Anger <-(angry_moods$Anger_Expression[angry_moods$Gender==1 & angry_moods$Sports==1])
Male_NonSports_Anger <- (angry_moods$Anger_Expression[angry_moods$Gender==1 & angry_moods$Sports==2])
Female_Sports_Anger <-(angry_moods$Anger_Expression[angry_moods$Gender==2 & angry_moods$Sports==1])
Female_NonSports_Anger<-(angry_moods$Anger_Expression[angry_moods$Gender==2 & angry_moods$Sports==2])

Mean_Male_Sports_Anger <-mean(Male_Sports_Anger)=31.909
Mean_Male_NonSports_Anger <- mean(Male_NonSports_Anger)=40.21
Mean_Female_Sports_Anger <- mean(Female_Sports_Anger)=30.21
Mean_Female_NonSports_Anger<- mean(Female_NonSports_Anger)=39.65

SST<-sum((Anger-Mean_Anger)**2)=12,896
SSG<-30*((37.1667-37)**2)+48*((36.896-37)**2)= 1.35
SSSports<-25*((30.96-37)**2)+53*((39.849-37)**2)=1342.23
SSE<-sum((Male_Sports_Anger-Mean_Male_Sports_Anger)**2)+sum((Male_NonSports_Anger-Mean_Male_NonSports_Anger)**2)+
  sum((Female_Sports_Anger-Mean_Female_Sports_Anger)**2)+sum((Female_NonSports_Anger-Mean_Female_NonSports_Anger)**2)=11,532
SSGSports<- SST-(SSG+SSSports+SSE)= 20.23

MSG <- SSG/1= 1.352
MSSports <-SSSports/1= 1342.23
SSGSports<- SSGSports/1= 20.23
MSE<- SSE/(78-4)=155.84

