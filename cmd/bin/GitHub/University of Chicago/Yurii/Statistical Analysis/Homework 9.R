library(relaimpo)
Project.Data<-read.table(paste('C:/Users/u353822/Downloads','Week9_Test_Sample.csv',sep = '/'), header=TRUE)
head(Project.Data, 3)
Data.Levels<-as.numeric(Project.Data[1,])
Project.Data<-Project.Data[-1,]

Project.Data.comp<-Project.Data[,2:10]
Project.Data.comp1<-princomp(Project.Data.comp)
Project.Data.comp1$loadings
#The line below tells you the cumulative sum of explanation of factors needed to explain total variance
cumsum(Project.Data.comp1$sdev^2/sum(Project.Data.comp1$sdev^2))

Project.Data1<-as.data.frame(cbind(Resp=Project.Data[,1],Project.Data.comp1$score))
LinMod<-lm(Resp~.,Project.Data1)
#Code below tells you which factors have the most importance
calc.relimp(LinMod)



.9*summary(LinMod)$r.squared

#comp.2+comp.1+comp.5
1.555121e-01+

  
comp.2 comp
LinMod.test<-lm(Resp~Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8+Comp.9,Project.Data1)
calc.relimp(LinMod.test)

LinMod.test<-lm(Resp~Comp.2+Comp.1+Comp.5,Project.Data1)
calc.relimp(LinMod.test)
