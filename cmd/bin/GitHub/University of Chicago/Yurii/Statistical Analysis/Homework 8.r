if(!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}
pacman::p_load(faraway,faraway,corrplot, tinytex, dplyr,zoo, knitr)



coagulation
plot(coag~diet, data=coagulation, pch=19,col="black")

summaryByGroup<-aggregate(coag~diet,data=coagulation,FUN=summary)
summaryByGroup

means<-cbind(Means=summaryByGroup$coag[,4],Sizes=aggregate(coag~diet,data=coagulation,FUN=length)$coag)
rownames(means)<-as.character(summaryByGroup$diet)
means

modelSummary<-lm(coag~diet,coagulation)
coag.model<-lm(coag~diet,coagulation)
summary(modelSummary)
summary(modelSummary)$df

anova(modelSummary)

coag<-coagulation
coag$x1<-coag$diet=="B"
coag$x2<-coag$diet=="C"
coag$x3<-coag$diet=="D"

coag.model.full<-lm(coag~x1+x2+x3, data=coag)
coag.model.null<-lm(coag~1,data=coag)
anova(coag.model.null,coag.model.full)

grand.mean<-mean(coagulation$coag)
nrow(subset(coagulation, diet == 'A'))
subset(coagulation, diet == 'A')

group.mean<-c(rep(61,4),rep(66,6),rep(68,6),rep(61,8))

SST<-sum((coagulation$coag-grand.mean)^2)
SSE<-sum((coagulation$coag-group.mean)^2)
cbind(SST,SSE,SSM=SST-SSE)

anova(coag.model)
#When else would we use this?
model.matrix(coag.model)

coag.altmodel<-lm(coag~diet-1,data=coagulation)
sumary(coag.model.full)$coefficients
summary(coag.model)
#The above two are the same but why

model.matrix(coag.altmodel)

test_dat <- read.table(paste('C:/Users/u353822/Downloads','Week8_Test_Sample.csv',sep = '/'), header=TRUE)
head(test_dat)

subsetA<-rep(mean(test_dat[test_dat$Treatment=='A',1]),length(test_dat[test_dat$Treatment=='A',1]))
subsetB<-rep(mean(test_dat[test_dat$Treatment=='B',1]),length(test_dat[test_dat$Treatment=='B',1]))
subsetC<-rep(mean(test_dat[test_dat$Treatment=='C',1]),length(test_dat[test_dat$Treatment=='C',1]))
groupmeans<-c(subsetA,subsetB,subsetC)
groupmeans


test_dat1<-cbind(test_dat,groupmeans)
test_dat1

SSE<-sum((test_dat$Output-groupmeans)^2)
SSE=53.08616
SST<-sum((test_dat$Output-mean(test_dat$Output))^2)
SSM<-SST-SSE
SSM
SSM=41.74485


#All group means manlues equal to 0?
Treatment.model<-lm(Output~Treatment,test_dat)

model.matrix(Treatment.model)
anova(Treatment.model)


Treatment.model.alt<-lm(Output~Treatment-1,test_dat)
model.matrix(Treatment.model.alt)
anova(Treatment.model.alt)

anova(Treatment.model,Treatment.model.alt)
