AssignmentData<-  read.csv(file=paste("C:/Users/u353822/Documents/R/Statistical Analysis","Statistical Analysis Project.csv",sep="/"),
                           row.names=1,header=TRUE,sep=",")
head(AssignmentData,5)

#Reassigning Variables
require(dplyr)

##Inputs
matplot(AssignmentData[,-c(8,9,10)],type='l')

##Inputs with Output
matplot(AssignmentData[,-c(9,10)],type='l')

##3m_Output
Input1.Linear.Model<-lm(Output1~USGG3M,AssignmentData)
#is thats what Epsilon is
summary(Input1.Linear.Model)$sigma^2

matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input1.linear.Model$fitted.values,col="red")

##6m_Output
Input2.Linear.Model<-lm(Output1~USGG6M,AssignmentData)
summary(Input2.Linear.Model)$sigma^2

##2yr_Output
Input3.Linear.Model<-lm(Output1~USGG2YR,AssignmentData)
summary(Input3.Linear.Model)$sigma^2

##3yr_Output
Input4.Linear.Model<-lm(Output1~USGG3YR,AssignmentData)
Coefficient.Input4<-Input4.Linear.Model$coefficients
summary(Input4.Linear.Model)$sigma^2

##5yr_Output
Input5.Linear.Model<-lm(Output1~USGG5YR,AssignmentData)
summary(Input5.Linear.Model)$sigma^2

##10yr_Output
Input6.Linear.Model<-lm(Output1~USGG10YR,AssignmentData)
summary(Input5.Linear.Model)$sigma^2

##30yr_Output
Input7.Linear.Model<-lm(Output1~USGG30YR,AssignmentData)
summary(Input7.Linear.Model)$sigma^2

#Fitted LinearModel 1 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input1.Linear.Model$fitted.values,col="red")

#Fitted LinearModel 2 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input2.Linear.Model$fitted.values,col="red")

#Fitted LinearModel 3 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input3.Linear.Model$fitted.values,col="red")

#Fitted LinearModel 4 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input4.Linear.Model$fitted.values,col="red")

#Fitted LinearModel 5 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input5.Linear.Model$fitted.values,col="red")

#Fitted LinearModel 6 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input6.Linear.Model$fitted.values,col="red")

#Fitted LinearModel 7 against Output1
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(Input7.Linear.Model$fitted.values,col="red")

##Step2
apply(AssignmentData,2, function(z) lm(Output1~z,AssignmentData)$coefficients)

##Step 3
apply(AssignmentData,2, function(z) lm(z~Output1,AssignmentData)$coefficients)


lapply(AssignmentData$USGG3M, function(z)
  lm(z~AssignmentData$Output1))
