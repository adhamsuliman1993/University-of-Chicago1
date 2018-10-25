##rm(list=ls())
#Read in the data, create simple model
birds <- read.csv('C:/Users/u353822/Downloads/BirdLengWingMass.csv',header=T,skip=13)
birds
full.model <- lm(WingspanIN ~ LengthIN + WeightOZ, data=birds)
summary(full.model)

#Explore interpretative deficiencies
summary(lm(WingspanIN ~ LengthIN, data=birds))
summary(lm(WingspanIN ~ WeightOZ, data=birds))
cor(birds$LengthIN,birds$WeightOZ)

#install.packages('ggplot2')
library(ggplot2)
ggplot(data=birds, aes(x=LengthIN, y=WeightOZ)) +
  geom_point(aes(size=WingspanIN),alpha=0.5)

#Look at plot of the two predictors
plot(birds$LengthIN,birds$WeightOZ)
birds$labels <- NA
birds$labels[birds$LengthIN>26] <- 
  levels(birds$Species)[rank(birds$Species)][birds$LengthIN>26]
text(x=birds$LengthIN,y=birds$WeightOZ,labels=birds$labels,cex=0.5,pos=4)

#Perform PCA to recover loadings and factors/scores
birds.pca <- princomp(birds[,c('LengthIN','WeightOZ')])
names(birds.pca)
birds.pca$loadings
birds.pca$scores
cor(birds.pca$scores[,1],birds.pca$scores[,2])

#Loading Thesaurs: Factor Loadings, loadings, weights, Eigenvectors
#Factor Thesaurs: Factors, Principal Components, Scores



#'Manual' replication
Y <- as.matrix(birds[,c('LengthIN','WeightOZ')])
Ymeans <- cbind(rep(mean(Y[,1]),99),rep(mean(Y[,2]),99))
Y0 <- Y - Ymeans
L <- eigen(cov(Y0))$vectors
F <- Y0%*%L 
sum(abs(L - birds.pca$loadings))
sum(abs(F - birds.pca$scores))
#The two below are equal to one another
(Ymeans + F%*%t(L))[1:5,]
Y[1:5,]

#Visualize new basis vectors
plot(birds$LengthIN,birds$WeightOZ,asp=1,ylim=c(0,60))
lines(x=mean(birds$LengthIN)+c(0,0.228)*birds.pca$sdev[1],
      y=mean(birds$WeightOZ)+c(0,0.974)*birds.pca$sdev[1],
      col='blue',lwd=2)
lines(x=mean(birds$LengthIN)+c(0,-0.974)*birds.pca$sdev[2],
      y=mean(birds$WeightOZ)+c(0,0.228)*birds.pca$sdev[2],
      col='red',lwd=2)

#Use new factors to better understand the world
birds$Largeness <- birds.pca$scores[,1]
birds$Longness <- -1*birds.pca$scores[,2]
plot(birds$Largeness,birds$Longness)
birds$labels <- NA
birds$labels[birds$Largeness>35] <- 
  levels(birds$Species)[rank(birds$Species)][birds$Largeness>35]
text(x=birds$Largeness,y=birds$Longness,labels=birds$labels,cex=0.5,pos=2)

plot(birds$Largeness,birds$Longness,xlim=c(-20,-10),ylim=c(-7,2))
birds$labels <- NA
birds$labels <- levels(birds$Species)[rank(birds$Species)]
text(x=birds$Largeness,y=birds$Longness,labels=birds$labels,cex=0.5,pos=4)

new.model <- lm(WingspanIN ~ Largeness + Longness, data=birds)
summary(new.model)


