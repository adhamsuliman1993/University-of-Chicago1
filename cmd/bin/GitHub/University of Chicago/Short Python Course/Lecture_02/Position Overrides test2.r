install.packages("caret")
library(caret)

my.German <- read.csv("German.Credit.csv")

names(my.German)
apply(my.German, 2, range)
#create a loop to return each vaf value between 2:10 because that is what we are going to plot, y=vaf against x=number of clusts
#;  size and centroid are needed at the end
#loop 2:10
#Arguments that go into kmeans:
cluster.object <- kmeans(my.German.Train, centers = i, nstart = 100)

#1 run kmeans for 2-10 total clusters. we will need a loop to do this
#2 save the vaf (variance inflaction factor) for each number of clusters

VAF <- cluster.object$betweenss/cluster.object$totss

#3 Size of the clusters
size <- cluster.object$size/ntraining #proportional

#4 Centroiids of the clusters
centroid<- cluster.object$centers

#step4/5 -- Use the scree plot to determine the appropriate number of clusters based on where we observe teh elbow feature.
# Also, look at the size of the clusters in the object you are selecting. Ideally clusters will be as close to evenly proportioned as possible

#Step 6: Check the interpretability of your clusters by looking at the centers. Can you understand them?
#Consider restoring the scaled data to it's originnal value for better interpretability. How did you scale the data -- apply inverse function

# (centroid value - min(col))/max(col)-min(col)) - scaled.centroid.value
#Scaled.centroid*(max-min) + min = restored centroid value

#Q: Is there a good way to visualize and compare the centers? It would be really hard to look at so many numbers when k is larger.
#A:Joshua Goldberg: to Everyone  centroid + min(col) * (max(col) - min(col))?
  
  
#once you are happy with your selection, perform holdout validation. We will only be running k means for one number of clusters this time wtih the starting centers
#assigned as the centers found using the training data.

my.test.data <- kmeans(my.German.Test, centers = cluster.object$centers) # no multiple starts

#We will want to compare VAF, size, and centers (or restored centers).
#Results consitent with train data should confirm that we've made a good selection

#step 7 -- KO-means
#run komeans function for the number of clusters you chose using kmeans
#if I chose 4 clusters above. keep other parameters the same
#ex.
komeans.4 <- komeans(German.Training.Scaled, nclust = 4, lnorm = 2, nloops = 100, tolerance = .001, seed = 3)

#Note we are using only train data. current komeans function makes holdout validation difficult.
#interpret komeans.4$
#3 Explore the komeans object
#4 Compare the VAF

#5. Choose 4 clusters out of the 16 komeans has created to compare wtih 4 kmean clusters: 

k.vs.ko<- table(km.object.4$cluster,komeans.4$Group) #This shows us how many are in each clusters
apply(k.vs.ko,2,sum) #This sums all the columns (bottom page 6 pdf)

#6 Find the observations are in the clusters you selected above to compare with kmeans
komeans.4$Group

#the numbers that show up are in reference to the partition that it is in 
#use which(komeans.4$Group == 5)

#use this to create the cluster centers and restored centers and compare with kmeans
#which solution is more interpretable