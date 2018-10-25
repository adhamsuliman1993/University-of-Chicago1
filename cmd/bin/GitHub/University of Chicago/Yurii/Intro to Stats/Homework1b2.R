install.packages("ggplot2")

test <-matrix(c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3), nrow= 10, byrow=FALSE)
test
test_box <- ggplot(test(aes([1],[2],[3])))
rm(test)
Non_Players <- c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2)
Beginners <- c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7)
Tournament_Players <- c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)
chess <- data.frame(Non_Players, Beginners, Tournament_Players)
chess

Records <-c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2, 32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)

boxplot(chess, Main = "Boxplot", xlab = "Group", ylab= "Number of Correct Pieces",
        las = 1, )

ggplot(chess, aes(Non_Players,Beginners, Tournament_Players))                 

Groups1<-c("Non_Players1","Non_Players1","Non_Players1","Non_Players1","Non_Players1","Non_Players1","Non_Players1","Non_Players1","Non_Players1","Non_Players1",
           "Beginners1","Beginners1","Beginners1","Beginners1","Beginners1","Beginners1","Beginners1","Beginners1","Beginners1","Beginners1",
           "Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1","Tournament_Players1")

New_Test <- data.frame(Groups1,Records)
New_Test

ggplot(New_Test, aes(x=Groups1,y=Records)) + geom_boxplot()
