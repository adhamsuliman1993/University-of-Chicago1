
head(angry_moods_1_)
hist(angry_moods_1_$`Control-Out`, main = "Control Out", ylab = 'Count', xlab = "Control Out Level")

sports <- angry_moods_1_[angry_moods_1_$Sports<=1,]


mean(angry_moods_1_$`Control-Out`[angry_moods_1_$Sports > 1])


angry_moods_1 <- transform(angry_moods_1_,Anger_Expression = angry_moods_1_$`Anger-Out`+ angry_moods_1_$`Anger-In` - angry_moods_1_$`Control-Out`- angry_moods_1_$`Control-In`+48)

angry_moods_2 <- data.frame(angry_moods_1)
install.packages("moments")

library("ggplot2")
head(angry_moods_2)
boxplot(angry_moods_2$Anger_Expression~angry_moods_2$Gender, 
        xlab = "Gender", ylab="Anger Expression",
        las = 1,
        names = c("Male","Female"))

x=(angry_moods_2$`Control Out`)
y=(angry_moods_2$`Control In`
   
cor(x,y)
