---
title: "Assignment 7"
output: pdf_document
---

```{r}
Regression.ANOVA.Data<-read.table('C:/Users/u353822/Documents/R/Statistical Analysis/Week7_Test_Sample.csv',header=TRUE)
head(Regression.ANOVA.Data)
fit.1<-lm(Output~1,data=Regression.ANOVA.Data)
fit.1.2<-lm(Output~1+Input1,data=Regression.ANOVA.Data)
fit.1.3<-lm(Output~1+Input2,data=Regression.ANOVA.Data)
fit.1.2.3<-lm(Output~.,data=Regression.ANOVA.Data)

```

## R Markdown

Part 2
Anova(fit.1.2)$Df is equal to 498 due to the two predictors of intercept and Input1 
anova(fit.1.2)$"Sum Sq" Looks at the sum of squares of the (1 and Input1)
anova(fit.1.2)$"F value"[1] Looks at the F Value taken between Output and (1 and Input1) 
anova(fit.1.2)$"Pr(>F)"[1] Looks at the significance value from the F table saying there there is a significant difference between Output and (1 and Input 1)
anova(fit.1.2) Any samples that we get with significance levels below 2.2e-16 would be considered significant

```{r }
anova(fit.1.2)
anova(fit.1.2)$Df
anova(fit.1.2)$"Sum Sq"
anova(fit.1.2)$"F value"[1]
anova(fit.1.2)$"Pr(>F)"[1]
```

Did adding Input.2 change RSS in the anova table? No, but why, still dk?
Sum of Squares explained by Input1 in model fit.1.2 : 260.05
Sum of Squares unexplained by Input1 in model fit.1.2: 184.15

```{r}
anova(fit.1.2)
```

Sum of Squares explained by Input2 in model fit.1.3: 5.26
Sum of Squares unexplained by Input2 in model fit.1.3: 438.93
```{r}
anova(fit.1.3)
```

F statistic for comparison of fit.1 and fit.1.2.3: 352.23
P-value for comparison of fit.1 with fit.1.2.3: 2.2e-16
```{r}
anova(fit.1,fit.1.2.3)
```




