## This file will create static visualisations
library(ggplot2)
library(dplyr)
clean_data <- read.csv("clean/us_food.csv")
head(clean_data)


## I need to code the jfs - house hold income model - adjust for confounding number in household, time in us?
jfsincomemod <- lm(jfs~as.factor(Household_Income),data=clean_data)
jfsincomemodlin <- lm(jfs~Household_Income,data=clean_data)
jhpred <- data.frame(Household_Income = 1:11)
jhpred$jfspred <-predict(jfsincomemod,newdata=jhpred,interval="confidence")
jhpred$jfspredlin <-predict(jfsincomemodlin,newdata=jhpred,interval="confidence")
j<- ggplot(data = clean_data, mapping = aes(x=as.factor(Household_Income),y=jfs)) +
  geom_boxplot()
j<-j+
  geom_line(data=jhpred,mapping=aes(x=Household_Income,y=jfspred[,1]))+
  geom_line(data=jhpred,mapping=aes(x=Household_Income,y=jfspredlin[,1]),col="red")
j <- j +
  geom_ribbon(data=jhpred,aes(x = Household_Income, ymin = jfspred[,2], ymax = jfspred[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")+
  geom_ribbon(data=jhpred,aes(x = Household_Income, ymin = jfspredlin[,2], ymax = jfspredlin[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,fill="red",col="red")
j
summary(jfsincomemod)
summary(jfsincomemodlin)


for (i in 1:length(colnames(clean_data))){
  cat(range(clean_data[,i]),"\n")
}
## jfs - bmi model
clean_data$grouped <- ntile(clean_data$jfs,10)
head(clean_data)
jfsbmilinmod <- lm(BMI~grouped,data=clean_data)
jfsbmicatmod <- lm(BMI~as.factor(grouped),data=clean_data)
summary(jfsbmilinmod)
summary(jfsbmicatmod)
preds<- data.frame(grouped = c(1,2,3,4,5,6,7,8,9,10))
preds2<- data.frame(grouped = c(1,2,3,4,5,6,7,8,9,10))
preds$bmipreds <- predict(jfsbmilinmod,newdata=preds,interval="confidence")
preds2$bmipredscat <- predict(jfsbmicatmod,newdata=preds2,interval="confidence")

h<- ggplot(data = clean_data, mapping = aes(x=as.factor(grouped),y=BMI)) +
  geom_boxplot()
h<-h+
  geom_line(data=preds,mapping=aes(x=grouped,y=bmipreds[,1]))+
  geom_line(data=preds2,mapping=aes(x=grouped,y=bmipredscat[,1]),col="red")
h <- h +
  geom_ribbon(data=preds,aes(x = grouped, ymin = bmipreds[,2], ymax = bmipreds[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")+
  geom_ribbon(data=preds2,aes(x = grouped, ymin = bmipredscat[,2], ymax = bmipredscat[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,fill="red",col="red")

h
summary(jfsbmilinmod)
summary(jfsbmicatmod)
## Maybe do bmi fruit juice plot
