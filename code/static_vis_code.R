## This file will create static visualisations
library(ggplot2)
clean_data <- read.csv("clean/us_food.csv")
head(clean_data)


## I need to code the jfs - house hold income model - adjust for confounding number in household, time in us?
jfsincomemod <- lm(jfs~as.factor(Household_Income),data=clean_data)
jhpred <- data.frame(Household_Income = 1:13)
jhpred$jfspred <-predict(jfsincomemod,newdata=jhpred,interval="confidence")
j<- ggplot(data = clean_data, mapping = aes(x=as.factor(Household_Income),y=jfs)) +
  geom_boxplot()
j<-j+
  geom_line(data=jhpred,mapping=aes(x=Household_Income,y=jfspred[,1]))
j <- j +
  geom_ribbon(data=jhpred,aes(x = Household_Income, ymin = jfspred[,2], ymax = jfspred[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")
j

## jfs - bmi model
clean_data$grouped <- ntile(clean_data$BMI,10)
jfsbmilinmod <- lm(jfs~BMI,data=clean_data)
jfsbmicatmod <- lm(jfs~as.factor(grouped),data=clean_data)
summary(jfsbmilinmod)
summary(jfsbmicatmod)
preds<- data.frame(BMI = c(1,2,3,4,5,6,7,8,9,10))
preds2<- data.frame(grouped = c(1,2,3,4,5,6,7,8,9,10))
preds$jfspreds <- predict(jfsbmilinmod,newdata=preds,interval="confidence")
preds2$jfspredscat <- predict(jfsbmicatmod,newdata=preds2,interval="confidence")
head(preds)
dim(preds)
ggplot(data = clean_data, mapping = aes(x=as.factor(Household_Income),y=jfs)) +
  geom_boxplot()
h<- ggplot(data = clean_data, mapping = aes(x=as.factor(grouped),y=jfs)) +
  geom_boxplot()
head(preds)
h<-h+
  geom_line(data=preds,mapping=aes(x=BMI,y=jfspreds[,1]))+
  geom_line(data=preds2,mapping=aes(x=grouped,y=jfspredscat[,1]),col="red")
h <- h +
  geom_ribbon(data=preds,aes(x = BMI, ymin = jfspreds[,2], ymax = jfspreds[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")+
  geom_ribbon(data=preds2,aes(x = grouped, ymin = jfspredscat[,2], ymax = jfspredscat[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,fill="red",col="red")

h

## Maybe do bmi fruit juice plot

