demo <- read.csv("clean/DEMO_D.csv")
bmi <- read.csv("clean/BMI.csv")
diet <- read.csv("clean/FFQRAW_D.csv")
library(ggplot2 )
library(dplyr)
library(tidyverse)
## BMI fits log normal dists
ggplot(data=bmi, aes(x=(BMXBMI))) +
  geom_histogram(bins=40)

## Create new data set with all three linked together by SEQN ID
linked_data <- merge(bmi,demo,by="SEQN")
linked_data <- merge(linked_data,diet,by="SEQN")
head(linked_data)
ggplot(linked_data,aes(x=WTS_FFQ))+
  geom_histogram()

## Not sure what WTS_FFQ, DRDINT and FFQ_MISS mean

## Need to describe what food type we want to have as EV, probs BMI as Outcome is easiest

## Add section to remove extreme BMI Data points
linked_data <- filter(linked_data, BMXBMI>10 & BMXBMI<60)
range(linked_data$BMXBMI)


## Perhaps - which fruit juice has biggest effect on BMI?
## Tomato, grape, apple, orange, '100%', or other - confounders of diet/sugar free
## Lets see what we can do with tomato

tomatojuicedata <- linked_data[, c("FFQ0001","BMXBMI", "FFQ0006A")]
## remove 'blanks'
tomatojuicedata$FFQ0001[tomatojuicedata$FFQ0001=="88"] <- NA
tomatojuicedata$FFQ0001[tomatojuicedata$FFQ0001=="99"] <- NA
tomatojuicedata <- na.omit(tomatojuicedata)
catmod <- lm(BMXBMI ~ as.factor(FFQ0001),data=tomatojuicedata)
catmod
tomatojuicedata$pred <- catmod$fitted.values
library(ciTools)
cipredict<-add_ci(data.frame(FFQ0001=c(1,2,3,4,5,6,7,8,9,10)), catmod, alpha = 0.05, names = NULL, yhatName = "pred")


#Plot the lines of predicted bmi based off categorical/numberic x variable 

g <- ggplot(tomatojuicedata,mapping= aes(x=FFQ0001,y=BMXBMI)) +
  geom_boxplot(aes(x=as.factor(FFQ0001),y=BMXBMI))  +
  geom_smooth(method="lm",formula=y~as.numeric(x), fill="#69b3a2",level=0.95) +
  geom_line(aes(x=FFQ0001,y=pred,group=1),data=cipredict,col='red')+
  scale_y_continuous(limits=c(0,70), breaks=seq(0,100,10), expand = c(0, 0))
g <- g + geom_ribbon(data = cipredict, aes(x = FFQ0001, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'red')
g

## Create data frame with just juice data
juicedata <- linked_data[, c("BMXBMI","FFQ0001","FFQ0002","FFQ0003","FFQ0004","FFQ0005","FFQ0006","FFQ0006A")]
colnames(juicedata) <- c("BMI","Tomato","Orange","Apple","Grape","hundred","Other","Diet")
head(juicedata)

## Remove rows where we have blank or error data
for (i in 1:length(names(juicedata))){
  juicedata[i][juicedata[i]=="88"] <- NA
  juicedata[i][juicedata[i]=="99"] <- NA
}

juicedata <- na.omit(juicedata)
nrow(juicedata)
names(juicedata)
## Generate the linear models and subsequently the predicted values describing each juice and bmi relationship
catmodtom <- lm(BMI ~ as.factor(Tomato),data=juicedata)
catmodora <- lm(BMI ~ as.factor(Orange),data=juicedata)
catmodapp <- lm(BMI ~ as.factor(Apple),data=juicedata)
catmodgra <- lm(BMI ~ as.factor(Grape),data=juicedata)
catmod100 <- lm(BMI ~ as.factor(hundred),data=juicedata)
catmodoth <- lm(BMI ~ as.factor(Other),data=juicedata)

juicedata$tompred <- catmodtom$fitted.values
juicedata$orapred <- catmodora$fitted.values
juicedata$apppred <- catmodapp$fitted.values
juicedata$grapred <- catmodgra$fitted.values
juicedata$pred100 <- catmod100$fitted.values
juicedata$othpred <- catmodoth$fitted.values

cipredicttom<-add_ci(data.frame(Tomato=c(1,2,3,4,5,6,7,8,9,10)), catmodtom, alpha = 0.05, names = NULL, yhatName = "pred")
cipredictora<-add_ci(data.frame(Orange=c(1,2,3,4,5,6,7,8,9,10)), catmodora, alpha = 0.05, names = NULL, yhatName = "pred")
cipredictapp<-add_ci(data.frame(Apple=c(1,2,3,4,5,6,7,8,9,10)), catmodapp, alpha = 0.05, names = NULL, yhatName = "pred")
cipredictgra<-add_ci(data.frame(Grape=c(1,2,3,4,5,6,7,8,9,10)), catmodgra, alpha = 0.05, names = NULL, yhatName = "pred")
cipredict100<-add_ci(data.frame(hundred=c(1,2,3,4,5,6,7,8,9,10)), catmod100, alpha = 0.05, names = NULL, yhatName = "pred")
cipredictoth<-add_ci(data.frame(Other=c(1,2,3,4,5,6,7,8,9,10)), catmodoth, alpha = 0.05, names = NULL, yhatName = "pred")

plotasfac <- ggplot() +
  geom_line(aes(x=Tomato,y=pred,color="Tomato"),data=cipredicttom) +
  geom_line(aes(x=Orange,y=pred,color="Orange"),data=cipredictora) +
  geom_line(aes(x=Apple,y=pred,color="Apple"),data=cipredictapp) +
  geom_line(aes(x=Grape,y=pred,color="Grape"),data=cipredictgra) +
  geom_line(aes(x=hundred,y=pred,color="hundred"),data=cipredict100) +
  geom_line(aes(x=Other,y=pred,color="Other"),data=cipredictoth) +
  scale_colour_manual(name="legend", values=c("green", "orange","pink","blue","cyan","red"))

plotasfac <- plotasfac +
  geom_ribbon(data = cipredicttom, aes(x = Tomato, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'red') +
  geom_ribbon(data = cipredictora, aes(x = Orange, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'blue') +
  geom_ribbon(data = cipredictapp, aes(x = Apple, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'green') +
  geom_ribbon(data = cipredictgra, aes(x = Grape, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'orange') +
  geom_ribbon(data = cipredict100, aes(x = hundred, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'pink') +
  geom_ribbon(data = cipredictoth, aes(x = Other, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'cyan')
plotasfac


## Now produce numeric models of linear relationship
plotasnum <- ggplot(juicedata) +
  geom_smooth(method="lm",mapping=aes(x=Tomato,y=BMI,color="Tomato"),formula=y~as.numeric(x), fill="red",level=0.95) +
  geom_smooth(method="lm",mapping=aes(x=Orange,y=BMI,color="Orange"),formula=y~as.numeric(x), fill="blue",level=0.95) +
  geom_smooth(method="lm",mapping=aes(x=Apple,y=BMI,color="Apple"),formula=y~as.numeric(x), fill="green",level=0.95) +
  geom_smooth(method="lm",mapping=aes(x=Grape,y=BMI,color="Grape"),formula=y~as.numeric(x), fill="orange",level=0.95) +
  geom_smooth(method="lm",mapping=aes(x=hundred,y=BMI,color="hundred"),formula=y~as.numeric(x), fill="pink",level=0.95) +
  geom_smooth(method="lm",mapping=aes(x=Other,y=BMI,color="Other"),formula=y~as.numeric(x), fill="cyan",level=0.95) +
  scale_colour_manual(name="legend", values=c("green", "orange","pink","blue","cyan","red"))
  
plotasnum

head(juicedata)


png(file="plots/factorplot.png",
    width=600, height=350)
plotasfac
dev.off()


png(file="plots/numericplot.png",
    width=600, height=350)
plotasnum
dev.off()

## Add a factored version of BMI to the data set
trimmed_data$bmigrp <- ntile(trimmed_data$BMX,10) 

library(png)
img <- readPNG("plots/factorplot.png")
