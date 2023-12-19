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
  scale_y_continuous(limits=c(0,50), breaks=seq(0,100,10), expand = c(0, 0))
g <- g + geom_ribbon(data = cipredict, aes(x = FFQ0001, ymin = LCB0.025, ymax = UCB0.975),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'red')
g


## Reduce the data to just the columns we care about - need to decide which these are
trimmed_data <- linked_data[,c("SEQN","BMXBMI","RIDEXMON","SIAINTRP","FFQ0135")]
head(trimmed_data)
## Add a factored version of BMI to the data set
trimmed_data$bmigrp <- ntile(trimmed_data$BMX,10) 
