demo <- read.csv("clean/DEMO_D.csv")
bmi <- read.csv("clean/BMI.csv")
diet <- read.csv("clean/FFQRAW_D.csv")
## This r file will look to find association between household income and what 
## fruit juice is drunk - both are categorical variables so we should do this with logistic regression
head(demo)

## Create new data set with all three linked together by SEQN ID
linked_data <- merge(bmi,demo,by="SEQN")
linked_data <- merge(linked_data,diet,by="SEQN")

## Need to describe what food type we want to have as EV, probs BMI as Outcome is easiest

## Add section to remove extreme BMI Data points
linked_data <- filter(linked_data, BMXBMI>10 & BMXBMI<60)
range(linked_data$BMXBMI)
head(linked_data)

## Now extract from linked data the relecant columns
fruitwagedata <- linked_data[, c("BMXBMI","INDHHINC","FFQ0001","FFQ0002","FFQ0003","FFQ0004","FFQ0005","FFQ0006","FFQ0006A")]
  
  
for (i in 1:length(names(fruitwagedata))){
  fruitwagedata[i][fruitwagedata[i]=="77"] <- NA
  fruitwagedata[i][fruitwagedata[i]=="88"] <- NA
  fruitwagedata[i][fruitwagedata[i]=="99"] <- NA
}
fruitwagebin <- fruitwagedata
fruitwagedata[2][fruitwagedata[2]=="12"] <- NA
fruitwagedata[2][fruitwagedata[2]=="13"] <- NA
fruitwagedata <- na.omit(fruitwagedata)
fruitwagebin[2][fruitwagebin[2] <= 4 | fruitwagebin[2] == 13] <- 0
fruitwagebin[2][fruitwagebin[2] <= 12 & fruitwagebin[2] >= 5] <- 1
fruitwagebin[2][fruitwagebin[2] > 13] <- NA
fruitwagebin <- na.omit(fruitwagebin)
head(fruitwagebin)
head(fruitwagedata)

tommod <- glm(data=fruitwagebin,formula = as.factor(INDHHINC) ~ as.factor(FFQ0001),family = "binomial")
coefs<- tommod$coefficients
exp(coef(tommod))
plot.dat <- data.frame(prob=fruitwagebin$INDHHINC,
                       fruit=fruitwagebin$FFQ0001,
                       fit=predict(tommod,fruitwagebin))
plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))
ggplot(plot.dat,aes(x=fruit,y=prob)) +
  geom_point() +
  geom_line(aes(x=fruit,y=fit_prob))


ggplot(data=fruitwagebin,aes(x=as.factor(FFQ0001),y=INDHHINC))+
  geom_boxplot() 
ggplot(data=fruitwagedata,aes(x=as.factor(FFQ0002),y=INDHHINC))+
  geom_boxplot()
ggplot(data=fruitwagedata,aes(x=as.factor(FFQ0003),y=INDHHINC))+
  geom_boxplot()
ggplot(data=fruitwagedata,aes(x=as.factor(FFQ0004),y=INDHHINC))+
  geom_boxplot()
ggplot(data=fruitwagedata,aes(x=as.factor(FFQ0005),y=INDHHINC))+
  geom_boxplot()
