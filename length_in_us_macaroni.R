demo <- read.csv("clean/DEMO_D.csv")

## Mac and cheese
modelmaccat <- lm(data = relevantdata,(Mac_Cheese_Consumed)~as.factor(Years_In_US))
relevantdata$fittedmaccat <-modelmaccat$fitted.values
modelmacnum <- lm(data = relevantdata,(Mac_Cheese_Consumed)~(Years_In_US))
relevantdata$fittedmacnum <-modelmacnum$fitted.values
mac <- ggplot(relevantdata)  +
  geom_boxplot(aes(x=as.factor(Years_In_US),y=Mac_Cheese_Consumed))
mac <- mac +
  geom_line(aes(x=(Years_In_US),y=fittedmaccat),col="red")+
  geom_line(aes(x=(Years_In_US),y=fittedmacnum),col="blue")
mac

## Lettuce
modelletcat <- lm(data = relevantdata,(Lettuce)~as.factor(Years_In_US))
relevantdata$fittedletcat <- modelletcat$fitted.values
modelletnum <- lm(data = relevantdata,(Lettuce)~(Years_In_US))
relevantdata$fittedletnum <-modelletnum$fitted.values
let <- ggplot(relevantdata)  +
  geom_boxplot(aes(x=as.factor(Years_In_US),y=Lettuce))
let <- let +
  geom_line(aes(x=(Years_In_US),y=fittedletcat),col="red")+
  geom_line(aes(x=(Years_In_US),y=fittedletnum),col="blue")
let



## French Fries
modelFFcat <- lm(data = relevantdata,(French_Fries)~as.factor(Years_In_US))
relevantdata$fittedFFcat <- modelFFcat$fitted.values
modelFFnum <- lm(data = relevantdata,(French_Fries)~(Years_In_US))
relevantdata$fittedFFnum <-modelFFnum$fitted.values
FF <- ggplot(relevantdata)  +
  geom_boxplot(aes(x=as.factor(Years_In_US),y=French_Fries))
FF <- FF +
  geom_line(aes(x=(Years_In_US),y=fittedFFcat),col="red")+
  geom_line(aes(x=(Years_In_US),y=fittedFFnum),col="blue")
FF

## Salsa
modelsalcat <- lm(data = relevantdata,(Salsa)~as.factor(Years_In_US))
relevantdata$fittedsalcat <- modelsalcat$fitted.values
modelsalnum <- lm(data = relevantdata,(Salsa)~(Years_In_US))
relevantdata$fittedsalnum <-modelsalnum$fitted.values
sal <- ggplot(relevantdata)  +
  geom_boxplot(aes(x=as.factor(Years_In_US),y=Salsa))
sal <- sal +
  geom_line(aes(x=(Years_In_US),y=fittedsalcat),col="red")+
  geom_line(aes(x=(Years_In_US),y=fittedsalnum),col="blue")
sal


## Pickles
modelpiccat <- lm(data = relevantdata,(Pickles)~as.factor(Years_In_US))
relevantdata$fittedpiccat <- modelpiccat$fitted.values
modelpicnum <- lm(data = relevantdata,(Pickles)~(Years_In_US))
summary(modelpicnum)
relevantdata$fittedpicnum <-modelpicnum$fitted.values
pic <- ggplot(relevantdata)  +
  geom_boxplot(aes(x=as.factor(Years_In_US),y=Pickles))
pic <- pic +
  geom_line(aes(x=(Years_In_US),y=fittedpiccat),col="red")+
  geom_line(aes(x=(Years_In_US),y=fittedpicnum),col="blue")
pic

## Pancakes
modelpancat <- lm(data = relevantdata,(Pancakes)~as.factor(Years_In_US))
relevantdata$fittedpancat <- modelpancat$fitted.values
modelpannum <- lm(data = relevantdata,(Pancakes)~(Years_In_US))
summary(modelpannum)
relevantdata$fittedpannum <-modelpannum$fitted.values
pan <- ggplot(relevantdata)  +
  geom_boxplot(aes(x=as.factor(Years_In_US),y=Pancakes))
pan <- pan +
  geom_line(aes(x=(Years_In_US),y=fittedpancat),col="red")+
  geom_line(aes(x=(Years_In_US),y=fittedpannum),col="blue")
pan


cbind(coef(modelletcat),coef(modelFFcat),coef(modelsalcat),coef(modelpiccat),coef(modelpancat),coef(modelmaccat))
cbind(coef(modelletnum),coef(modelFFnum),coef(modelsalnum),coef(modelpicnum),coef(modelpannum),coef(modelmacnum))

sumcat <- ggplot(relevantdata) +
  geom_line(aes(x=(Years_In_US),y=fittedletcat,color="Lettuce"))+
  geom_line(aes(x=(Years_In_US),y=fittedFFcat,color="French Fries"))+
  geom_line(aes(x=(Years_In_US),y=fittedsalcat,color="Salsa"))+
  geom_line(aes(x=(Years_In_US),y=fittedpiccat,color="Pickle"))+
  geom_line(aes(x=(Years_In_US),y=fittedpancat,color="Pancake"))+
  geom_line(aes(x=(Years_In_US),y=fittedmaccat,color="Mac and Cheese"))+
  scale_colour_manual(name="legend", values=c("blue", "red","cyan","pink","orange","green"))
  
sumnum <- ggplot(relevantdata) +
  geom_line(aes(x=(Years_In_US),y=fittedletnum,color="Lettuce"))+
  geom_line(aes(x=(Years_In_US),y=fittedFFnum,color="French Fries"))+
  geom_line(aes(x=(Years_In_US),y=fittedsalnum,color="Salsa"))+
  geom_line(aes(x=(Years_In_US),y=fittedpicnum,color="Pickle"))+
  geom_line(aes(x=(Years_In_US),y=fittedpannum,color="Pancake"))+
  geom_line(aes(x=(Years_In_US),y=fittedmacnum,color="Mac and Cheese"))+
  scale_colour_manual(name="legend", values=c("blue", "red","cyan","pink","orange","green"))

sumcat
sumnum
  