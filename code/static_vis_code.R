## This file will create static visualisations
library(ggplot2)
library(dplyr)
## Read the clean data from the where we saved it with the data prep file
clean_data <- read.csv("clean/us_food.csv")

## Establish list of foods to use as headings - could extract from data but want to change some
## so easier to do manually.
food <- c("Fries","Pancakes", "Mac_and_Cheese","Peanut_Butter","Hamburgers","Spareribs", "Fried_Fish","Cake", "Cookies","Choc_Candy","jfs")
## Define gap variable which we later use as explanatory variable values for predictions
gap<-1:11
## Define the explanatory variable for our first plot
xvar <- clean_data$Household_Income
## Initiate a data frame with E.V. values that we can add predictins to
predictions <- data.frame(Index = gap)
## Iteratively add the predictions for each food tot he dataframe using a for loop
for (i in 1:length(food)){
    y1 <- clean_data[,food[i]]
    predictions <- cbind(predictions,predict(lm(y1~xvar,data=clean_data),method=binomial(family="logit"),newdata = data.frame(xvar=1:11)))
}
## Rename prediction data frame columns for interpretability
colnames(predictions)<-c("Index",food)
## Transform the data frame, combining all predictions into one vector to make use of in built
## ggplot function
df=data.frame(x=1:11, 
              values=c(predictions[,2],
                       predictions[,3],
                       predictions[,4],
                       predictions[,5],
                       predictions[,6],
                       predictions[,7],
                       predictions[,8],
                       predictions[,9],
                       predictions[,10],
                       predictions[,11],
                       predictions[,12]/10), 
              food=c(rep("Fries",11),
                     rep("Pancakes",11),
                     rep("Mac_and_Cheese",11),
                     rep("Peanut_Butter",11),
                     rep("Hamburgers",11),
                     rep("Spareribs",11),
                     rep("Fried_Fish",11),
                     rep("Cake",11),
                     rep("Cookies",11),
                     rep("Choc_Candy",11),
                     rep("Junk-Food-Score \n (Standardized)",11)),stringsAsFactors=T
) 
## Reorder the different food types to appear in plot as we originally intended
df$food <- factor(df$food, levels = c("Junk-Food-Score \n (Standardized)",food))

## Produce plot - because of data frame formatting this is a very simple process
g <- ggplot(df,aes(x,values,col=food,linetype=food,linewidth = food)) +
  geom_line() + 
  ## Format the plot to have labels, a title and change the colors, width and types of the
  ## lines so they're identifiable from the legend
  scale_linetype_manual(values = c(4,rep(1,10))) + 
  scale_linewidth_manual(values = c(1.5,rep(.75,10)))+
  labs(title="Plot demonstrating the relationships between amounts \n of different foods eaten and Salary",
       x ="Household Income", y = "Quantity of food eaten")+
  theme(plot.title = element_text(hjust = 0.5))

## Now we want to product our second plot which investigates BMI against JFS
## The first task is to separate the junk food scores into 10 groups so that we can plot the
## BMIs for each group easily as boxplots
clean_data$grouped <- ntile(clean_data$jfs,10)
## We then fit a linear regression line for BMI against the grouped JFS variable with a 
## confounder of household income
jfsbmilinmod <- lm(BMI~grouped + Household_Income,data=clean_data)
## We don't want these to display in output but they give values to use in the report
# summary(jfsbmilinmod)
# cbind(coef(jfsbmilinmod),confint(jfsbmilinmod))
## This for loop calculates the mean household income for each of our subgroups to use as the 
## value for the new data in the predict function. This is method one of our prediction
means=c()
for (i in 1:10){
  means=c(means,mean(clean_data$Household_Income[clean_data$grouped==i]))
}
## Produce new data data frame
preds<- data.frame(grouped = c(1,2,3,4,5,6,7,8,9,10),Household_Income=means)
## Use the predict function to predict from our model
preds$bmipreds <- predict(jfsbmilinmod,newdata=preds,interval="confidence")

## Outputs we require for report write up
#cbind(coef(jfsbmilinmod),confint(jfsbmilinmod))
#summary(jfsbmilinmod)

## For method 2 we will stratify accross household income and take a weighted average of the
## predictions. 
## We again create a data frame to add predictions to but only values for JFS as by stratifying
## Household income is no longer an explanatory variable in the model
preds2 <-data.frame(grouped = c(1,2,3,4,5,6,7,8,9,10)) 
## Produce 0 data frame to help calculate the weighted average
sum <- data.frame(matrix(rep(0,30),nrow=10))
## Also for the weighted average
w_isum <- 0
## For loop to obtain stratified predictions, we use 10 strata
for (i in 1:10){
  ## Produce strata by taking subsets of the data
  subsetdata <- data.frame(subset(clean_data,clean_data$Household_Income==i))
  ## Within the strata produce a model to predict from
  model <- lm(BMI~grouped,data=subsetdata)
  ## Define weights for our weighted average as 1/s^2 = 1/variance, so find variance of
  ## predictions. Find strata predictions before combining to the sum which ultimately gives
  ## weighted predictions.
  apred <- predict(model,newdata=data.frame(preds2),interval="confidence",se.fit=TRUE)
  ## Calc weights
  w_i <- 1/(apred$se.fit^2)
  ## Ammend sums
  sum <- sum + w_i * apred$fit
  w_isum <- w_isum+w_i
}
## Produce final weighted predictions
preds2$BMI <- sum/w_isum


## Now we produce a plot which shows the original adjusted model in black and stratified 
## predictions in red, both with 95% CIs, these are shown atop the raw box plot data
## We do this in sections for clarity
h<- ggplot(data = clean_data, mapping = aes(x=as.factor(grouped),y=BMI)) +
  geom_boxplot()
h<-h+
  geom_line(data=preds,mapping=aes(x=grouped,y=bmipreds[,1]))+
# Stratified line
  geom_line(data=preds2,mapping=aes(x=grouped,y=BMI[,1]),col="red")
h <- h +
  geom_ribbon(data=preds,aes(x = grouped, ymin = bmipreds[,2], ymax = bmipreds[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")+
# Stratified ribbon
  geom_ribbon(data=preds2,aes(x = grouped, ymin = BMI[,2], ymax = BMI[,3]),inherit.aes = FALSE, linetype=2, alpha=0.1,fill="red",col="red")
h <- h +
  labs(title="Plot demonstrating the relationships between BMI \n and Junk Food Score",
       x ="Junk-Food-Score (grouped into 10 sets)", y = "BMI")+
  theme(plot.title = element_text(hjust = 0.5))

## Save the plots to 'visualisation', as .jpegs, so that they can be used in the report
jpeg(file="visualisation/jfs_bmi_plot.jpeg")
h
dev.off()
jpeg(file="visualisation/food_household_plot.jpeg")
g
dev.off()
