## Read in raw data and lode relevant libraries
demo <- read.csv("raw/DEMO_D.csv")
bmi <- read.csv("raw/BMI.csv")
diet <- read.csv("raw/FFQRAW_D.csv")
library(dplyr)

## This file takes the raw data provided and produces a new dataframe which we then write into the clean data folder

## First stage is to create a very large linked dataset
linked_data <- merge(bmi,demo,by="SEQN")
linked_data <- merge(linked_data,diet,by="SEQN")

## In this project I'm going to conduct a study into the effect of household income on how much junk food is eaten
## We'll visualize this for food treated separately and with a general 'junk food score'(jfs)
## We will adjust for factors like # in household and length of time in the US

## Finally we will conclude by looking at the relationship between BMI and jfs.

## The first stage of data cleaning for this will be removing the individuals who
## have a BMI outside the range [10,80] as these are likely to be errors, or if not
## they won't be representative of the general population so we exclude them from
## our study
linked_data <- dplyr::filter(linked_data, BMXBMI >10 & BMXBMI <80)

## The relevant factors we want to take from the raw data (excluding foods) are: BMI - BMXBMI, Household Income - INDHHINC, Yrs in US - DMDYRSUS, Age(Months) - RIDAGEMN, # In household - DMDHHSIZ 
## Then we want the following codes for junk food: (FFQ____)
# french fries 0047, pancakes 0059 (a w. syrup), mac and cheese 0061, peanut butter 0068, hamburgers 0075, hot dogs 0077, spareribs 0081, fried fish 0094, pizza 0098, potato chips 0102, tortilla chips 0103, popcorn 0104, pretzels 0105, ice cream 0112, cake 0114, cookies 0115, doughnuts 0116, sweet muffins 0117, chocolate candy 0120, other candy 0121

## First we produce the us food data set and remove any non-informative responses
## This involves removing responses of 88 - 'blank' - or 99 -'error'. All responses should be on a scale 1-11

## Extact the relevant columns
us_food <- linked_data[ ,c("SEQN","BMXBMI","INDHHINC","DMDYRSUS","RIDAGEMN","DMDHHSIZ","FFQ0047","FFQ0059","FFQ0059A","FFQ0061","FFQ0068","FFQ0075","FFQ0077","FFQ0081","FFQ0094","FFQ0098","FFQ0102","FFQ0103","FFQ0104","FFQ0105","FFQ0112","FFQ0114","FFQ0115","FFQ0116","FFQ0117","FFQ0120","FFQ0121")]

## Rename columns to be more informative
colnames(us_food) <- c("ID","BMI","Household_Income","Yrs_in_US","Age(Months)",">6_in_household?","Fries","Pancakes","Pancackes_syrup","Mac_and_Cheese","Peanut_Butter","Hamburgers","Hot_Dogs","Spareribs","Fried_Fish","Pretzels","Potato_Chips","Tortilla_Chips","Popcorn","Pretzels","Ice_Cream","Cake","Cookies","Doughnuts","Sweet_Muffins","Choc_Candy","Oth_Candy")

## Rename any values not on 1 to 11 scale as NA so can easily be omitted
for (col in 1:length(colnames(us_food))){
  ## Careful to not filter by this for age in months
  if (col != 5){
    us_food[,col][us_food[,col]=="77" | us_food[,col]=="88" | us_food[,col]=="99" | us_food[,col]=="."] <- NA
  }
}
us_food <- na.omit(us_food)
## This leaves 576 valid participants with 'full data' - still a moderate sample
## Now lets create the 'junk food score', this is just a sum of score on all other food factors
us_food$jfs <- rowSums(us_food[7:27])

## Write this to cleaned data folder
write.csv(us_food, "clean/us_food.csv", row.names=FALSE)



