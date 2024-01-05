## Read in raw data and lode relevant libraries
demo <- read.csv("raw/DEMO_D.csv")
bmi <- read.csv("raw/BMI.csv")
diet <- read.csv("raw/FFQRAW_D.csv")
library(dplyr)

## This file takes the raw data provided and produces a new dataframe which we then write into the clean data folder

## First stage is to create a very large linked dataset

names(bmi)
## The relevant factors we want to take from the raw data (excluding foods) are: BMI - BMXBMI, Household Income - INDHHINC, Age(Months) - RIDAGEMN, # In household - DMDHHSIZ 
# Extract these from the 'demo' database
trimmed_demo <- demo[,c("SEQN","INDHHINC","RIDAGEMN","DMDHHSIZ")]
## Then we want the following codes for junk food: (FFQ____)
# french fries 0047, pancakes, mac and cheese 0061, peanut butter 0068, hamburgers 0075, spareribs 0081, fried fish 0094, cake 0114, cookies 0115, chocolate candy 0120
# Extract these from the 'diet' database
trimmed_diet <- diet[ , c("SEQN","FFQ0047","FFQ0059","FFQ0061","FFQ0068","FFQ0075","FFQ0081","FFQ0094","FFQ0114","FFQ0115","FFQ0120")]

## Now link these together using the SEQN column
linked_data <- merge(bmi,trimmed_demo,by="SEQN")
linked_data <- merge(linked_data,trimmed_diet,by="SEQN")

## The first stage of data cleaning for this will be removing the individuals who
## have a BMI outside the range [10,80] as these are likely to be errors, or if not
## they won't be representative of the general population so we exclude them from
## our study
us_food <- dplyr::filter(linked_data, BMXBMI >10 & BMXBMI <80)

## Rename columns to be more informative
colnames(us_food) <- c("ID","BMI","Household_Income","Age(Months)","Number_in_household?","Fries","Pancakes","Mac_and_Cheese","Peanut_Butter","Hamburgers","Spareribs","Fried_Fish","Cake","Cookies","Choc_Candy")

## Now we produce the us food data set and remove any non-informative responses
## This involves removing responses of 88 - 'blank' - or 99 -'error'. All responses should be on a scale 1-11


## Rename any values not on 1 to 11 scale as NA so can easily be omitted
for (col in 1:length(colnames(us_food))){
  ## Filter our extra 2 for this column
  if (col > 5){
    us_food[,col][us_food[,col]=="77" | us_food[,col]=="88" | us_food[,col]=="99" | us_food[,col]=="."] <- NA
  }else if(col == 3){
    us_food[,col][us_food[,col]=="12" | us_food[,col]=="13" | us_food[,col]=="77" | us_food[,col]=="88" | us_food[,col]=="99" | us_food[,col]=="."] <- NA
  }
}
us_food <- na.omit(us_food)
## This leaves 5306 valid participants with 'full data' - still a moderate sample
## Now lets create the 'junk food score', this is just a sum of score on all other food factors
us_food$jfs <- rowSums(us_food[7:length(colnames(us_food))])
dim(us_food)

summary(us_food$jfs)
## Write this to cleaned data folder
write.csv(us_food, "clean/us_food.csv", row.names=FALSE)


