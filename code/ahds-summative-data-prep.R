
## have a look at the nhanes diet, bmi and demographic data, and save to csv
## data from here - https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=2005
## https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005
## https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&Cycle=2005-2006

## diet questionnaire
library(foreign)
diet <- read.csv("raw/FFQRAW_D.csv")
head(diet)
diet<-na.omit(diet)
write.csv(diet, "clean/FFQRAW_D.csv", row.names=FALSE)

## bmi
bmi <- read.csv("raw/BMI.csv")
bmi<-na.omit(bmi)
head(bmi)
write.csv(bmi, "clean/BMX_D.csv", row.names=FALSE)

# save id and bmi only from BMXBMI data
bmi = bmi[, c('SEQN', 'BMXBMI')]
write.csv(bmi, 'clean/BMI.csv', row.names=FALSE)


## demographics
demo <- read.csv("raw/DEMO_D.csv")
head(demo)
#demo<-na.omit(demo)
write.csv(demo, "clean/DEMO_D.csv", row.names=FALSE)
