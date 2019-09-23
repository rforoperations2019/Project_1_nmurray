library(dplR)
library(tidyverse)

crimes <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")

crimes <- data.frame(crimes)

#Exploratory Data Analysis 
colnames(crimes)
dim(crimes)
names(crimes)
colSums(is.na(crimes)) 


# Check out data values
lapply(crimes, class)
lapply(crimes, typeof)

#Save and write for Shiny Use
write.csv(crimes, file = "crimes.csv")

save(crimes, file = "crimes.Rdata")
