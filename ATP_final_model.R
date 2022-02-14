###### This code is to do a final analysis with the established ATP model
  
#### Libraries needed:
  
library(sqldf)
library(ggplot2)
library(olsrr)
library(nlme)

#### Dataset needed:
oocInfo = read.csv(file = "../../transformed_data/final_dataset.csv")

#### Running the linear model
atp = oocInfo$atp_pg
year = oocInfo$year
timeG2toFA = oocInfo$timeG2toFA
g2e2 = oocInfo$g2e2
g2_measure = oocInfo$g2_measure

summary(lme(atp ~ timeG2toFA + g2e2 + g2_measure, random = (~1|year), na.action = na.omit))