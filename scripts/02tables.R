#Loading Libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(GGally)

#Set working directory
setwd("X:/CommonPrograms/Oral Health Services Research Core/Spiro/BradfordCarolyn")

#Reading in data
jawdata <- readRDS("jawdata.rds")

#making tables

##demographics
table(jawdata$jaw_number)
prop.table(table(jawdata$jaw_number))

table(jawdata$gender)
prop.table(table(jawdata$gender))

table(jawdata$surgery_type)
prop.table(table(jawdata$surgery_type))

table(jawdata$ethnicity)
prop.table(table(jawdata$ethnicity))

mean(jawdata$age)
sd(jawdata$age)

##facial agreement
median(jawdata[,9])
range(jawdata[,9])

median(jawdata[,10])
range(jawdata[,10])

median(jawdata[,11])
range(jawdata[,11])

median(jawdata[,12])
range(jawdata[,12])

##Delta variables
sapply(jawdata[,grep("delta", names(jawdata))], median)
sapply(jawdata[,grep("delta", names(jawdata))], IQR)
