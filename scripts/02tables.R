#Loading Libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(GGally)

#Set working directory
#setwd("X:/CommonPrograms/Oral Health Services Research Core/Spiro/BradfordCarolyn")
setwd("C:/Users/stili/Documents/facial_recognition_project/data")

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

sapply(jawdata[,grep("delta", names(jawdata))], function(x){median(abs(x))})
sapply(jawdata[,grep("delta", names(jawdata))], function(x){summary(abs(x))})
options(scipen = 999)
sapply(jawdata[,grep("delta", names(jawdata))], function(x){wilcox.test(abs(x))$p.value})



delta_data <- read.csv("delta_data.csv", header = TRUE)
names(delta_data) <- c("pre_sna",
                       "pre_snb",
                       "pre_anb",
                       "pre_u1sn",
                       "pre_l1mp",
                       "pre_upperlip",
                       "pre_lowerlip",
                       "pre_convexity",
                       "post_sna",
                       "post_snb",
                       "post_anb",
                       "post_u1sn",
                       "post_l1mp",
                       "post_upperlip",
                       "post_lowerlip",
                       "post_convexity",
                       "delta_sna",
                       "delta_snb",
                       "delta_anb",
                       "delta_u1sn",
                       "delta_l1mp",
                       "delta_upperlip",
                       "delta_lowerlip",
                       "delta_convexity")

wilcox.test(delta_data$pre_sna, delta_data$post_sna, paired = TRUE)