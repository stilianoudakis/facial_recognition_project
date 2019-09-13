#Loading Libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(GGally)

#Set working directory
setwd("X:/CommonPrograms/Oral Health Services Research Core/Spiro/BradfordCarolyn")

#Reading in data
#jawdata <- read.xlsx("Data_for_Initial_Eval_bradford.xlsx",  
#                     sheet = 1,
#                     startRow = 1,
#                     detectDates = TRUE,
#                     colNames = TRUE)

#New dataset
jawdata <- read.xlsx("Ceph Data for Analysis_Bradford.xlsx",  
                     sheet = 1,
                     startRow = 1,
                     rows = c(1:26),
                     detectDates = TRUE,
                     colNames = TRUE)

dim(jawdata) #25 44

#Keeping only necessary columns
jawdata <- jawdata[,c(1,25:36,40:44,37:39)]

#changing names of data
names(jawdata) <- c("id",
                    "delta_sna",
                    "delta_snb",
                    "delta_anb",
                    "delta_u1sn",
                    "deltal_l1mp",
                    "delta_upperlip",
                    "delta_lowerlip",
                    "delta_convexity",
                    "pre_relaxed_v_post_relaxed",
                    "pre_smile_v_post_smile",
                    "pre_relaxed_v_post_smile",
                    "pre_smile_v_post_relaxed",
                    "jaw_number",
                    "gender",
                    "surgery_type",
                    "ethnicity",
                    "age",
                    "control1",
                    "control2",
                    "control3")

#Data exploration
##ID
which(is.na(jawdata$id)) #0

##DOB
#class(jawdata$dob)
#which(is.na(jawdata$dob)) #0

##surgery date
#class(jawdata$dob)
#which(is.na(jawdata$surgery_date)) #0

##jaw number
table(jawdata$jaw_number, exclude = "ifany")
#Both  Man  Max 
#13    5    7 

##gender
table(jawdata$gender, exclude = "ifany")
#F  M 
#12 13 

##surger type
table(jawdata$surgery_type, exclude = "ifany")
length(table(jawdata$surgery_type, exclude = "ifany")) #19 separate categories
###patients with just BSSO
setdiff(grep("BSSO", jawdata$surgery_type), grep("LeFort", jawdata$surgery_type))
###patients with just LeFort
setdiff(grep("LeFort", jawdata$surgery_type), grep("BSSO", jawdata$surgery_type))
###patient with both BSSO or Lefort
intersect(grep("LeFort", jawdata$surgery_type), grep("BSSO", jawdata$surgery_type))

###recategorizing surgery type as BSSO, LeFort, or Both
jawdata$surgery_type[setdiff(grep("BSSO", jawdata$surgery_type), grep("LeFort", jawdata$surgery_type))] <- "BSSO"
jawdata$surgery_type[setdiff(grep("LeFort", jawdata$surgery_type), grep("BSSO", jawdata$surgery_type))] <- "Lefort"
jawdata$surgery_type[intersect(grep("LeFort", jawdata$surgery_type), grep("BSSO", jawdata$surgery_type))] <- "Both"

table(jawdata$surgery_type)
#Both   BSSO Lefort 
#12      5      8 

##cross tabulating jaw and surgery; should be a 1:1 relationship
table(jawdata$jaw_number, jawdata$surgery_type)
###recode patient with surgery==Lefort & jawd==both as jaw==Max
jawdata$jaw_number[which(jawdata$surgery_type=="Lefort" & jawdata$jaw_number=="Both")] <- "Max"
###sanity check
table(jawdata$jaw_number, jawdata$surgery_type)

##ethnicity
table(jawdata$ethnicity, exclude = "ifany")

###recategorizing as white, black, other
jawdata$ethnicity[-which(jawdata$ethnicity=="African American" |
                          jawdata$ethnicity=="Caucasian")] <- "Other"
table(jawdata$ethnicity)
#African American        Caucasian            Other 
#8               14                3 

##age
summary(jawdata$age)
hist(jawdata$age) 

##pre vs post relaxed pictures
summary(jawdata$pre_relaxed_v_post_relaxed)
hist(jawdata$pre_relaxed_v_post_relaxed) #left skewed
hist(log(jawdata$pre_relaxed_v_post_relaxed))
#hist(sqrt(jawdata$pre_relaxed_v_post_relaxed))
#hist((jawdata$pre_relaxed_v_post_relaxed)^2)
#hist((jawdata$pre_relaxed_v_post_relaxed)^(1/3))

##pre vs post smile pictures
summary(jawdata$pre_smile_v_post_smile)
hist(jawdata$pre_smile_v_post_smile)

##pre vs post; relaxed vs smile
summary(jawdata$pre_relaxed_v_post_smile)
hist(jawdata$pre_relaxed_v_post_smile)
hist((jawdata$pre_relaxed_v_post_smile)^(1/3))

##pre vs post; smile vs relaxed
summary(jawdata$pre_smile_v_post_relaxed)
hist(jawdata$pre_smile_v_post_relaxed)
#hist(sqrt(jawdata$pre_smile_v_post_relaxed))

##pre vs pre; relaxed vs smile
#summary(jawdata$pre_relaxed_v_pre_smile_control)
#hist(jawdata$pre_relaxed_v_pre_smile_control)

##post vs post; relaxed vs smile
#summary(jawdata$post_relaxed_v_post_smile_control)
#hist(jawdata$post_relaxed_v_post_smile_control)

#save data
jawdata$jaw_number <- as.factor(jawdata$jaw_number)
jawdata$gender <- as.factor(jawdata$gender)
jawdata$surgery_type <- as.factor(jawdata$surgery_type)
jawdata$ethnicity <- as.factor(jawdata$ethnicity)

saveRDS(jawdata, "jawdata.rds")

