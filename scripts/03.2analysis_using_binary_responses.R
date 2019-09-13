#Loading Libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(Hmisc)
#library("PerformanceAnalytics")
library(MVN)
library(MASS)
library(gridExtra)
library(ggpubr)
library(ggfortify)
library(GGally)
library(reshape2)
library(ggcorrplot)
library(rcompanion) 

#Set working directory
setwd("X:/CommonPrograms/Oral Health Services Research Core/Spiro/BradfordCarolyn")

#Reading in data
jawdata <- readRDS("jawdata.rds")

#binarizing the response variable
jawdata[,grep("pre_", names(jawdata))] <- apply(jawdata[,grep("pre_", names(jawdata))], 2, function(x){ifelse(x<99,0,1)})
jawdata$pre_relaxed_v_post_relaxed <- factor(jawdata$pre_relaxed_v_post_relaxed)
jawdata$pre_smile_v_post_smile <- factor(jawdata$pre_smile_v_post_smile)
jawdata$pre_relaxed_v_post_smile <- factor(jawdata$pre_relaxed_v_post_smile)
jawdata$pre_smile_v_post_relaxed <- factor(jawdata$pre_smile_v_post_relaxed)
str(jawdata)

#logistic regression
##Relaxed: Pre vs Post
table(jawdata$pre_relaxed_v_post_relaxed)
###stepwise selection
####null model
glm.null <- glm(pre_relaxed_v_post_relaxed ~ 1, 
                data = jawdata, 
                family = binomial)
#full model
glm.full <- glm(pre_relaxed_v_post_relaxed ~ delta_sna + delta_snb + delta_anb + 
                                             delta_u1sn + deltal_l1mp + delta_upperlip +
                                             delta_lowerlip + delta_convexity,
                data = jawdata, 
                family = binomial)

best.fit.both.relaxed = step(glm.null,
                     scope=list(lower=formula(glm.null),
                                upper=formula(glm.full)), 
                     direction="both",
                     trace=0)

summary(best.fit.both.relaxed)

##Relaxed: Pre vs Post
table(jawdata$pre_smile_v_post_smile)
###stepwise selection
####null model
glm.null <- glm(pre_smile_v_post_smile ~ 1, 
                data = jawdata, 
                family = binomial)
#full model
glm.full <- glm(pre_smile_v_post_smile ~ delta_sna + delta_snb + delta_anb + 
                  delta_u1sn + deltal_l1mp + delta_upperlip +
                  delta_lowerlip + delta_convexity,
                data = jawdata,
                control = list(maxit = 50),
                family = binomial)

best.fit.both.smile = step(glm.null,
                             scope=list(lower=formula(glm.null),
                                        upper=formula(glm.full)), 
                             direction="both",
                             trace=0)

summary(best.fit.both.smile)

##Pre relaxed vs Post smile
table(jawdata$pre_relaxed_v_post_smile)
###stepwise selection
####null model
glm.null <- glm(pre_relaxed_v_post_smile ~ 1, 
                data = jawdata, 
                family = binomial)
#full model
glm.full <- glm(pre_relaxed_v_post_smile ~ delta_sna + delta_snb + delta_anb + 
                  delta_u1sn + deltal_l1mp + delta_upperlip +
                  delta_lowerlip + delta_convexity,
                data = jawdata,
                control = list(maxit = 50),
                family = binomial)

best.fit.both.prerelaxed.postsmile = step(glm.null,
                           scope=list(lower=formula(glm.null),
                                      upper=formula(glm.full)), 
                           direction="both",
                           trace=0)

summary(best.fit.both.prerelaxed.postsmile)

##Pre smile vs Post relaxed
table(jawdata$pre_smile_v_post_relaxed)
###stepwise selection
####null model
glm.null <- glm(pre_smile_v_post_relaxed ~ 1, 
                data = jawdata, 
                family = binomial)
#full model
glm.full <- glm(pre_smile_v_post_relaxed ~ delta_sna + delta_snb + delta_anb + 
                  delta_u1sn + deltal_l1mp + delta_upperlip +
                  delta_lowerlip + delta_convexity,
                data = jawdata,
                control = list(maxit = 50),
                family = binomial)

best.fit.both.presmile.postrelaxed = step(glm.null,
                                          scope=list(lower=formula(glm.null),
                                                     upper=formula(glm.full)), 
                                          direction="both",
                                          trace=0)

summary(best.fit.both.presmile.postrelaxed)

