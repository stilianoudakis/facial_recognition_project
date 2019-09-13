#Loading Libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(Hmisc)
library("PerformanceAnalytics")
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

###########################################################################################



###########################################################################################

#looking at correlations among the response variables:
#pre vs post: relaxed and
#pre vs post: smile
#pre relaxed vs post smile
#pre smile vs post relaxed

#ggpairs(jawdata[,grep("pre", names(jawdata))], columnLabels = c("Relaxed: Pre vs Post",
#                                         "Smile: Pre vs Post",
#                                         "Relaxed Pre vs Smiling Post",
#                                         "Smiling Pre vs Relaxed Post"))

cormat <- round(cor(jawdata[,grep(paste(c("pre", "delta"), collapse = "|"), names(jawdata))], method="spearman"),2)

cordat <- rcorr(as.matrix(jawdata[,grep(paste(c("pre", "delta"), collapse = "|"), names(jawdata))]), type = c("spearman"))

cormat <- round(cordat$r,2)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman \n Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels=c("Pre Relaxed vs Post Relaxed",
                            "Pre Smile vs Post Smile",
                            "Pre Relaxed vs Post Smile",
                            "Pre Smile vs Post Relaxed"))+
  scale_y_discrete(labels=c("Pre Relaxed vs Post Relaxed",
                            "Pre Smile vs Post Smile",
                            "Pre Relaxed vs Post Smile",
                            "Pre Smile vs Post Relaxed"))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 20, hjust = 1),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20))+
  coord_fixed()

#there is high correlation between both response variables
#consider fitting multivariate regression model

#M <-cor(jawdata[,grep("pre", names(jawdata))])
#corrplot(M, type="upper", order="hclust",
#         col=brewer.pal(n=8, name="RdYlBu"))

#rcorr(as.matrix(jawdata[,grep("pre", names(jawdata))]), type = "pearson")$P

#chart.Correlation(jawdata[,grep("pre", names(jawdata))], histogram=TRUE, pch=19)

#checking assumption of multivariate data
#mvn(data = jawdata[,grep("pre", names(jawdata))],subset = NULL,mvnTest = "royston")
#assumption is violated

#check outliers using Mahalanobis distance
#outdata <- mvn(data = jawdata[,grep("pre", names(jawdata))], multivariateOutlierMethod = "adj",
#showOutliers = TRUE, showNewData = TRUE)

#remove outliers
#newdata <- outdata$newData
#jawdata2 <- jawdata[-19,]

#ggpairs(jawdata2[,grep("pre", names(jawdata2))])

#par(mar=c(1,1,1,1))
#mvn(data = jawdata2[,grep("pre", names(jawdata2))], mvnTest = "royston", univariatePlot = "qqplot")
#mvn(data = jawdata2[,grep("pre", names(jawdata2))], mvnTest = "royston", univariatePlot = "histogram")
#par(mfrow=c(1,1))

#mvn(data = jawdata2[,grep("pre", names(jawdata2))],subset = NULL,mvnTest = "royston")

######################################################

#wilcoxon rank sum tests for difference variables
sapply(jawdata[,grep("delta", names(jawdata))], function(x){wilcox.test(x, mu=0, conf.int = TRUE, conf.level = 0.95)$p.value})

#heatmap of correlation between delta variables and facial comparisons
cordat <- rcorr(abs(as.matrix(jawdata[,grep(paste(c("pre", "delta"), collapse = "|"), names(jawdata))])), type = c("spearman"))

cormat2 <- round(cordat$r[1:8,9:12],2)

melted_cormat2 <- melt(cormat2, na.rm = TRUE)

ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman \n Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels=c("Pre Relaxed vs Post Relaxed",
                            "Pre Smile vs Post Smile",
                            "Pre Relaxed vs Post Smile",
                            "Pre Smile vs Post Relaxed"))+
  scale_y_discrete(labels=c("SNA",
                            "SNB",
                            "ANB",
                            "U1-SN",
                            "L1-MP",
                            "Upper Lip",
                            "Lower Lip",
                            "Convexity"))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 20, hjust = 1),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=20))+
  coord_fixed()

#######################################################

#boxplots of response variables

Score <- c(jawdata[,9],
              jawdata[,10],
              jawdata[,11],
              jawdata[,12])
Evaluation <- c(rep("Relaxed: Pre v Post",25),
           rep("Smile: Pre v Post",25),
           rep("Relaxed Pre v Smiling Post",25),
           rep("Smiling Pre v Relaxed Post",25))
dat <- cbind.data.frame(Score,Evaluation)
#boxplots
ggplot(dat, aes(x = Evaluation, y = Score))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot(aes(fill = Evaluation))

#performing 1-way anova
aov1 = aov(Score~Evaluation,data=dat)
summary(aov1)

######################################################

#box plots/histograms for each predictor

compdata <- data.frame(Score = c(jawdata$pre_relaxed_v_post_relaxed,
                                      jawdata$pre_smile_v_post_smile,
                                      jawdata$pre_relaxed_v_post_smile,
                                      jawdata$pre_smile_v_post_relaxed),
                       Evaluation = c(rep("Relaxed: Pre v Post", 25),
                                      rep("Smiling: Pre v Post", 25),
                                      rep("Relaxed Pre v Smiling Post", 25),
                                      rep("Smiling Pre v Relaxed Post", 25)),
                       Jaw = rep(jawdata$jaw_number,4),
                       Gender = rep(jawdata$gender,4),
                       Surgery = rep(jawdata$surgery_type,4),
                       Ethnicity = rep(jawdata$ethnicity,4),
                       Age = rep(jawdata$age,4))

#Jaw
jaw <- ggplot(compdata,aes(x=Jaw,y=Score, fill=Evaluation)) +
    #stat_boxplot(geom = "errorbar")+
    geom_boxplot(position=position_dodge(1))+
    scale_x_discrete(labels=c("Both","Mandibular","Maxillary"))+
    ggtitle("(A)")

#Gender
gend <- ggplot(compdata,aes(x=Gender,y=Score, fill=Evaluation)) +
  #stat_boxplot(geom = "errorbar")+
  geom_boxplot(position=position_dodge(1))+
  scale_x_discrete(labels=c("Female","Male"))+
  ggtitle("(B)")

#Surgery Type
surg <- ggplot(compdata,aes(x=Surgery,y=Score, fill=Evaluation)) +
  #stat_boxplot(geom = "errorbar")+
  geom_boxplot(position=position_dodge(1))+
  scale_x_discrete(labels=c("Both","BSSO","LeFort"))+
  ggtitle("(C)")

#Ethnicity
ethn <- ggplot(compdata,aes(x=Ethnicity,y=Score, fill=Evaluation)) +
  #stat_boxplot(geom = "errorbar")+
  geom_boxplot(position=position_dodge(1))+
  scale_x_discrete(labels=c("African American","White","Other"))+
  ggtitle("(D)")

#Age
age <- ggplot(compdata, aes(x=Age, y=Score, shape=Evaluation, color=Evaluation)) +
  geom_point() + stat_smooth(method="loess", se=FALSE)+ggtitle("(B)")

age2 <- ggplot(compdata, aes(x=Age, y=Score, shape=Evaluation, color=Evaluation)) +
  geom_point() + stat_smooth(method="lm", se=FALSE)+ggtitle("(A)")

ggarrange(jaw, gend, surg, ethn, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggarrange(age2, age, ncol = 2, common.legend = TRUE, legend = "bottom")

#######################################################################



#######################################################################

#Univariate Analyses

##Nonparametric Kruskal-Wallis tests

###Relaxed: Pre vs Post

####Jaw 
kruskal.test(pre_relaxed_v_post_relaxed ~ jaw_number, data = jawdata)
pairwise.wilcox.test(jawdata$pre_relaxed_v_post_relaxed, jawdata$jaw_number,
                     p.adjust.method = "bonf")

####Gender
kruskal.test(pre_relaxed_v_post_relaxed ~ gender, data = jawdata)

####Surgery
kruskal.test(pre_relaxed_v_post_relaxed ~ surgery_type, data = jawdata)
pairwise.wilcox.test(jawdata$pre_relaxed_v_post_relaxed, jawdata$surgery_type,
                     p.adjust.method = "bonf")

####Ethnicity
kruskal.test(pre_relaxed_v_post_relaxed ~ ethnicity, data = jawdata)

#################

###Smiling: Pre vs Post

####Jaw 
kruskal.test(pre_smile_v_post_smile ~ jaw_number, data = jawdata)

####Gender
kruskal.test(pre_smile_v_post_smile ~ gender, data = jawdata)

####Surgery
kruskal.test(pre_smile_v_post_smile ~ surgery_type, data = jawdata)

####Ethnicity
kruskal.test(pre_smile_v_post_smile ~ ethnicity, data = jawdata)

################
###Relaxed Pre v Smiling Post

####Jaw 
kruskal.test(pre_relaxed_v_post_smile ~ jaw_number, data = jawdata)
pairwise.wilcox.test(jawdata$pre_relaxed_v_post_smile, jawdata$jaw_number,
                     p.adjust.method = "bonf")

####Gender
kruskal.test(pre_relaxed_v_post_smile ~ gender, data = jawdata)

####Surgery
kruskal.test(pre_relaxed_v_post_smile ~ surgery_type, data = jawdata)
pairwise.wilcox.test(jawdata$pre_relaxed_v_post_smile, jawdata$surgery_type,
                     p.adjust.method = "bonf")

####Ethnicity
kruskal.test(pre_relaxed_v_post_smile ~ ethnicity, data = jawdata)

####################
###Smiling Pre v Relaxed Post

####Jaw 
kruskal.test(pre_smile_v_post_relaxed ~ jaw_number, data = jawdata)

####Gender
kruskal.test(pre_smile_v_post_relaxed ~ gender, data = jawdata)

####Surgery
kruskal.test(pre_smile_v_post_relaxed ~ surgery_type, data = jawdata)

####Ethnicity
kruskal.test(pre_smile_v_post_relaxed ~ ethnicity, data = jawdata)


