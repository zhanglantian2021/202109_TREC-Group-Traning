#Analysis of variance( ANOVA)###

getwd()
setwd("C:/Users/Shankar Panthi/Desktop/0910")#set working directory

diver <-read.table(file="diver.csv",header=T,row.names=NULL,sep=",")
head(diver)
#View(diver)# check type of the data columns

summary(diver)#check basic stats 
# we can see that both "experience" and "respiration" are being identified as continuous variates (because the summary stats give means for both of them)
# we will use the data like this for regression


#plot distributions of variables
hist(diver$experience) #equal sampling from all groups hence the weird shape
hist(diver$respiration) #data is coarsely normal

#library(rcompanion)
plotNormalHistogram(diver$respiration)

plot(diver$experience,diver$respiration)#scatter plot

diver_lm<-lm(respiration ~ experience, data=diver)#Fit a regression slope to interpret the results
anova(diver_lm)
summary(diver_lm)


#check residuals
par(mfrow=c(2,2)) #this line splits the display panel into four parts
plot(diver_lm)
###
lines(diver$experience, diver_lm $fitted)  
diver_lm

#######################
######proceed Group comparison#########
#################################
# 1) Perform an ANOVA using the aov() command and interpret the results 

# with as.factor()
diver_anova1<-aov(respiration ~ as.factor(experience), data= diver)
anova(diver_anova1)
summary(diver_anova1)

#quartz()
windows()
plot(as.factor(diver$experience),diver$respiration)

# without as.factor() 
diver_anova2<-aov(respiration ~ experience, data= diver)
anova(diver_anova2)
summary(diver_anova2)
plot(diver$experience,diver$respiration)

# 2) Perform an ANOVA using the lm()and inspect the results. 

diver_anova3<-lm(respiration ~ as.factor(experience), data=diver)
anova(diver_anova3)
summary(diver_anova3)

# 3) Interpret the model coefficients to predict the group means. Check your calculations using:
#tapply(response, factor, mean)

tapply(diver$respiration, as.factor(diver$experience), mean)

##########################################

#########################
####TWO-WAY ANOVA#################
##################################
yam <-read.table(file="yam.csv",header=T,row.names=NULL,sep=",")
head(yam)
View(yam)# check type of the data columns

summary(yam)#check basic stats 

#library(dplyr)
#find mean and standard deviation spp richness for each treatment group
#yam %>%
#  group_by(alt, LC) %>%
#  summarise(mean = mean(sppspr),
#            sd = sd(sppspr))
#View(yam)

#plot distributions of variables
hist(yam$alt) #nearly equal sampling from all groups hence the weird shape
hist(yam$logsppspr) #data is normal

library(rcompanion)
plotNormalHistogram(yam$logsppspr)

#plot distributions of variables
hist(yam$alt) #nearly equal sampling from all groups hence the weird shape
hist(yam$logsppspr) #data is normal

spr_anova1 <- aov(logsppspr ~ as.factor(alt) * LC, data = yam)
anova(spr_anova1)
summary(spr_anova1) #view the model output

## Independency: Observations in each groups are independent to each other 
#Check normality#
#define model residuals
resid <-spr_anova1$residuals

#create histogram of residuals
hist(resid, main = "Histogram of Residuals", xlab = "Residuals", col = "steelblue")
plotNormalHistogram(resid)

#####################
#Check equality of variances##
library(car)

#conduct Levene's Test for equality of variances
leveneTest(logsppspr ~ as.factor(alt)  * LC, data = yam)

#Since the p-value >0.05, we can assume that our assumption of equality of variances among groups is met.

#perform Tukey's Test for multiple comparisons
TukeyHSD(spr_anova1, conf.level=.95) 

#set axis margins so labels don't get cut off
par(mar=c(4.1, 13, 4.1, 2.1))

#create confidence interval for each comparison
plot(TukeyHSD(spr_anova1, conf.level=.95), las = 2)
#############################################################
