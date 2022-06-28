# PRACTICAL 2: GENERALIZED LINEAR MODELS


#in this practical you will learn how to conduct generalised linear models in R

#setwd("/Users/kyletomlinson/Dropbox/Teaching/XTBG advanced stats 2021/Lectures/Lesson 2 Generalised linear models")

# PLEASE NOTE:
# The main functions we use here are found in the basic package of R installed on your computer
# That said, for a number of things we shall use additional packages that you will need to install. These are mentioned as they arise.
install.packages("arm")

library(ggplot2)
library(arm)

## EXAMPLE 2.1. LOGISTIC REGRESSION WITH BINOMIAL DATA 1: THE SEAL RESPONSE DATA
# A behavioural ecologist  approached 50 nursing seals several times over a 2 day period 
#and recorded the number of aggressive responses. She wanted to check if the age of cubs influences the probability of an aggressive response.

#upload the sealData1.csv file
#and look at its structure

seal <- read.csv('sealData1.csv', h=T)
summary(seal)
head(seal)
dim(seal)


#IMPORTANT: the present data  shows the total number of experimented for a given pupage in 'n.obs' and reports the PROPORTION of "successes" in 'n.response' , so we are not looking at the raw 0-1 data that was collected, but the actual sums of those individual "experiments" categorised by age. This is not a problem in R. As long as we know the total number of experiments ('n.obs'), and the number of successes ('n.response'), R will will be able to make the calculation, as you will see below.


#Now let's plot the proportion of successes against age

plot(n.response/n.obs~pupage,data=seal)

#here we can see the data is bound between 0 and 1 and appears to decline as pupge increases


#so lets run the generalised linear model to test whether age affects aggression
#for generalised linear models we use the glm() function
#we must specify:
#1. the regression relationship we are interested in (cbind(n.response, n.obs-n.response)~pupage)
#(cbind allows us to use the totals in the analysis. Essentially we are binding the number of successes ("n.response") with the number of failures ("n.obs-n.response") in each age category

#2. the residual distribution of the response (family=binomial)
#3. the source of the data for the analysis (seal)

#we can also specify the link function (logit), but this is not necessary
#because the glm will choose the canonical (standard) functions associated with the distribution
#of the response
#compare the two statements below; they should produce the same results

## fit the binomial model from the first day
mod.seal <- glm(cbind(n.response, n.obs-n.response)~pupage,
                data=seal,family=binomial(link=logit))
summary(mod.seal)            

mod.seal1 <- glm(cbind(n.response, n.obs-n.response)~pupage,
                 data=seal,family=binomial)
summary(mod.seal1)



#####################################################
############### RETURN TO LECTURE ###################
#####################################################

#PREDICTION

mod.seal <- glm(cbind(n.response, n.obs-n.response)~pupage,
                data=seal,family=binomial(link=logit))
summary(mod.seal)  


#now we will make a output figures using the derived linear generalised linear model

summary(seal)

## build new data frame to house predictions
##it should cover the same range as the values you have recorded in oberevations
newdata <- data.frame(pupage=1:30)
newdata
summary(mod.seal)
## make the predictions - notice the type='link' argument
preds <- predict(mod.seal, newdata=newdata,type= 'link', se.fit=T)

preds #the command produces a fitted value with standard error for each predictor value in newdata
summary(seal)
dim(seal)
## add CI predictions to new data frame in the transformed range 
# as n = 50 for original data, using z=1.96 to get the 95% CIs is reasonable

newdata$fit_T <- preds$fit
newdata$upr_T <- preds$fit+preds$se.fit*1.96
newdata$lwr_T <- preds$fit-preds$se.fit*1.96

## plot with prediction in the transformed range
plot(newdata$pupage,newdata$fit_T,type="l")
lines(newdata$pupage,newdata$upr_T,lty=2)
lines(newdata$pupage,newdata$lwr_T,lty=2)


#back-transform them with plogis to get response range
?plogis
head(newdata)
newdata$fit <- plogis(preds$fit)
newdata$upr <- plogis(preds$fit+preds$se.fit*1.96) 
newdata$lwr <- plogis(preds$fit-preds$se.fit*1.96)


head(newdata) #note the extra columns in the dataframe
str(newdata)


## plot raw data and model prediction in the response range
#for this we will use ggplot
ggplot(data=seal, aes(x=pupage, y=n.response/n.obs)) + geom_point()+
geom_smooth(data=newdata,aes(x=pupage, y=fit, ymin=lwr, ymax=upr),
              stat='identity')


#####################################################
############### RETURN TO LECTURE ###################
#####################################################

#INFERENCE

#returning to the seal problem, lets check whether the tested model explains a significant amount of deviance

## fit the tested model: aggression ~ pupage           
mod.seal1 <- glm(cbind(n.response, n.obs-n.response)~pupage, data=seal,family=binomial)
summary(mod.seal1)
# residual and null deviance estimates provided at the bottom for tested and null models respectively
logLik(mod.seal1)

       
       
## fit the null model: aggression ~ 1
mod.seal0 <- glm(cbind(n.response, n.obs-n.response)~1, data=seal,family=binomial)
summary(mod.seal0)
# residual and null deviances are identical
logLik(mod.seal0)


#how to get a saturated model? A saturated model has one parameter for each data point
#we can make a dummy to carry this
seal $x1 <- as.factor(1:nrow(seal))
summary(seal)
mod.sealS <- glm(cbind(n.response, n.obs-n.response)~x1, data=seal,family=binomial)
summary(mod.sealS)
#now you can see the residual deviance for the saturated model is basically zero
logLik(mod.sealS)

#so residual deviance for tested model is:
2*(logLik(mod.sealS)-logLik(mod.seal1))

#so residual deviance for null model is:
2*(logLik(mod.sealS)-logLik(mod.seal0))

#compare with output from summary(mod.seal1)
summary(mod.seal1)

#Now compare the residual and null deviances (ask whether there has been a significant reduction in the deviance caused by adding in the predictor pupage)
anova(mod.seal1,test='Chisq')
#the result shows that there is a very significant effect of age on agression


#if you want more detail, look at this explanation:
# https://stats.stackexchange.com/questions/316763/log-likelihood-function-in-poisson-regression
# https://en.wikipedia.org/wiki/Logistic_regression


##Please note: typically you would do this evaluation step before moving to prediction. However I introduce it the other way around because in that way you can see how the glm model prediction relates to the raw data. 
##########Over distribution
#check for overdispersion formally
chisq <- sum(resid(mod.seal1, type='pearson')^2)
chisq/df.residual(mod.seal1) ## much greater than 1

## significantly so?
1-pchisq(chisq, df.residual(mod.seal1)) ## non-significant


####################################################
############### RETURN TO LECTURE ###################
#####################################################
data <- read.csv("Toona data.csv")
computeCriticalDates(data)
head(data)
summary(data)
data$Tree <- as.factor(data$Tree )
data$RF <- as.factor(data$RF )
data$EZ

BDF$E <- ifelse(BDF$EZ > 0.5, 1, 0)
BDF$W <- ifelse(BDF$WZ > 0.5, 1, 0)
BDF$M <- ifelse(BDF$MZ > 0.5, 1, 0)

EZ_phase <- data%>%select(DY,EZ)%>%filter(DY<346)
EZ_phase <- na.exclude(EZ_phase)
EZ_phase$presence <- ifelse(EZ_phase$EZ>0,1,0)
head(EZ_phase)

mylogit <- glm(presence ~ DY, data = EZ_phase, family = "binomial")
summary(mylogit)

mod.qp <- glm(presence ~ DY, data = EZ_phase, family=quasipoisson(link=log))


##########model inference
## fit the tested model
# residual and null deviance estimates provided at the bottom for tested and null models respectively
logLik(mylogit)



## fit the null model: aggression ~ 1
mylogit0 <- glm(presence ~ 1, data = EZ_phase, family = "binomial")
summary(mylogit0)
# residual and null deviances are identical
logLik(mylogit0)


#how to get a saturated model? A saturated model has one parameter for each data point
#we can make a dummy to carry this
EZ_phase $x1 <- as.factor(1:nrow(EZ_phase))
summary(EZ_phase)
mylogitS <- glm(presence ~ x1, data = EZ_phase, family = "binomial")
summary(mylogitS)
#now you can see the residual deviance for the saturated model is basically zero
logLik(mylogitS)

#so residual deviance for tested model is:
2*(logLik(mylogitS)-logLik(mylogit))

anova(mylogit,test='Chisq')

#now check the residual diagnostics

par(mfrow=c(2,2)); plot(mylogit)
# perform an overdispersion test (recall we compare residual chi-sq to the residual degrees of freedom)
chisq <- sum(resid(mylogit, type='pearson')^2)
chisq/df.residual(mylogit) ## close to 1; no overdispersion problem
library(arm)
?binnedplot
x <- predict(mylogit)
y <- resid(mylogit)
binnedplot(x,y)
############################cambial activity

data <- read.csv("Toona data.csv")
computeCriticalDates(data)
head(data)
summary(data)
data$Tree <- as.factor(data$Tree )
data$RF <- as.factor(data$RF )
data$EZ


EZ_phase <- data%>%select(DY,EZ)%>%filter(DY<346)
EZ_phase <- na.exclude(EZ_phase)
EZ_phase$presence <- ifelse(EZ_phase$EZ>0,1,0)
head(EZ_phase)

mylogit <- glm(presence ~ DY, data = EZ_phase, family = "binomial")
summary(mylogit)

mod.qp <- glm(presence ~ DY, data = EZ_phase, family=quasipoisson(link=log))


##########model inference
## fit the tested model
# residual and null deviance estimates provided at the bottom for tested and null models respectively
logLik(mylogit)

## fit the null model: aggression ~ 1
mylogit0 <- glm(presence ~ 1, data = EZ_phase, family = "binomial")
summary(mylogit0)
# residual and null deviances are identical
logLik(mylogit0)


#how to get a saturated model? A saturated model has one parameter for each data point
#we can make a dummy to carry this
EZ_phase $x1 <- as.factor(1:nrow(EZ_phase))
summary(EZ_phase)
mylogitS <- glm(presence ~ x1, data = EZ_phase, family = "binomial")
summary(mylogitS)
#now you can see the residual deviance for the saturated model is basically zero
logLik(mylogitS)

#so residual deviance for tested model is:
2*(logLik(mylogitS)-logLik(mylogit))

anova(mylogit,test='Chisq')

#now check the residual diagnostics

par(mfrow=c(2,2)); plot(mylogit)
# perform an overdispersion test (recall we compare residual chi-sq to the residual degrees of freedom)
chisq <- sum(resid(mylogit, type='pearson')^2)
chisq/df.residual(mylogit) ## close to 1; no overdispersion problem

?binnedplot
x <- predict(mylogit)
y <- resid(mylogit)
binnedplot(x,y)

####################predict
##it should cover the same range as the values you have recorded in oberevations
summary(EZ_phase)
newdata <- data.frame(DY=39:318)
newdata

summary(mylogit)
## make the predictions - notice the type='link' argument
preds <- predict(mylogit, newdata=newdata,type= 'link', se.fit=T)
preds #the command produces a fitted value with standard error for each predictor value in newdata
summary(EZ_phase)
dim(EZ_phase)
newdata$fit_T <- preds$fit
newdata$upr_T <- preds$fit+preds$se.fit*1.96
newdata$lwr_T <- preds$fit-preds$se.fit*1.96

## plot with prediction in the transformed range
plot(newdata$DY,newdata$fit_T,type="l")
lines(newdata$DY,newdata$upr_T,lty=2)
lines(newdata$DY,newdata$lwr_T,lty=2)


head(newdata)
newdata$fit <- plogis(preds$fit)
newdata$upr <- plogis(preds$fit+preds$se.fit*1.96) 
newdata$lwr <- plogis(preds$fit-preds$se.fit*1.96)

head(newdata) #note the extra columns in the dataframe
str(newdata)

## plot raw data and model prediction in the response range
#for this we will use ggplot
ggplot(data=EZ_phase, aes(x=DY, y=presence)) + geom_point()+
  geom_smooth(data=newdata,aes(x=DY, y=fit, ymin=lwr, ymax=upr),
              stat='identity')


