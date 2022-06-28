##Practical 1: Linear regression models

#setwd("/Users/kyletomlinson/Dropbox/Teaching/XTBG advanced stats 2021/Lectures/Lesson 1 Linear models")

getwd()


#Example 1.1: Comparing a t-test with a linear model for groups

#READ IN THE DATA
# import the data "fishspeed.csv" into R and attach the data
fishspeed <-read.table(file="fishspeed.csv",header=T,row.names=NULL,sep=",")
head(fishspeed)
summary(fishspeed)
dim(fishspeed)

fishspeed$Species <- as.factor(fishspeed$Species)
summary(fishspeed)

summary(lm(Speed~Temperature,data= fishspeed))
summary(lm(Speed~Species,data= fishspeed))
t.test(Speed~Species,data= fishspeed,var.equal = TRUE)


#####################################################
############### RETURN TO LECTURE ###################
#####################################################

#Example 1.2: multi-model with factor and variate predictors
 
#A researcher measures the maximum swimming speed of 10 brown trout and 10 Canterbury galaxias at a range of temperatures: 
#Response (Y) = swimming speed
#Factor = species 
#Variate (X1) = temperature
#Dummy variable (D) [not necessary to use, but illustrating how the data is constructed]


#plot the data with separate colours for each fish type
plot(fishspeed $Temperature, fishspeed $Speed,col= c("red", "blue")[as.numeric(fishspeed $Species)])

#certainly looks like they behave differently
#test this using the linear model

lm1 <- lm(Speed~Temperature*Species,fishspeed)
summary(lm1)
#everything significant; most important result is interaction is significant.
#two categories = 2 lines
#please write out the equations for each species!


#check anova as well
anova(lm1)

# plot the line of fitted values
plot(fishspeed $Temperature,lm1$fitted,ylab="Fish speed",xlab="Temperature")
# add the scatter plot points
points(fishspeed $Temperature,fishspeed $Speed,pch=3,col="blue")



#####################################################
############### RETURN TO LECTURE ###################
#####################################################

#Exercise 1.3: Problem with three factor levels
#Here we have the same problem as before but now we need to compare the swimming speed of 3 fish species: trout, galaxias, tenebrias

#Please do the following
#1. read in the data (fishspeed2.csv)
#2. plot the data for the 3 species
#3. write out the full linear model with dummies into your solution file 
#4. use lm() to evaluate the model: 
#check significance levels and write out the linear models for each species
#Please note!!!! you need to run lm() twice with different default levels of the fish. so please look up the function relevel(), which you will need to change the default level

#SMALL CLUE: EVERYTHING GETS COMPARED TO THE DEFAULT CASE!!!

fishspeed2 <-read.table(file="fishspeed2.csv",header=T,row.names=NULL,sep=",")
summary(fishspeed2)
fishspeed2$Species <- as.factor(fishspeed2$Species)
lm1 <- lm(Speed~Temperature*Species,fishspeed2)
summary(lm1)



#S = b1 + b2*T + b3[Ten] + b4[Tro] + b5*T[Ten] + b6*T[Tro]

#G: S = b1 + b2*T
#Ten: S = b1 + b2*T + b3[Ten]  + b5*T[Ten] 
#Tro: S = b1 + b2*T + b4[Tro]  + b6*T[Tro]

# change the default species
fishspeed2$Species <- relevel(fishspeed2$Species,ref="Tenebrias")
lm2 <- lm(Speed~Temperature*Species,fishspeed2)
summary(lm2)




#####################################################
############### RETURN TO LECTURE ###################
#####################################################

# Example 1.4: checking diagnostics on lm() objects

#use the model you built with fishspeed2 in EXERCISE 1E

fishspeed2 <-read.table(file="fishspeed2.csv",header=T,row.names=NULL,sep=",")
summary(fishspeed2)
fishspeed$Species <- as.factor(fishspeed$Species )

lm3 <- lm(Speed~Temperature*Species,fishspeed2)
summary(lm3)

#running diagnostics is simple
#first partition

par(mfrow=c(2,2))
plot(lm3)


## SOME FORMAL TESTS FOR THE RESIDUALS

library(car)
###Assumptions###
#Normality
resids <- resid(lm3, type='pearson')

## qq plot with 95% CIs
par(mfrow=c(1,1))
qqPlot(resids)

#Shapiro test to check for non-normality (a significant test result means non-normal)
shapiro.test(residuals(lm3))
#not significant so residuals are normal

#Homoscedasticity
plot(sqrt(abs(resids))~lm3$fitted); abline(a = 1.96, b = 0, col = 2)

#Levene test for homogeneity of variance across groups
leveneTest(residuals(lm3), as.factor(fishspeed2$Species))
#not significant, so groups variances are not significantly different
