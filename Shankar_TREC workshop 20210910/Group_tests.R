##################
##GROUP TESTS##################
getwd()
setwd("C:/Users/Shankar Panthi/Desktop/0910")

size <-read.table(file="height_shoesize.csv",header=T,row.names=NULL,sep=",")
head(size)
summary(size)
dim(size)  

#1) check the distribution of the height data using a histogram 

hist(size$Height) #show a histogram of the height data
library(rcompanion)
plotNormalHistogram(size$Height)
logHeight=log(size$Height)
hist(logHeight)
plotNormalHistogram(logHeight)

#2)##1-sample t-test## 
#checking whether humans are on average taller than 150 cm (i.e. fixed mu=150)(1-tailed test)

t.test(size$Height,mu=150,alternative="greater")
dim(size)
#Note the output given by the test (mean, confidence intervals and significance of tests)

########################
###3) 2-sample t-test/Independent sample t-test############
###3.1) checking whether males differ in height from females (2-tailed test) 

t.test(Height~Gender,data=size) #here we ignore the 'alternative' argument and take its default case (a two-tailed test)
##or## t.test(size$Height~size$Gender) 
##t.test(Height~Gender,data=size, alternative="two.sided")

#######################
#3.2) 2-sample t-test checking whether females are shorter than males (1-tailed test)
###########################

#here we require the 'alternative' argument to specify this is a one-tailed test
#we specify the direction of the relationship using the  alternative=c("two.sided", "less", "greater") argument. 

t.test(Height~Gender,data=size, alternative="less")
t.test(Height~Gender,data=size, alternative="greater")
#Note: the t-value does not change but the p-value changes. How does it change?

#######################################
# 3.3) 2-sample t-test checking whether females have smaller shoe-sizes than males
#########################
t.test(Shoe_Size~Gender,data=size, alternative="two.sided")
t.test(Shoe_Size~Gender,data=size, alternative="less")
t.test(Shoe_Size~Gender,data=size, alternative="greater")

##############################################
####################################################
#######Paired-sample t-test#########
#############################################################
sppn <-read.table(file="yam.csv",header=T,row.names=NULL,sep=",")
head(sppn)
summary(size)
dim(sppn)  

#1) check the distribution of the height data using a histogram 
hist(sppn$sppspr) #show a histogram
logsppspr=log(sppn$sppspr)
hist(logsppspr)

hist(sppn$sppwin)
logsppwin=log(sppn$sppwin)
hist(logsppwin)

library(rcompanion) ###adding normal curves to histograms
plotNormalHistogram(sppn$logsppspr)
plotNormalHistogram(sppn$logsppwin)


t.test(sppn$logsppwin,sppn$logsppspr,paired=TRUE, alternative="two.sided") 
t.test(sppn$logsppwin,sppn$logsppspr, alternative="two.sided") #independent sample t-test

###############
#### Non-parametric T-tests#############
#############################################
##WICOXON sign test##########
wilcox.test(sppn$sppwin, sppn$sppspr) # if the two groups are independent

##WICOXON signed-rank test##########
# checking mean test when the distribution is symmetric)
wilcox.test(sppn$sppwin, sppn$sppspr, paired=TRUE) 
#wilcox.test(sppn$sppwin, sppn$sppspr, paired=TRUE, alternative="two.sided") 

#Check one-sided/1-tailed tests 
#wilcox.test(sppn$sppwin, sppn$sppspr, paired=TRUE, alternative="less") 
#wilcox.test(sppn$sppwin, sppn$sppspr, paired=TRUE, alternative="greater") 
#wilcox.test(sppn$sppspr, sppn$sppwin, paired=TRUE, alternative="greater") 

########################

