install.packages("lavaan")
# Set working directory and load data

dat <- read.csv("Lavaan_basic.csv")

#view data
head(dat)

summary(dat)

pairs(dat)

##load lavaan

library(lavaan)


# Step 1: Specify model
mod.1 <- "y1 ~ x1
y2 ~ x1
y3 ~ y1 + y2"



# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=dat)

# Check variances
varTable(mod.1.fit)

## adjust data in roughly same scale
x1 <- dat$x1/100
y1 <- dat$y1/100
y2 <- dat$y2
y3 <- dat$y3/100
plot(y1)
### Create Transformed Dataset
##adjust data in same scale

t.dat <- data.frame(x1, y1, y2, y3)


#check data again
summary(t.dat)


# repeat Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=t.dat)

# Step 3: Extract results
summary(mod.1.fit) 







### Request Modification Indices
summary(mod.1.fit, modindices=TRUE)


### MODEL modification  
#Step 1: Specify model
mod.2 <- 'y1 ~ x1
y2 ~ x1
y3 ~ y1 + y2 + x1'

# Step 2: Estimate model
mod.2.fit <- sem(mod.2, data=t.dat)

# Step 3: Extract results
summary(mod.2.fit)     # standard stuff

##### model fiting evaluation measures

summary(mod.2.fit, fit.measures=T)

fitMeasures(mod.2.fit)

fitMeasures(mod.2.fit, c("cfi", "rmsea", "srmr"))
fitMeasures(mod.1.fit, c("cfi", "rmsea", "srmr"))

# Compare models 1 and 2
anova(mod.1.fit, mod.2.fit)



#Standardized Coefficients2 r square in summary
summary(mod.2.fit,standardized=T, rsq=T)
summary(mod.2.fit, modindices=TRUE)

#calculate direct2indirect effects


mod.3 <- 'y1 ~ b*x1
          y2 ~ a*x1
          y3 ~ c*y1 + d*y2 + e*x1

          # define quantities 
          direct   := e+c+d 
          indirect := (a*d)+(b*c)
          total    := e+c+d+(a*d)+(b*c)'
mod.3.fit <- sem(mod.3, data=t.dat)
summary(mod.3.fit, stand=T, rsq=T)

#save the results
sink(file="sem_basic_output.txt")
"model fitting results"
"    "
"mod.1 summary"
"    "
summary(mod.1.fit,standardized=T, rsq=T);
"    "
"    "
"mod.2 summary"
"    "

summary(mod.2.fit,standardized=T, rsq=T)
"    "
"    "
"mod.3 summary for direct and total effects"
"    "
summary(mod.3.fit,standardized=T, rsq=T)
"    "
"    "
"compare mod.1 and mod.2"
anova(mod.1.fit, mod.2.fit)
"    "
"    "
"fit measure mod.2"
"    "
fitMeasures(mod.2.fit, c("cfi", "rmsea", "srmr"))
"    "
"    "
"fit measure mod.1"
"    "
fitMeasures(mod.1.fit, c("cfi", "rmsea", "srmr"))

sink()


