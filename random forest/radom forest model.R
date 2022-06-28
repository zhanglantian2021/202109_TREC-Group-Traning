install.packages("randomForest")
library(randomForest)
data(mtcars)
##replace NAs with column medians
###method 1
for(i in 1:ncol(mtcars))  
  mtcars[, i][is.na(mtcars[, i])] <- median(mtcars[, i],  
                                            na.rm=TRUE)

mtcars$cyl = as.factor(mtcars$cyl)
mtcars$vs  = as.factor(mtcars$vs)
mtcars$am = as.factor(mtcars$am)
str(mtcars)
###impute missing data
###method 2
set.seed(100)
mtcars.Im=rfImpute(mpg ~ .,data=mtcars)
####find m
names(mtcars)
n=length(names(mtcars))
n
rate=1
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(formula=mpg~.,data=mtcars,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$rsq)   #calculate the mean var explained rate
  print(rf_train)    
}
plot(rate)
#### another method to find mtry
model_tuned <- tuneRF(
  x=mtcars[,-1], #define predictor variables
  y=mtcars$mpg, #define response variable
  ntreeTry=1000,
  mtryStart=3, 
  stepFactor=1.5,
  improve=0.01,
  trace=T, # show real-time progress
  plot=TRUE
)
####find the best ntree
set.seed(100)
model<-randomForest(formula=mpg~.,data=mtcars,mtry=6,ntree=1000)
plot(model)    #figure for error vs. ntree 
which.min(model$mse)
####random forest model
model<-randomForest(formula=mpg~.,data=mtcars,mtry=6,ntree=300)
####importance
importance(model)
varImpPlot(x=model,sort=TRUE,n.var=nrow(rf_train$importance),type=2,main="variable importance")
#####predict
pred = predict(model, data=mtcars)
library(ggplot2)
ggplot()+
  geom_point(aes(x=mtcars$mpg,y=pred))+
  geom_line(aes(x=mtcars$mpg,y=mtcars$mpg),col=2)
####tree size
hist(treesize(model))
