ls(); rm(list=ls())
getwd()
setwd("F:/XTBG/club meeting/R_Workshop_202109/1. R Basic Tidyverse")
#install.packages("tidyverse")
library(tidyverse)
##tidyverse: A powerful collection of R packages for preparing, wrangling and visualizing data
##tidyverse include the following packaes: dplyr, tidyr, stringr, forcats, tibble, readr, purr, ggplot, readr
##first read your data with readr
#The readr package solves the problem of parsing a flat file into a tibble
##The Salaries data set:  It describes the 9 month academic salaries of 397 college professors at a single institution in 2008-2009
##Read .csv
Salaries <- read.csv("salaries.csv")
class(Salaries) ## data format
names(Salaries) ## variable names
str(Salaries)  ##loot at the data structure
head(Salaries) ## first 6 rows of variables
tail(Salaries) ## last 6 rows of variables
##Read excel
library(readxl)
Salaries <- read_excel("salaries.xlsx", sheet=1)
glimpse(Salaries)  ## same as str(), show more data
View(Salaries)
print(Salaries)

ggplot(Salaries) +
  geom_boxplot(aes(x=rank, y=salary))
ggplot(Salaries) + 
  geom_boxplot(aes(x=discipline, y=salary))
ggplot(Salaries) +
  geom_boxplot(aes(x=sex, y=salary))
ggplot(Salaries) +
  geom_point(aes(x=yrs.since.phd, y=salary, color=rank))
ggplot(Salaries) +
  geom_point(aes(x=yrs.service, y=salary, color=rank))

ggplot(Salaries) +
  geom_boxplot(aes(x=sex, y=salary, fill=sex))+
  facet_grid(discipline~rank, scale="free")

###### Select variables
newdata <- select(Salaries, rank, sex, salary)
head(newdata)

newdata <- select(Salaries, rank:sex)
head(newdata)

newdata <- select(Salaries, rank, sex, everything())
newdata
###### Select observations
newdata <- filter(Salaries, rank == "Prof")
newdata
newdata <- filter(Salaries, sex != "Male")
newdata
newdata <- filter(Salaries, rank == "Prof" & sex!="Male")
newdata

###### order the rows data descre
newdata <- arrange(Salaries, salary)
newdata
###### Creating new variables
newdata <- mutate(Salaries, salary_10 = salary * 10)
newdata

newdata <- mutate(Salaries, salary_levels = ifelse(salary > 100000, "High", "Low"))
newdata

newdata <- mutate(Salaries, salary_levels = ifelse(salary < 100000, "NA", salary))
newdata

###### Summarizing data
newdata <- summarize(Salaries, 
                     mean_salary = mean(salary, na.rm=T),
                     max_salary = max(salary, na.rm=T))
newdata
######pipe, %>%: X %> f(y) means that x is ??piped?? in to the function f(x, y)
######The %>% operator passes the result on the left to the first parameter of the function on the right.
Salaries %>% head(n=10)
Salaries %>% names() 

Salaries %>%
  filter(rank == "Prof")

Salaries %>%
  select(rank, sex, salary)

Salaries %>%
  mutate(salary_10 = salary * 10)

Salaries %>%
  summarize(mean_salary = mean(salary, na.rm=T))

Salaries %>%
  filter(rank == "Prof") %>%
  select(rank, sex, salary) %>%
  mutate(salary_10 = salary * 10) %>%
  summarize(mean_salary = mean(salary, na.rm=T))

###### Grouping 
newdata <- Salaries %>%
  group_by(rank) %>%
  summarize(mean_salary = mean(salary, na.rm=T),
            max_salary = max(salary, na.rm=T))
newdata
###### Mutation and summarize using group_by
newdata <- Salaries %>%
  group_by(rank, sex) %>%
  summarize(mean_salary = mean(salary, na.rm=T),
            max_salary = max(salary, na.rm=T))
newdata

newdata %>%
  ggplot()+
  geom_col(aes(x=rank, y=mean_salary, fill=sex), position = "dodge2")
######Data cleaning
#####gather: combine multiple columns into one
glimpse(Salaries)
##### combine columns in to one
Salaries %>%
  mutate(rank_discipline = paste(rank, discipline, sep="_"))

newdata <- Salaries %>%
  unite("rank_discipline", c(rank, discipline))
newdata
##### separate one column to multiple columns
newdata %>%
  separate("rank_discipline", c("rank", "discipline"))
#####
##### gather multiple variabls
long_data <- Salaries %>%
  mutate(id = 1:dim(Salaries)[1]) %>%
  select(id, rank,  discipline, sex, yrs.since.phd, yrs.service, salary) %>%
  gather(key="Variable", value="value", c(yrs.since.phd, yrs.service, salary))

long_data
#### spread variable into multiple ones
wide_data <- long_data %>%
   spread(Variable, value)
wide_data

####join: join tow tbls together

####inner_join: An inner join matches pairs of observations whenever their keys are equal
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
x; y
Z <- x %>% 
  full_join(y, by = "key")
Z
####mutate: add new columns that are functions of existing columns
####pipe, %>%: 
####mutate: ade new variables
Data <- Data %>%
  mutate(mmse_10 = mmse * 10,
         mmse_100 = mmse * 100)
names(Data)
####transmutate: add new variables and drops existing one
Data_new <- Data %>%
  transmute(mmse_10 = mmse * 10)
names(Data_new)
#####group_by: Group different observations together such that the original dataset does not change
Data_Group <- group_by(Data, health_status, sex)
summarise(Data_Group, AVE = mean(mmse, na.rm=T))
Data_Group <- ungroup(Data_Group)
summarise(Data_Group, mean(mmse))
#####summarse: summarise all functions
summarise(Data_Group,
          mean(mmse))
Data_Group <- group_by(Data, sex, health_status)
summarise(Data_Group,
          mean(mmse))
#####compute by group metrics
Data_scale <- Data %>%
  group_by(sex, health_status) %>%
  mutate(
    mmse_scale = (mmse - mean(mmse))/sd(mmse)
  )
glimpse(Data_scale)
#######purr
glimpse(Data)
ggplot(Data)+
  geom_point(aes(x=age, y=mmse, color=health_status))+
  geom_smooth(aes(x=age, y=mmse, color=health_status), method="lm")

models <- Data %>% 
  split(.$health_status) %>%
  map(~summary(lm(mmse ~ age, data = .))) %>% 
  map(summary) %>% 
  map(coef)
models
############
Data_group <- Data %>%
  group_by(health_status) %>%
  nest()
names(Data_group)
mod_fun <- function(data){
  lm(mmse ~ age, data)
}

model <- Data_group %>%
  mutate(model = map(data, mod_fun)) 
model

model_coef <- model$model %>%
  map(summary) %>%
  map(coef) 
model_coef
#####All analysis in one step
glimpse(Data)
Data_NEW <- Data %>%  #### pipe to Data_New
  group_by(sex, health_status) %>% ###group variables by sex and health-status
  filter(age >= 80) %>% ###filter data for age more than 80 years
  mutate(mmse_1000 = mmse * 1000) %>%  ###add new variable of mmse_10 equal to mmse *10
  select(age:health_status, mmse_1000) %>%  ###select subset of variables
  summarise(mmse_1000_mean = mean(mmse_1000, na.rm=T)) %>%  ###summarise the mean of mmse_10 according to groups (age, health_status)
  ggplot() + geom_point(aes(x=sex, y=health_status, size=mmse_1000_mean)) ###plot the mmse_10_mean, with x=sex, and y=health_status, with Data_NEW
Data_NEW
##### more professional plot
glimpse(Data)
g1 <- ggplot(Data, aes(x = drug_treatment, y = mmse, fill=drug_treatment)) +
  geom_boxplot(color = "black", width = 0.7) +
  facet_grid(sex~health_status, scale="free") +
  scale_y_continuous(breaks=10) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "green", "purple")) +
  theme(legend.position = "NULL",
        legend.title = element_blank(),
        axis.title = element_text(size = 36),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=32),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 22)) +
  labs(x = "\nDrug Treatment", 
       y = "Cognitive Function (MMSE)\n",
       caption = "\nFigure 1. Effect of novel drug treatment AD-x37 on cognitive function in healthy and demented elderly adults. \nn = 100/treatment group (total n = 600)")
g1
ggsave(plot=g1, "Tidyverse_test.jpg", height=10, width=10, dpi=150)
#############
glimpse(Data)
Data %>%
  mutate(sex=factor(sex, level=c(0,1))) %>%
  group_by(sex) %>%
  ggplot() + geom_point(aes(x=age, y=mmse, color=drug_treatment))
