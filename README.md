# Wee7_assignment3
R Script containing code for project

#Loading libraries
library (tidyverse)
library (haven)
library(aod)
library(ggplot2)
library(dplyr)


entire_dataset <- read.csv("DataSet_HR Analytics at Scalene Works.csv")#load the csv data into entire_dataset dataframe

#Get Total values for each category available in the column
table(entire_dataset['Status'])
table(entire_dataset['Joining.Bonus'])
table(entire_dataset['Gender'])
table(entire_dataset['Candidate.Source'])
table(entire_dataset['DOJ.Extended'])

entire_dataset$Joining.Bonus<-ifelse(entire_dataset$Joining.Bonus=="Yes",1,0)
table(entire_dataset['Joining.Bonus'])

entire_dataset$Gender<-ifelse(entire_dataset$Gender=="Male",1,0)
table(entire_dataset['Gender'])

entire_dataset$Candidate.Source<-ifelse(entire_dataset$Candidate.Source=="Agency",1,
                                     ifelse(entire_dataset$Candidate.Source=="Direct",2,3))
table(entire_dataset['Candidate.Source'])

entire_dataset$DOJ.Extended<-ifelse(entire_dataset$DOJ.Extended=="Yes",1,0)
table(entire_dataset['DOJ.Extended'])


entire_dataset$Status <- factor(entire_dataset$Status)
entire_dataset$Joining.Bonus <- factor(entire_dataset$Joining.Bonus)
entire_dataset$Gender <- factor(entire_dataset$Gender)
entire_dataset$Candidate.Source <- factor(entire_dataset$Candidate.Source)
entire_dataset$DOJ.Extended <- factor(entire_dataset$DOJ.Extended)

mylogit <- glm(Status ~ Notice.period+Joining.Bonus+Gender+Candidate.Source+DOJ.Extended+Rex.in.Yrs+Age, data = entire_dataset, family = "binomial")

summary(mylogit)

newdata1 <- with(entire_dataset, data.frame(Notice.period = mean(Notice.period), Rex.in.Yrs = mean(Rex.in.Yrs), Age = mean(Age), Candidate.Source = factor(1:3)))

data1logit <- glm(Status ~ Notice.period+Candidate.Source+Rex.in.Yrs+Age, data = entire_dataset, family = "binomial")

summary(data1logit)

newdata1$Candidate.SourceP <- predict(data1logit, newdata = newdata1, type = "response")

newdata2 <- with(entire_dataset, data.frame(Notice.period = rep(seq(from = 0, to = 120, length.out = 100),
                                              3), Age = mean(Age), Rex.in.Yrs = mean(Rex.in.Yrs), Candidate.Source = factor(rep(1:3, each = 100))))

newdata3 <- cbind(newdata2, predict(data1logit, newdata = newdata2, type = "link", se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

head(newdata3)

ggplot(newdata3, aes(x = Notice.period, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,ymax = UL, fill = Candidate.Source), alpha = 0.2) + geom_line(aes(colour = Candidate.Source),size = 1)

