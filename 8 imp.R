# Banking Sector Case Study
# Question: Understand customer spent an repayment behavior along with evaluating areas of corruption, fraud
# and collections. also respond to customer requests for them with proactive offers and service. 
# Perform data pre processing, data manipulation, data modelling, statistical analysis for the  given application

df<-read.csv("D:\\vsc\\SEM4\\SEM6\\data anal\\Datasets\\NSE_BANKING_SECTOR.csv")
head(df)

df<-na.omit(df)
df<-head(df,500)
head(df)

library(dplyr)
summarize(df)

#splitting into training and test data
library(caTools)# required library for data splition
split = sample.split(df$X.DELIVERBLE, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
head(training_set)
head(test_set)

#Data modelling
#install.packages("ReDaMoR")
#library(ReDaMoR)
#m <- model_relational_data()

#install.packages("gapminder")
#install.packages("tidyverse")
library(gapminder)
library(tidyverse)
library(ggplot2)
ggplot(df, aes(TURNOVER,TRADES)) +geom_point()
#Quartile Function over F-Distribution
qf(p=.05, df1=6164, df2=1, lower.tail=FALSE)

#Statistical Analysis
#logistic regression
loan_model <- glm(TRADES ~ ., data = training_set, family = binomial)

#Histogram
hist(df$TRADES)

#Boxplot
boxplot(df$VOLUME)

#t.test
t.test(df$X.DELIVERBLE)

#confidence Interval
t.test(df$TURNOVER,var.equal=TRUE)

#anova
an=aov(df$OPEN ~ df$HIGH)
an

summary(df)