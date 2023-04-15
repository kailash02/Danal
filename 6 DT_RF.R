#### Implemtation of Decision Tree and Random Forest

################ Decision Tree #############################
#install.packages("caTools")
library(caTools)
df=read.csv("D:\\vsc\\SEM4\\SEM6\\data anal\\Datasets\\bill_authentication.csv")

#Splitting the dataset into test and train
sample_data = sample.split(df, SplitRatio = 0.7)
tr_data <- subset(df, sample_data == TRUE)
te_data <- subset(df, sample_data == FALSE)

#install.packages("party")
library(party)
model<- ctree(Class~ ., tr_data)
plot(model)
model
predictions<-predict(model,newdata=te_data)
accuracy<-mean(predictions==te_data$Class)
accuracy

################ Random Forest #############################
#install.packages("randomForest")

library(randomForest)

#Splitting the dataset into test and train
sample_data = sample.split(df, SplitRatio = 0.7)
tr_data <- subset(df, sample_data == TRUE)
te_data <- subset(df, sample_data == FALSE)

classifier_RF = randomForest(x = tr_data,y = tr_data$Class)
classifier_RF
plot(classifier_RF)  #plotting the model
importance(classifier_RF) #importance Plot

#Predicting the test set results
y_pred = predict(classifier_RF, newdata = te_data)
y_pred

#Accuracy
accuracy<-mean(y_pred==te_data$Class)
accuracy
