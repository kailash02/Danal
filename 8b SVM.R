# SVM Implementation
# 1. Sigmoid   2. Radial   3. Polynomial
df=read.csv("D:\\vsc\\SEM4\\SEM6\\data anal\\Datasets\\UniversalBank.csv")
head(df)

# Preprocessing
df <- subset(df, select = -c(1,5))
df$Education <- as.factor(df$Education)
df$Personal.Loan <- as.factor(df$Personal.Loan)
df$Securities.Account <- as.factor(df$Securities.Account)
df$CD.Account <- as.factor(df$CD.Account)
df$Online <- as.factor(df$Online)
df$CreditCard <- as.factor(df$CreditCard)
df$Personal.Loan <- as.factor(df$Personal.Loan)
head(df)

# Splitting the data into train and test

library(caret)
set.seed(123)
tr_r<-caret::createDataPartition(df$Personal.Loan,p=0.7,list = F)
tr<-df[tr_r,]
tst<-df[-tr_r,]

# SVM classifier
dummies <- caret::dummyVars(Personal.Loan~.,data=df)
x.train= predict(dummies, newdata = tr)
y.train= tr$Personal.Loan
x.test = predict(dummies, newdata = tst)
y.test = tst$Personal.Loan

# install.packages("e1071")
library(e1071)

# Building the model on train data
### Kernel: Linear
#model  =  svm(x = x.train, y = y.train, type = "C-classification", kernel = "linear")
#summary(model)
# Predict on test using the model
#pred_test<-predict(model,x.test)
# Build Confusion matrix
#confusionMatrix(pred_test,y.test)

#######  Radial
model_rad = svm(x.train,y.train, method = "C-classification", kernel = "radial")
#summary(model_rad)
# Predict on test using the model
pred_test1<-predict(model_rad,x.test)
# Build Confusion matrix
confusionMatrix(pred_test1,y.test)

#######  Sigmoid
model_sig = svm(x.train,y.train, method = "C-classification", kernel = "sigmoid")
#summary(model_sig)
# Predict on test using the model
pred_test1<-predict(model_sig,x.test)
# Build Confusion matrix
confusionMatrix(pred_test1,y.test)

#######  Polynomial
model_poly = svm(x.train,y.train, method = "C-classification", kernel = "polynomial")
#summary(model_poly)
# Predict on test using the model
pred_test1<-predict(model_poly,x.test)
# Build Confusion matrix
confusionMatrix(pred_test1,y.test)

