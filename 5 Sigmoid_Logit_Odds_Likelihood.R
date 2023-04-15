df=read.csv("D:\\vsc\\SEM4\\SEM6\\data anal\\Datasets\\heart.csv")
head(df)

i<-df[,c("age","chol")]
lr=glm(formula = df$target~df$chol + df$age , data = i, family = binomial)
summary(lr)

plot(age ~ chol, data=i, col="steelblue")

predicts=predict(lr,df,type="response")

###### Sigmoid Function
install.packages("sigmoid")
library(sigmoid)
plot(sigmoid(predicts))


######  Logit Function
plot(logit(predicts))


######  Odds Function
lr=glm(formula = df$target~df$chol + df$age , data = i, family = binomial(link="logit"))
predicts=predict(lr,df,type="response")
plot(predicts)


######  Likelihood Function
plot(logLik(predicts))
