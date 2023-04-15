################################   Problem 1

##### Performing Multiple Linear Regression on mtcars dataset
data("mtcars")
i=mtcars[,c("mpg","disp","hp","wt")]
head(i)

#Plotting 3D Scatterplot
install.packages("scatterplot3d")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg,main="3D Scatterplot")

###### 20BRS1208 ########
#Applying a Regression Model
fit=lm(mpg~wt+disp)
fit
s3d = scatterplot3d(wt,disp,mpg,main="3D scatterplot")
s3d$plane3d(fit)

#########################  QUESTION 2 - Testing of Hypothesis

#In a random sample of 6 students each from the CSE major branch of 2014 and
#2020, each student was asked about their salary package after they completed
#graduation. We wanted to know if the typical salary offered after graduation
#had changed over the past 6 years
batch2014=c(667,859,1129,500,1098,1036) 
batch2014
batch2020=c(920,1060,800,645,869,1101)
batch2020

x1b=mean(batch2014)
x2b=mean(batch2020)

s1=sd(batch2014)
s2=sd(batch2020)

n1=length(batch2014)
n2=length(batch2020)

diff_in_means=x1b-x2b
SE_diff_mean=sqrt((s1^2/n1)+(s2^2/n2))
t_stat=diff_in_means/SE_diff_mean
t_stat

### The pt() work in R Language is utilized to return the likelihood aggregate 
#thickness of the Student t-distribution.
pval=2*pt(t_stat,df=n1+n2-2)
pval



##################   Problem 3.1 testing of hypothesis (z proportion-test)
#Out of a sample of 1000 people, 300 watched movies in theatres before the
#pandemic. In a sample of 1200 persons, it was discovered that 350 of them
#watched movies in theatres after the outbreak. We wish to investigate whether
#there is a decline in theater attendance following the pandemic at the 5%
#level of significance
prop.test(x=c(300,350), n=c(1000,1200),,alternative = "two.sided")


###################    Problem 3.2 
#Perform two-proportion z-test
prop.test(x=c(800,900),n=c(1000,1200),alternative = "greater")


######################   Problem 4
#Estimate the correlation coefficient using pearson correlation and spearman rank correlation (solve using manual method and R code)
x = c(11,32,12,4,5)
y = c(1,3,71,69,20)

#Pearson Method
cor(x,y,method = "pearson")
#Spearman Method
cor(x,y,method = "spearman")
