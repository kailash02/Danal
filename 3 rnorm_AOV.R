# Hypothesis testing

# Question 1
#######################  20BRS1240 ###############
# A new drug released. A total of 14000 individuals
# with high bp (x=150, sd=10) given for a month and
# tested them again. Mean decreased to 144, sd=9
before <- rnorm(14000, 150, 10)
after <- rnorm(14000, 144, 9)
t.test(before, after, paired=TRUE)



# Question 2 
# Multi way ANOVA test
# This table gives monthly s of a firm in 3 states
# by 4 sman. Test whether thee is any significant difference
# i. between smen
# ii. between s in the state
#States smen

#  I II III IV
#A 6 5   3  8
#B 8 9   6  5
#C 10 7  8  7

s<-c('A','B','C')
s1<-c(6,8,10)
s2<-c(5,9,7)
s3<-c(3,6,8)
s4<-c(8,5,7)
df<-data.frame(s,s1,s2,s3,s4)
df
a<-aov(s1~s2+s3+s4)
summary(a)
A<-c(6,5,3,8)
B<-c(8,9,6,5)
C<-c(10,7,8,7)
summary(aov(A~B+C))