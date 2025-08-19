rm(list=ls())

#attaching required libraries#
library(ggplot2)
library(cowplot)
library(ggeffects)
library(DescTools)
library(ggpubr)

#reading the data#
getwd()
A=read.csv(file="C:/Users/profu/OneDrive/Documents/Dissertation_Regression_Data.csv",header=T)
View(A)
A=na.omit(A);A
View(A)

#reading the data in categorical form#
data0=read.csv(file="C:/Users/profu/OneDrive/Documents/dissertation_data.csv",header=T)
data0
attach(data0)
data0=na.omit(data0) #to omit missing values (if any)#
View(data0)
str(data0)           #to check the structure of the data#

#We can observe that all of the predictors are being taken as integers, but apart from age, all of the other predictors should be taken as factors.#
#So, we convert them into factors.#

y.new=as.factor(y)
gender.new=as.factor(gender)
income.new=as.factor(income)
smoking.new=as.factor(smoking)
friends.new=as.factor(friends)

data=data.frame(y.new,age,gender.new,income.new,smoking.new,friends.new);data                #to create new data farme, with the transformed predictors#

data1=data.frame(y,age,gender,income,smoking,friends);data1

str(data)  #to check the structure of the new data frame, after transformations#

#separting those values of the predictors corresponding to y=1#
data.1=read.csv(file="C:/Users/profu/OneDrive/Documents/dissertation_data_1.csv",header=T)
data.1
data.1=na.omit(data.1)
data.1

#separting those values of the predictors corresponding to y=0#
data.0=read.csv(file="C:/Users/profu/OneDrive/Documents/dissertation_data_0.csv",header=T)
data.0
data.0=na.omit(data.0)
data.0

#frequency of observations#

table(A$PECA)

table(data$age)
table(data.1$age1)
table(data.0$age0)

table(data$gender)
table(data.1$gender1)
table(data.0$gender0)

table(data$income)
table(data.1$income1)
table(data.0$income0)

table(data$smoking)
table(data.1$smoking1)
table(data.0$smoking0)

table(data$friends)
table(data.1$friends1)
table(data.0$friends0)

table(data.0$try.curiosity)
table(data.0$try.friend)

#plots of y vs each predictor#

A1=ggplot(data=data1,aes(x=gender.new,y=y))+geom_jitter(height=0.05,alpha=0.1)+stat_smooth(method="glm",color="green",se=FALSE,method.args = list(family=binomial))+ggtitle("Scatterplot of y vs gender (x2)")+xlab("gender (x2)")
B1=ggplot(data=data1,aes(x=income.new,y=y))+geom_jitter(height=0.05,alpha=0.1)+stat_smooth(method="glm",color="green",se=FALSE,method.args = list(family=binomial))+ggtitle("Scatterplot of y vs income (x3)")+xlab("income (x3)")
C1=ggplot(data=data1,aes(x=smoking.new,y=y))+geom_jitter(height=0.05,alpha=0.1)+stat_smooth(method="glm",color="green",se=FALSE,method.args = list(family=binomial))+ggtitle("Scatterplot of y vs smoking status (x4)")+xlab("smoking status (x4)")
D1=ggplot(data=data1,aes(x=friends.new,y=y))+geom_jitter(height=0.05,alpha=0.1)+stat_smooth(method="glm",color="green",se=FALSE,method.args = list(family=binomial))+ggtitle("Scatterplot of y vs number of friends who vape (x5)")+xlab("friends (x5)")
A1;B1;C1;D1
figure=ggarrange(A1,B1,C1,D1,labels = c("A", "B", "C", "D"),ncol = 2, nrow = 2)
figure

#reading excel file of count data#
B=read.csv(file="C:/Users/profu/OneDrive/Documents/dissertation_data_counts.csv",header=T)
B
B=na.omit(B):B
attach(B)

vape.counts=na.omit(vape.counts)
lbls0=c("Have smoked e-cigarettes - ", "Have never smoked e-cigarettes - ")
pct0=round(vape.counts/sum(vape.counts)*100)
lbls0=paste(lbls0, pct0)
lbls0=paste(lbls0,"%",sep="")      #add % to labels#
pie(vape.counts,labels=lbls0,col=c("navy blue","light blue"),main="Pie Chart of Proportion of Respondents who Consume E-Cigarettes")

par(mfrow=c(3,1))
lbls1=c("18 years - ", "19 years - ", "20 years - ", "21 years - ", "22 years - ")
pct1=round(total.age/sum(total.age)*100)
lbls1=paste(lbls1, pct1)
lbls1=paste(lbls1,"%",sep="")      #add % to labels#
pie(total.age,labels=lbls1,col=rainbow(length(lbls1)),main="Pie Chart of Age Distribution of Respondents")

lbls2=c("18 years - ", "19 years - ", "20 years - ", "21 years - ", "22 years - ")
pct2=round(age.1/sum(age.1)*100)
lbls2=paste(lbls2, pct2)
lbls2=paste(lbls2,"%",sep="")      #add % to labels#
pie(age.1,labels=lbls2,col=rainbow(length(lbls2)),main="Pie Chart of Age Distribution of Respondents who Vape")

lbls3=c("18 years - ", "19 years - ", "20 years - ", "21 years - ", "22 years - ")
pct3=round(age.0/sum(age.0)*100)
lbls3=paste(lbls3, pct3)
lbls3=paste(lbls3,"%",sep="")      #add % to labels#
pie(age.0,labels=lbls3,col=rainbow(length(lbls3)),main="Pie Chart of Age Distribution of Respondents who do not Vape")

par(mfrow=c(3,1))
total.gender=na.omit(total.gender)
lbls4=c(" Female - ", " Male - ")
pct4=round(total.gender/sum(total.gender)*100)
lbls4=paste(lbls4, pct4)
lbls4=paste(lbls4,"%",sep="")      #add % to labels#
pie(total.gender,labels=lbls4,col=c("light green","pink"),main="Pie Chart of Gender Distribution of Respondents")

gender.1=na.omit(gender.1)
lbls5=c(" Female - ", " Male - ")
pct5=round(gender.1/sum(gender.1)*100)
lbls5=paste(lbls5, pct5)
lbls5=paste(lbls5,"%",sep="")      #add % to labels#
pie(gender.1,labels=lbls5,col=c("light green","pink"),main="Pie Chart of Gender Distribution of Respondents who Vape")

gender.0=na.omit(gender.0)
lbls6=c(" Female - ", " Male - ")
pct6=round(gender.0/sum(gender.0)*100)
lbls6=paste(lbls6, pct6)
lbls6=paste(lbls6,"%",sep="")      #add % to labels#
pie(gender.0,labels=lbls6,col=c("light green","pink"),main="Pie Chart of Gender Distribution of Respondents who do not Vape")

par(mfrow=c(3,1))
total.smoker=na.omit(total.smoker)
lbls7=c(" Non-Smoker - ", " Smoker - ")
pct7=round(total.smoker/sum(total.smoker)*100)
lbls7=paste(lbls7, pct7)
lbls7=paste(lbls7,"%",sep="")      #add % to labels#
pie(total.smoker,labels=lbls7,col=c("green","blue"),main="Pie Chart showing Proportion of Smokers among the Respondents")

smoker.1=na.omit(smoker.1)
lbls8=c(" Non-Smoker - ", " Smoker - ")
pct8=round(smoker.1/sum(smoker.1)*100)
lbls8=paste(lbls8, pct8)
lbls8=paste(lbls8,"%",sep="")      #add % to labels#
pie(smoker.1,labels=lbls8,col=c("green","blue"),main="Pie Chart showing Proportion of Smokers among the Respondents who Vape")

smoker.0=na.omit(smoker.0)
lbls9=c(" Non-Smoker - ", " Smoker - ")
pct9=round(smoker.0/sum(smoker.0)*100)
lbls9=paste(lbls9, pct9)
lbls9=paste(lbls9,"%",sep="")      #add % to labels#
pie(smoker.0,labels=lbls9,col=c("green","blue"),main="Pie Chart showing Proportion of Smokers among the Respondents who do not Vape")

par(mfrow=c(3,1))
total.income=na.omit(total.income)
lbls10=c(" Low Income Class - ", " High Income Class - ")
pct10=round(total.income/sum(total.income)*100)
lbls10=paste(lbls10, pct10)
lbls10=paste(lbls10,"%",sep="")      #add % to labels#
pie(total.income,labels=lbls10,col=c("yellow","red"),main="Pie Chart showing the Income Class Distribution of the Respondents")

income.1=na.omit(income.1)
lbls11=c(" Low Income Class - ", " High Income Class - ")
pct11=round(income.1/sum(income.1)*100)
lbls11=paste(lbls11, pct11)
lbls11=paste(lbls11,"%",sep="")      #add % to labels#
pie(income.1,labels=lbls11,col=c("yellow","red"),main="Pie Chart showing the Income Class Distribution of the Respondents who Vape")

income.0=na.omit(income.0)
lbls12=c(" Low Income Class - ", " High Income Class - ")
pct12=round(income.0/sum(income.0)*100)
lbls12=paste(lbls12, pct12)
lbls12=paste(lbls12,"%",sep="")      #add % to labels#
pie(income.0,labels=lbls12,col=c("yellow","red"),main="Pie Chart showing the Income Class Distribution of the Respondents who do not Vape")

par(mfrow=c(3,1))
total.friends=na.omit(total.friends)
lbls13=c(" 0 friends who vape - ", " 1 or more friends who vape - ")
pct13=round(total.friends/sum(total.friends)*100)
lbls13=paste(lbls13, pct13)
lbls13=paste(lbls13,"%",sep="")      #add % to labels#
pie(total.friends,labels=lbls13,col=c("light blue","purple"),main="Pie Chart showing the Proportion of Influential Friends among the Respondents")

friends.1=na.omit(friends.1)
lbls14=c(" 0 friends who vape - ", " 1 or more friends who vape - ")
pct14=round(friends.1/sum(friends.1)*100)
lbls14=paste(lbls14, pct14)
lbls14=paste(lbls14,"%",sep="")      #add % to labels#
pie(friends.1,labels=lbls14,col=c("light blue","purple"),main="Pie Chart showing the Proportion of Influential Friends among the Respondents who Vape")

friends.0=na.omit(friends.0)
lbls15=c(" 0 friends who vape - ", " 1 or more friends who vape - ")
pct15=round(friends.0/sum(friends.0)*100)
lbls15=paste(lbls15, pct15)
lbls15=paste(lbls15,"%",sep="")      #add % to labels#
pie(friends.0,labels=lbls15,col=c("light blue","purple"),main="Pie Chart showing the Proportion of Influential Friends among the Respondents who do not Vape")

par(mfrow=c(2,1))
try.curiosity.counts=na.omit(try.curiosity.counts)
lbls17=c(" Definitely - ", " Maybe - "," Never - ")
pct17=round(try.curiosity.counts/sum(try.curiosity.counts)*100)
lbls17=paste(lbls17, pct17)
lbls17=paste(lbls17,"%",sep="")      #add % to labels#
pie(try.curiosity.counts,labels=lbls17,col=c("yellow","light green","light pink"),main="Pie Chart showing the Percentage of Non-Vapers that might try Vapes out of Curiosity") 

try.friend.counts=na.omit(try.friend.counts)
lbls18=c(" Definitely - ", " Maybe - "," Never - ")
pct18=round(try.friend.counts/sum(try.friend.counts)*100)
lbls18=paste(lbls18, pct18)
lbls18=paste(lbls18,"%",sep="")      #add % to labels#
pie(try.friend.counts,labels=lbls18,col=c("yellow","light green","light pink"),main="Pie Chart showing the Percentage of Non-Vapers that might try Vapes if offered by their Friends")

#logit link#

model=glm(y.new~age+gender.new+income.new+smoking.new+friends.new,data,family=binomial(link="logit"))
model
summary(model)          #AIC=250.64#

exp(coef(model))        #Odds ratio#

#PREDICTION#

pi.hat=fitted(model);pi.hat        #to obtain the values of pi's#
d1=data.frame(pi.hat,y);d1         #making a data frame of pi and y values#

pi=sort(pi.hat,decreasing=F);pi    #arranging the pi values in ascending order#

d=d1[order(pi.hat),];d              

#rearranging data frame in ascending order of pi values and corresponding y values# 

#considering 4 thresholds - fist quartile, mean, median  and third quartile- to find which one gives lower misclassification#

#FIRST QUARTILE Q1#
p0=quantile(pi.hat,0.25);p0          #Q1(pi.hat)=0.343146#
Y0.hat=ifelse(pi<=p0,0,1);Y0.hat
d$Y0.hat=Y0.hat;d
table(y,Y0.hat)                      #confusion matrix#
TPR0=114/(40+114);TPR0               #TPR=P[Y=1|y=1]=0.7402597#  
FPR0=70/(28+70);FPR0                 #FPR=P[Y=1|y=0]=0.7142857#

C0=1-TPR0+FPR0;C0          #total prob of misclassification=0.974026#

#MEAN#
p1=mean(pi.hat);p1                   #mean(pi.hat)=0.6111111#
Y1.hat=ifelse(pi<=p1,0,1);Y1.hat
d$Y1.hat=Y1.hat;d
table(y,Y1.hat)                      #confusion matrix#
TPR1=91/(63+91);TPR1                 #TPR=P[Y=1|y=1]=0.5909091#  
FPR1=47/(51+47);FPR1                 #FPR=P[Y=1|y=0]=0.4795918#

C1=1-TPR1+FPR1;C1          #total prob of misclassification=0.8886827#

#MEDIAN#
p2=median(pi.hat);p2                 #median(pi.hat)=0.7839838#
Y2.hat=ifelse(pi<=p2,0,1);Y2.hat
d$Y2.hat=Y2.hat;d
table(y,Y2.hat)                      #confusion matrix#
TPR2=79/(75+79);TPR2                 #TPR=P[Y=1|y=1]=0.512987#  
FPR2=40/(58+40);FPR2                 #FPR=P[Y=1|y=0]=0.408163#

C2=1-TPR2+FPR2;C2          #total prob of misclassification=0.8951763#

#THIRD QUARTILE Q3#
p3=quantile(pi.hat,0.75);p3          #Q3(pi.hat)=0.8297085#
Y3.hat=ifelse(pi<=p3,0,1);Y3.hat
d$Y3.hat=Y3.hat;d
table(y,Y3.hat)                      #confusion matrix#
TPR3=39/(115+39);TPR3                #TPR=P[Y=1|y=1]=0.2532468#  
FPR3=23/(75+23);FPR3                 #FPR=P[Y=1|y=0]=0.2346939#

C3=1-TPR3+FPR3;C3          #total prob of misclassification=0.9814471#

#total probabilty of misclassification is lowest for mean as threshold#

#so we choose mean value as the threshold#
#the corresponding y vlaues are taken as the predicted y values#

#ROC CURVE#

TPR=FPR=array(0)
k=1
for(i in pi)
{ 
 Y_hat_logit=ifelse(pi.hat>i,1,0)
 t=table(Y_hat_logit,y) 
 TPR[k]=t[2,2]/(t[1,2]+t[2,2]) 
 FPR[k]=t[2,1]/(t[2,1]+t[1,1])
 k=k+1 
}                  
TPR
FPR

par(mfrow=c(1,1))
x=seq(0,1,0.1) 
plot(FPR,TPR,xlim=c(0,1),ylim=c(0,1),type="l",main="Receiver Operated Curve (ROC)",col="blue") 
lines(x,x,col="red")
legend("bottomright",legend=c("ROC Curve","Chance Line (TPR=FPR)"),fill=c("blue","red"))


