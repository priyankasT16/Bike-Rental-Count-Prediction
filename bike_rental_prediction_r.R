##setting working directory
setwd("C:/Users/USER/Desktop/edwisor exercise/project/r")
library(caret)
library(e1071)
library(rcompanion)
library(ie2misc)
library(MASS)
library(randomForest)
library(lattice)
library(grid)
library(DMwR)
library(dplyr)
library(compare)
#getwd()
data_bike=read.csv("day.csv")
data_bike=as.data.frame(data_bike)
str(data_bike)
#head(data_bike)
######EDA and misssing values analysis#####################
data_bike$season=as.factor(data_bike$season)
data_bike$yr=as.factor(data_bike$yr)
data_bike$holiday=as.factor(data_bike$holiday)
data_bike$workingday=as.factor(data_bike$workingday)
data_bike$weathersit=as.factor(data_bike$weathersit)
data_bike$weekday=as.factor(data_bike$weekday)
data_bike$mnth=as.factor(data_bike$mnth)
data_bike$dteday=as.Date(data_bike$dteday,format="%d-%m-%Y")
str(data_bike)
sum(is.na(data_bike))
#####################Exploring data#################################33
library(DataExplorer)
create_report(data_bike)
################Outlier analysis##############################3
#head(data_bike)
oa_bike=data_bike
###dropping instant and dteday for outlier analysis.
oa_bike=oa_bike[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
str(oa_bike)
boxplot(oa_bike$temp,xlab='temp outlier analysis')
boxplot(oa_bike$atemp,xlab='atemp outlier analysis')
boxplot(oa_bike$hum,xlab='hum outlier analysis')
boxplot(oa_bike$windspeed,xlab='windspeed outlier analysis')
boxplot(oa_bike$cnt,xlab='cnt outlier analysis')
#str(oa_bike)
c=colnames(oa_bike)
for (i in c){
  value=oa_bike[,i][oa_bike[,i] %in% boxplot.stats(oa_bike[,i])$out]
  oa_bike[,i][oa_bike[,i] %in% value]=NA
}
sum(is.na(oa_bike))
for (i in c){
  oa_bike[,i][is.na(oa_bike[,i])]=mean(oa_bike[,i],na.rm = T)
}
sum(is.na(oa_bike))
boxplot(oa_bike$temp,xlab='temp outlier analysis')
boxplot(oa_bike$atemp,xlab='atemp outlier analysis')
boxplot(oa_bike$hum,xlab='hum outlier analysis')
boxplot(oa_bike$windspeed,xlab='windspeed outlier analysis')
boxplot(oa_bike$cnt,xlab='cnt outlier analysis')
##############################################  Feature selection#####
str(oa_bike)
data_bike$day=format(data_bike$dteday,"%d")
data_bike$day=as.integer(data_bike$dteday)
#colnames(data_bike)
#colnames(oa_bike)
data_bike$season=oa_bike$season
data_bike$yr=oa_bike$yr
data_bike$mnth=oa_bike$mnth
data_bike$holiday=oa_bike$holiday
data_bike$weekday=oa_bike$weekday
data_bike$workingday=oa_bike$workingday
data_bike$weathersit=oa_bike$weathersit
data_bike$temp=oa_bike$temp
data_bike$atemp=oa_bike$atemp
data_bike$hum=oa_bike$hum
data_bike$windspeed=oa_bike$windspeed
data_bike$casual=oa_bike$casual
data_bike$registered=oa_bike$registered
data_bike$cnt=oa_bike$cnt
str(data_bike)
#boxplot(data_bike$windspeed)
rm(oa_bike)
plot_correlation(data_bike,type='continuous','cnt')
colnames(data_bike)
data_bike=data_bike[,c(17,3,4,5,7,8,9,10,12,13,16)]
colnames(data_bike)
###################Dividing data into train and test#######
set.seed(1)
index=sample(nrow(data_bike),0.8*nrow(data_bike))
train_bike=data_bike[index,]
test_bike=data_bike[-index,]
str(train_bike)
str(test_bike)
##############################################################
#mpart=rpart(cnt~.,data = train_bike,method = 'anova')
#summary(mpart)
#pred_mpart=predict(mpart,test_bike[,-11])
#regr.eval(test_bike$cnt,pred_mpart)
################################### Using linear regression###############################
mlr=lm(cnt ~.,data=train_bike)
pred_lm=predict(mlr,test_bike[,-11])
summary(mlr)
regr.eval(test_bike$cnt,pred_lm)
###############################################3
#str(train_bike)
#str(test_bike)
data.frame(test_bike$cnt,pred_lm)
##############################Using Randomforest#######################
mrf = randomForest(cnt ~ ., train_bike, importance = TRUE, ntree = 500)
pred_mrf=predict(mrf,test_bike[,-11])
summary(mrf)
summary(pred_mrf)
regr.eval(test_bike$cnt,pred_mrf)
#######################################  increasing no of trees##############33
mrf2 = randomForest(cnt ~ ., train_bike, importance = TRUE, ntree = 800)
pred_mrf2=predict(mrf2,test_bike[,-11])
summary(mrf2)
regr.eval(test_bike$cnt,pred_mrf2)
####################################################################3
mrf3 = randomForest(cnt ~ ., train_bike, importance = TRUE, ntree = 1000)
pred_mrf3=predict(mrf3,test_bike[,-11])
summary(mrf3)
regr.eval(test_bike$cnt,pred_mrf3)
######################################################### Final model selection####
str(test_bike$cnt)
str(pred_mrf2)
output=data.frame(test_bike$cnt,pred_mrf2)
colnames(output)=c("actual value"," predicted values")
output
write.csv(output,file = 'output_r.csv',row.names = FALSE)

