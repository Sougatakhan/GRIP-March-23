reg_data<-read.csv('http://bit.ly/w-data')
View(reg_data)
dim(reg_data)
names(reg_data)

#Plotting the data
plot(reg_data,main="Hours Vs Percentage of Score",xlab="Hours Studied",ylab="Percentage of Score",pch=16,col="Red")
legend(x=1,y=95,'Scores',pch=16,col='Red')

#We see that there is a positive linear relationship between hours and percentage of score

# split the data into testing and training set
set.seed(0)
split<-sample(2,nrow(reg_data),replace=T,prob=c(0.7,0.3))
train<-reg_data[split==1,]
test<-reg_data[split==2,]

lmodel<-lm(Scores~Hours,data=train)
summary(lmodel)
plot(reg_data,main="Hours Vs Percentage of Score",xlab="Hours Studied",ylab="Percentage of Score",col="Red")
abline(lmodel,col="blue")

#Model Performance Evaluation
test$Predicted_Score= predict(lmodel,newdata = test)
test
#RMSE finding
library(Metrics)
rmse(train$Scores,predict(lmodel,train))
#What will be predicted score if a student studies for 9.25 hrs/ day?
predict(lmodel, data.frame(Hours = 9.25))