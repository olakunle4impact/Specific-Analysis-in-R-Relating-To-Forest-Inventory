getwd()
setwd(dir="C:/Users/BIODUN/Desktop/Assignment")
muyiwa<-read.csv("Data2.csv")
attach(joshua)
names(joshua)


##Missing Value
apply(joshua,2,function(x)sum(is.na(x)))

##Divide data into 2 parts

index<-sample(1:nrow(joshua),round(0.75*nrow(joshua)))

train<-joshua[index,]
test<-joshua[-index,]

##To Predict 

predH<-predict(fmnaslund,test)
test$predH<-predict(fmnaslund,test)
test

RMSE<-sqrt(sum(test$predH-test$THt)^2/nrow(test))
RMSE
MAB<-mean(abs(test$THt-test$predH))
MAB
t.test(test$THt,predH,paired=TRUE)	##paired sample t-test
t.test(test$THt,predH)		##Unpaired Sample t-test

