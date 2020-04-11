HR_Data=read.csv("E:\\R_Lectures\\dr. mohit\\PROJECT\\HR_Data.csv")
View(HR_Data)
?ChickWeight

#################Creation of training and testing dataset. So that we can test the created model on both the sets to look at the training and testing accuracy in order to judge that how effective over modelled prediction structure will be on unseen data.
l=sample(1:length(HR_Data$left),floor(0.7*length(HR_Data$left)))
train_data=HR_Data[l,]
test_data=HR_Data[-l,]


###################Modelling on train data.
lm_train=glm(left~.,data=train_data,family=binomial)
summary(lm_train)
#Asteriks against every entry in the table genrated by the summary signifies the importance of coresponding variables on the result.

#Training accuracy.
lm_train.prob =predict (lm_train,type ="response")
lm_train.pred=rep("0",length(train_data$left))
lm_train.pred[lm_train.prob>0.5]="1"
table(lm_train.pred,train_data$left,dnn = c('predicted','true'))
accu_train=(sum((lm_train.pred==train_data$left)/length(train_data$left)))*100
accu_train

#Testing accuracy.
lm_test.prob =predict(lm_train,test_data,type ="response")
lm_test.pred=rep("0",length(test_data$left))
lm_test.pred[lm_test.prob>0.5]="1"
table(lm_test.pred,test_data$left,dnn = c('predicted','true'))
accu_test=(sum((lm_test.pred==test_data$left)/length(test_data$left)))*100
accu_test

#
print(paste("tarining accuracy:",accu_train,"%       testing accuracy:",accu_test,"%" ))




##################Modelling data only on the basis of variables with p values less than '2e-16'.
lm_train1=glm(left~.-department-promotion_last_5years-average_montly_hours-last_evaluation ,data=train_data,family=binomial)
summary(lm_train1)
    #We can quite clearly see that the p value of every variable has moved more close to 0.

#Training accuracy
lm_train1.probs =predict (lm_train1,type ="response")
lm_train1.pred=rep("0",length(train_data$left))
lm_train1.pred[lm_train1.probs>0.5]="1"
table(lm_train1.pred,train_data$left,dnn = c('predicted','true'))
accu_train1=(sum((lm_train1.pred==train_data$left)/length(train_data$left)))*100
accu_train1

#Testing accuracy.
lm_test1.prob =predict(lm_train1,test_data,type ="response")
lm_test1.pred=rep("0",length(test_data$left))
lm_test1.pred[lm_test1.prob>0.5]="1"
table(lm_test1.pred,test_data$left,dnn = c('predicted','true'))
accu_test1=(sum((lm_test1.pred==test_data$left)/length(test_data$left)))*100
accu_test1

#
print(paste("tarining accuracy:",accu_train1,"%       testing accuracy:",accu_test1,"%" ))
    #We have quite clearly seen the improvement in train and test accuracy than what it was before.



###############Modelling data after adding the interaction term(time_spend_company:promotion_last_5years) obtained on the basis of EDA.
lm_train3=glm(left~.-department-promotion_last_5years-average_montly_hours-last_evaluation+time_spend_company:promotion_last_5years,data=train_data,family=binomial)
summary(lm_train3)


#Training accuracy
lm_train3.probs =predict (lm_train3,type ="response")
lm_train3.pred=rep("0",length(train_data$left))
lm_train3.pred[lm_train3.probs>0.5]="1"
table(lm_train3.pred,train_data$left,dnn = c('predicted','true'))
accu_train3=(sum((lm_train3.pred==train_data$left)/length(train_data$left)))*100
accu_train3

#Testing accuracy.
lm_test3.prob =predict(lm_train3,test_data,type ="response")
lm_test3.pred=rep("0",length(test_data$left))
lm_test3.pred[lm_test3.prob>0.5]="1"
table(lm_test3.pred,test_data$left,dnn = c('predicted','true'))
accu_test3=(sum((lm_test3.pred==test_data$left)/length(test_data$left)))*100
accu_test3

#
print(paste("tarining accuracy:",accu_train3,"%       testing accuracy:",accu_test3,"%" ))
#Both train and test accuracy has improved as compared to lm_train1.

###########Modelling data after adding the interaction term(average_montly_hours:salary) obtained on the basis of EDA.
lm_train4=glm(left~.-department-promotion_last_5years-average_montly_hours-last_evaluation+average_montly_hours:salary,data=train_data,family=binomial)
summary(lm_train4)


#Training accuracy
lm_train4.probs =predict (lm_train4,type ="response")
lm_train4.pred=rep("0",length(train_data$left))
lm_train4.pred[lm_train4.probs>0.5]="1"
table(lm_train4.pred,train_data$left,dnn = c('predicted','true'))
accu_train4=(sum((lm_train4.pred==train_data$left)/length(train_data$left)))*100
accu_train4

#Testing accuracy.
lm_test4.prob =predict(lm_train4,test_data,type ="response")
lm_test4.pred=rep("0",length(test_data$left))
lm_test4.pred[lm_test4.prob>0.5]="1"
table(lm_test4.pred,test_data$left,dnn = c('predicted','true'))
accu_test4=(sum((lm_test4.pred==test_data$left)/length(test_data$left)))*100
accu_test4

#
print(paste("tarining accuracy:",accu_train4,"%       testing accuracy:",accu_test4,"%" ))
#Both train and test accuracy has decreased as compared to lm_train1.




##########Modelling data after adding the interaction term(time_spend_company:salary) obtained on the basis of EDA.
lm_train5=glm(left~.-department-promotion_last_5years-average_montly_hours-last_evaluation+time_spend_company:salary,data=train_data,family=binomial)
summary(lm_train5)


#Training accuracy
lm_train5.probs =predict (lm_train5,type ="response")
lm_train5.pred=rep("0",length(train_data$left))
lm_train5.pred[lm_train5.probs>0.5]="1"
table(lm_train5.pred,train_data$left,dnn = c('predicted','true'))
accu_train5=(sum((lm_train5.pred==train_data$left)/length(train_data$left)))*100
accu_train5

#Testing accuracy.
lm_test5.prob =predict(lm_train5,test_data,type ="response")
lm_test5.pred=rep("0",length(test_data$left))
lm_test5.pred[lm_test5.prob>0.5]="1"
table(lm_test5.pred,test_data$left,dnn = c('predicted','true'))
accu_test5=(sum((lm_test5.pred==test_data$left)/length(test_data$left)))*100
accu_test5

#
print(paste("tarining accuracy:",accu_train5,"%       testing accuracy:",accu_test5,"%" ))
#Both train and test accuracy has decreased as compared to lm_train1.


################Modelling after adding interaction term(time_spend_company:satisfaction_level:salary).
lm_train2=glm(left~.-department-promotion_last_5years-average_montly_hours-last_evaluation+time_spend_company:satisfaction_level:salary ,data=train_data,family=binomial)
summary(lm_train2)
#We can quite clearly see that the p value of every variable has moved more close to 0.

#Training accuracy
lm_train2.probs =predict (lm_train2,type ="response")
lm_train2.pred=rep("0",length(train_data$left))
lm_train2.pred[lm_train2.probs>0.5]="1"
table(lm_train2.pred,train_data$left,dnn = c('predicted','true'))
accu_train2=(sum((lm_train2.pred==train_data$left)/length(train_data$left)))*100
accu_train2

#Testing accuracy.
lm_test2.prob =predict(lm_train2,test_data,type ="response")
lm_test2.pred=rep("0",length(test_data$left))
lm_test2.pred[lm_test2.prob>0.5]="1"
table(lm_test2.pred,test_data$left,dnn = c('predicted','true'))
accu_test2=(sum((lm_test2.pred==test_data$left)/length(test_data$left)))*100
accu_test2

#
print(paste("tarining accuracy:",accu_train2,"%       testing accuracy:",accu_test2,"%" ))
#We have quite clearly seen the improvement in train and test accuracy as compared to lm_train5.


#############Modelling after adding interaction term(time_spend_company:satisfaction_level:salary & #Modelling after adding interaction term(time_spend_company:satisfaction_level:salary).
 

lm_train6=glm(left~.-department-average_montly_hours-last_evaluation+time_spend_company:promotion_last_5years+time_spend_company:satisfaction_level:salary ,data=train_data,family=binomial)
summary(lm_train6)
#We can quite clearly see that the p value of every variable has moved more close to 0.

#Training accuracy
lm_train6.probs =predict (lm_train6,type ="response")
lm_train6.pred=rep("0",length(train_data$left))
lm_train6.pred[lm_train6.probs>0.5]="1"
table(lm_train6.pred,train_data$left,dnn = c('predicted','true'))
accu_train6=(sum((lm_train6.pred==train_data$left)/length(train_data$left)))*100
accu_train6

#Testing accuracy.
lm_test6.prob =predict(lm_train6,test_data,type ="response")
lm_test6.pred=rep("0",length(test_data$left))
lm_test6.pred[lm_test6.prob>0.5]="1"
table(lm_test6.pred,test_data$left,dnn = c('predicted','true'))
accu_test6=(sum((lm_test6.pred==test_data$left)/length(test_data$left)))*100
accu_test6

#
print(paste("tarining accuracy:",accu_train6,"%       testing accuracy:",accu_test6,"%" ))
#Test and train accuracy both have increased as compared to lm_train2.

###########Checking corelation between used variables.
cor(train_data[,c("satisfaction_level","number_project","time_spend_company","Work_accident")])
#There is no significant amount of corelation among checked vairables.
