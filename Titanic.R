trainDataSet<-read.csv('file:///Users/chenrui/Desktop/R/titanic/train.csv',stringsAsFactors = FALSE)
testDataSet<-read.csv('file:///Users/chenrui/Desktop/R/titanic/test.csv',stringsAsFactors = FALSE)
library(ggplot2)
attach(trainDataSet)
table(trainDataSet$Survived)
head(trainDataSet)
#how class influence
ggplot(data=trainDataSet,aes(x=Pclass,y=..count..,fill=factor(Survived))) + geom_bar(stat="count")+
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
# how sex influence
ggplot(data=trainDataSet,aes(x=Sex,y=..count..,fill=factor(Survived))) + geom_bar(stat="count")+
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
#embarked
ggplot(data=trainDataSet,aes(x=Embarked,y=..count..,fill=factor(Survived))) + geom_bar(stat="count")+
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
#Sibsp
ggplot(data=trainDataSet,aes(x=SibSp,y=..count..,fill=factor(Survived))) + geom_bar(stat="count")+
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
#parch
ggplot(data=trainDataSet,aes(x=Parch,y=..count..,fill=factor(Survived))) + geom_bar(stat="count")+
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
#Age
ggplot(data=trainDataSet,aes(x=Age,fill=factor(Survived),color=Survived)) + geom_line(aes(label=..count..),stat='count',binwidth=5)+
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")




library(Hmisc)
library(rpart)
trainDataSet$Embarked<-impute(trainDataSet$Embarked,median)
trainDataSet$Fare<-impute(trainDataSet$Fare,median)
testDataSet$Embarked<-impute(testDataSet$Embarked,median)
testDataSet$Fare<-impute(testDataSet$Fare,median)
age.model <- rpart(Age ~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=trainDataSet[!is.na(trainDataSet$Age), ], method='anova')
trainDataSet$Age[is.na(trainDataSet$Age)] <- predict(age.model, trainDataSet[is.na(trainDataSet$Age), ])
age1.model <- rpart(Age ~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=testDataSet[!is.na(testDataSet$Age), ], method='anova')
testDataSet$Age[is.na(testDataSet$Age)] <- predict(age.model, testDataSet[is.na(testDataSet$Age), ])


ggplot(OriginalDataSet,aes(x=OriginalDataSet$Age))+geom_histogram(binwidth=5,fill='lightblue',colour='black')

#transfer embarked and male into numerical
trainDataSet$Sex[trainDataSet$Sex=='male']<-0
trainDataSet$Sex[trainDataSet$Sex=='female']<-1
trainDataSet$Embarked[trainDataSet$Embarked=='C']<-0
trainDataSet$Embarked[trainDataSet$Embarked=='S']<-1
trainDataSet$Embarked[trainDataSet$Embarked=='Q']<-2
testDataSet$Sex[testDataSet$Sex=='male']<-0
testDataSet$Sex[testDataSet$Sex=='female']<-1
testDataSet$Embarked[testDataSet$Embarked=='C']<-0
testDataSet$Embarked[testDataSet$Embarked=='S']<-1
testDataSet$Embarked[testDataSet$Embarked=='Q']<-2


total.data<-read.csv('file:///Users/chenrui/Desktop/R/titanic/total.csv',stringsAsFactors = FALSE)
total.data$Survived<-as.factor(total.data$Survived)
total.data$SibSp[total.data$SibSp>0]=1
total.data$Parch[total.data$Parch>0]=1
set.seed(5)
library(randomForest)






glm.fit=glm(Survived ~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=total.data[1:891,],family=binomial)
preds2=predict(glm.fit,newdata=total.data[892:1309,],type='response')
preds2[preds.glm>=0.5]=1
preds2[preds.glm<0.5]=0
write.csv(preds2, file = " forcast-5.csv", row.names = FALSE)

library(nnet)
nn=nnet(Survived ~Pclass+Sex+AgeEmbarked,data=total.data[1:891,],size=10,decay=0.01,maxit=1000,linout=F,trace=F)
nn.pred=predict(nn,newdata=total.data[892:1309,])


library(e1071)
svm.fit=svm(Survived ~Pclass+Sex+Age+Embarked+SibSp+Parch,data=total.data[1:891,],kernel='radial')
pred.svm=predict(svm.fit,newdata=total.data[892:1309,])
write.csv(pred.svm, file = " forcast-9.csv", row.names = FALSE)

library(tree)
tree.fit=tree(Survived ~Pclass+Sex+Age+Embarked+SibSp+Parch,data=total.data[1:891,])
tree.pred=predict(tree.fit,newdata=total.data[892:1309,])
write.csv(pred.svm, file = " forcast-10.csv", row.names = FALSE)

library(randomForest)
Rf.fit=randomForest(Survived ~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=total.data[1:891,],importance=TRUE,ntree=1000)
preds=predict(Rf.fit,newdata=total.data[892:1309,])
write.csv(pred.svm, file = " forcast-RF.csv", row.names = FALSE)

#library(xgboost)
#dtrain<-xgb.DMatrix(as.matrix(total.data[,3:9][1:891,]),label=total.data[1:891,]$Survived)
#xgb.fit<-xgboost(data=dtrain,max.depth=8,eta=1,nthread=2,nrounds=2,objective='binary:logistic')

