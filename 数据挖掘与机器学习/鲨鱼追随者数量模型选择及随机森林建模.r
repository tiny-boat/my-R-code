rm(list=ls())
shark <- read.csv("D:\ͳ�Ʒ�������\����.csv")
anova(lm(shark[,c(1,2)]))
anova(lm(shark[,c(1,3)]))

#��������ͼ
attach(shark)
par(mfrow=c(1,4))
boxplot(satell~color,main="color")
boxplot(satell~spine,main="spine")

#����ɢ��ͼ
data1 <- shark[,c(1,4)]
data2 <- shark[,c(1,5)]
plot(data1)
plot(data2)

#ģ��ѡ��

z <- 10

#����������ֳ�z��

ss <- nrow(shark)/z #����������
train <- matrix(0,z,ss) #ѵ����Ԥ����������
sn <- c(1:nrow(shark)) #�������������
set.seed(7)
for(j in 1:z){
  train[j,] <- sample(sn,ss,F)
  sn <- setdiff(sn,train[j,])
}
 
error1 <- rep(0,z)
error2 <- rep(0,z)
error3 <- rep(0,z)
error4 <- rep(0,z)
error5 <- rep(0,z)
error6 <- rep(0,z)
error7 <- rep(0,z)
Error <- rep(0,7)
  
#ģ��һ��������

library(tree)

for(k in 1:z){
  bio.tree <- tree(formula=satell~., shark[-train[k,],])
  bio.tree.pred <- predict(bio.tree,shark[train[k,],])
  error1[k] <- mean((bio.tree.pred-shark[train[k,],'satell'])^2)
}

Error[1] <- mean(error1)

#ģ�Ͷ���Bagging

library(randomForest)

for(k in 1:z){
  bio.bag <- randomForest(formula=satell~., data=shark[-train[k,],], mtry=ncol(shark)-1)
  bio.bag.pred <- predict(bio.bag,shark[train[k,],])
  error2[k] <- mean((bio.bag.pred-shark[train[k,],'satell'])^2)
}

Error[2] <- mean(error2)

#ģ���������ɭ��

for(k in 1:z){
  bio.rf <- randomForest(formula=satell~., data=shark[-train[k,],], importance=T)
  bio.rf.pred <- predict(bio.rf,shark[train[k,],])
  error3[k] <- mean((bio.rf.pred-shark[train[k,],'satell'])^2)
}

Error[3] <- mean(error3)

#ģ���ģ�Adaboost

library(gbm)

for(k in 1:z){
  bio.boost <- gbm(formula=satell~., data=shark[-train[k,],], distribution='gaussian', n.trees=5000, interaction.depth=4)
  bio.boost.pred <- predict(bio.boost,shark[train[k,],],n.trees=5000)
  error4[k] <- mean((bio.boost.pred-shark[train[k,],'satell'])^2)
}

Error[4] <- mean(error4)

#ģ���壺���ɻع�ģ��
  
for(k in 1:z){
  n <- nrow(shark[-train[k,],])
  glm <- glm(formula=satell~., data=shark[-train[k,],],family=poisson)
  AICglm <- step(glm,trace=F)
  BICglm <- step(glm,k=log(n),trace=F)
  glm.pred <- predict(glm, shark[train[k,],])
  AICglm.pred <- predict(AICglm, shark[train[k,],])
  BICglm.pred <- predict(BICglm, shark[train[k,],])
  error5[k] <- mean((glm.pred-shark[train[k,],'satell'])^2)
  error6[k] <- mean((AICglm.pred-shark[train[k,],'satell'])^2)
  error7[k] <- mean((BICglm.pred-shark[train[k,],'satell'])^2)
}
  
Error[5] <- mean(error5)
Error[6] <- mean(error6)
Error[7] <- mean(error7)

Error

#���ɭ�ֽ�ģ

rm(list=ls())
shark <- read.csv("����.csv")

library(randomForest)

bio.rf <- randomForest(formula=satell~., data=shark, importance=T)
importance(bio.rf)
bio.rf.pred <- predict(bio.rf,shark)
error <- mean((bio.rf.pred-shark$satell)^2)

error
bio.rf
