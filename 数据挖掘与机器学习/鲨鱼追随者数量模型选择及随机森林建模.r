rm(list=ls())
shark <- read.csv("D:\统计分析报告\鲨鱼.csv")
anova(lm(shark[,c(1,2)]))
anova(lm(shark[,c(1,3)]))

#绘制箱线图
attach(shark)
par(mfrow=c(1,4))
boxplot(satell~color,main="color")
boxplot(satell~spine,main="spine")

#绘制散点图
data1 <- shark[,c(1,4)]
data2 <- shark[,c(1,5)]
plot(data1)
plot(data2)

#模型选择

z <- 10

#将样本随机分成z份

ss <- nrow(shark)/z #抽样样本量
train <- matrix(0,z,ss) #训练与预测样本矩阵
sn <- c(1:nrow(shark)) #待抽样样本标号
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
  
#模型一：决策树

library(tree)

for(k in 1:z){
  bio.tree <- tree(formula=satell~., shark[-train[k,],])
  bio.tree.pred <- predict(bio.tree,shark[train[k,],])
  error1[k] <- mean((bio.tree.pred-shark[train[k,],'satell'])^2)
}

Error[1] <- mean(error1)

#模型二：Bagging

library(randomForest)

for(k in 1:z){
  bio.bag <- randomForest(formula=satell~., data=shark[-train[k,],], mtry=ncol(shark)-1)
  bio.bag.pred <- predict(bio.bag,shark[train[k,],])
  error2[k] <- mean((bio.bag.pred-shark[train[k,],'satell'])^2)
}

Error[2] <- mean(error2)

#模型三：随机森林

for(k in 1:z){
  bio.rf <- randomForest(formula=satell~., data=shark[-train[k,],], importance=T)
  bio.rf.pred <- predict(bio.rf,shark[train[k,],])
  error3[k] <- mean((bio.rf.pred-shark[train[k,],'satell'])^2)
}

Error[3] <- mean(error3)

#模型四：Adaboost

library(gbm)

for(k in 1:z){
  bio.boost <- gbm(formula=satell~., data=shark[-train[k,],], distribution='gaussian', n.trees=5000, interaction.depth=4)
  bio.boost.pred <- predict(bio.boost,shark[train[k,],],n.trees=5000)
  error4[k] <- mean((bio.boost.pred-shark[train[k,],'satell'])^2)
}

Error[4] <- mean(error4)

#模型五：泊松回归模型
  
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

#随机森林建模

rm(list=ls())
shark <- read.csv("鲨鱼.csv")

library(randomForest)

bio.rf <- randomForest(formula=satell~., data=shark, importance=T)
importance(bio.rf)
bio.rf.pred <- predict(bio.rf,shark)
error <- mean((bio.rf.pred-shark$satell)^2)

error
bio.rf
