rm(list=ls())##清空内存
library(glmpath)
data(heart.data)
attach(heart.data)
data=data.frame(cbind(as.matrix(heart.data$x),y))#创建数据框
table(data$y)#统计频数

pairs(data)#画矩阵散点图（定量自变量间关系分析）

#列联分析（两定性总体在不同定性变量上的对比，定性因变量与定性自变量间关系分析）

attach(data)
W1 <- xtabs(~y+famhist)
W1
chisq.test(W1)

#箱线图分析(两定性总体在不同定量变量上的对比，定性因变量与定量自变量间关系分析)

par(mfrow=c(2,4))
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")

#随机抽取80%的样本作为训练集，其余作为测试集

set.seed(9)
n <- nrow(data)
ss <- sample(n,n*0.8,replace=FALSE)
datatr <- data[ss,]
datate <- data[-ss,]

#由训练集数据构建logistic回归模型、probit回归模型、逐步回归模型（AIC/BIC）

logit <- glm(y~.,data=datatr,family=binomial)
summary(logit)

probit <- glm(y~.,data=datatr,family=binomial(link=probit))
summary(probit)

AICl <- step(logit,trace=0)
summary(AICl)

AICp <- step(probit,trace=0)
summary(AICp)

BICl <- step(logit,k=log(n),trace=0)
summary(BICl)

BICp <- step(probit,k=log(n),trace=0)
summary(BICp)

##画4条曲线，进行模型选择

#求6个模型的概率预测值
attach(datate)
p1 <- matrix(0,length(datate[,1]),6) #创建n*0.2行6列空矩阵,储存六个模型的预测结果
p1[,1] <- predict(logit,datate)
p1[,2] <- predict(probit,datate)
p1[,3] <- predict(AICl,datate)
p1[,4] <- predict(AICp,datate)
p1[,5] <- predict(BICl,datate)
p1[,6] <- predict(BICp,datate)

p1 <- exp(p1)/(1+exp(p1))
p <- as.data.frame(p1)
names(p) <- c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit")
y.true <- datate$y #测试集中变量y赋值给y.true

#100个阈值，6个模型选择的初始化
ngrid <- 100
FPR <- rep(0,ngrid) #长度为ngrid的空向量
Sensitivity <- rep(0,ngrid)
TPR <- rep(0,ngrid) #长度为ngrid的空向量
Specificity <- rep(0,ngrid)
Youden <- rep(0,ngrid)
Wrong <- rep(0,ngrid)
Crivalue1 <- rep(0,6)
Crivalue2 <- rep(0,6)
a <- seq(0.01,1,0.01) #阈值

par(mfrow=c(2,2),cex=0.4,mai=c(0.9,0.8,0.3,0.1))

#图1：ROC曲线（本质上是“1-第一类错误”与“1-第二类错误”的相互关系），这里第一类错误定义为原为1，预测为0；第二类错误定义为原为0，预测为1.
plot(c(0,1),c(0,1),type="l",xlab="FPR",ylab="TPR")
#针对不同模型的大循环
for(k in 1:6){
  prob <- p[,k] #由第k个模型所得的y的概率预测值（6个模型，6次大循环）
  #针对同一模型不同阈值的小循环（1000个阈值，1000次小循环）
  for(i in 1:ngrid){
    p0 <- i/ngrid #阈值
    y.hat <- 1*(prob > p0) #由所建模型得到的y的预测值
    TPR[i] <- sum(y.true*y.hat)/sum(y.true) #原为1，预测为1的概率（True Positive Rate）
    FPR[i] <- sum((1-y.true)*y.hat)/sum(1-y.true) #原为0，预测为1的概率（False Positive Rate）
  }
  points(FPR,TPR,type="b",col=k,lty=k,pch=k) #ROC曲线描点
}
legend("bottomright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

#图2：灵敏度曲线（本质上是“1-第一类错误”随“阈值”的变化关系）
plot(c(0,1),c(0,1),type="l",xlab="Critical value",ylab="Sensitivity",col="white")
#针对不同模型的大循环
for(k in 1:6){
  prob <- p[,k] #由第k个模型所得的y的概率预测值（6个模型，6次大循环）
  #针对同一模型不同阈值的小循环（1000个阈值，1000次小循环）
  for(i in 1:ngrid){
    p0 <- i/ngrid #阈值
    y.hat <- 1*(prob > p0) #由所建模型得到的y的预测值
    TPR[i] <- sum(y.true*y.hat)/sum(y.true)
    Sensitivity[i] <- TPR[i] #灵敏度指数=TPR
  }
  points(a,Sensitivity,type="b",col=k,lty=k,pch=k) #Sensitivity曲线描点 
}
legend("topright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

#图3：特异度度曲线（本质上是“1-第二类错误”随“阈值”的变化关系）
plot(c(0,1),c(0,1),type="l",xlab="Critical value",ylab="Specificity",col="white")
#针对不同模型的大循环
for(k in 1:6){
  prob <- p[,k] #由第k个模型所得的y的概率预测值（6个模型，6次大循环）
  #针对同一模型不同阈值的小循环（1000个阈值，1000次小循环）
  for(i in 1:ngrid){
    p0 <- i/ngrid #阈值
    y.hat <- 1*(prob > p0) #由所建模型得到的y的预测值
    FPR[i] <- sum((1-y.true)*y.hat)/sum(1-y.true)
    Specificity[i] <- 1-FPR[i] #特异度指数=1-FPR
  }
  points(a,Specificity,type="b",col=k,lty=k,pch=k) #Specificity曲线描点
}
legend("bottomright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

#图4：约登曲线（本质上是“1-两类错误”随“阈值”的变化关系），由该曲线可得各模型的最优阈值（曲线顶点对应阈值）
plot(c(0,1),c(0,1),type="l",ylim=c(0,0.6),xlab="Critical value",ylab="youden",col="white")
#针对不同模型的大循环
for(k in 1:6){
  prob <- p[,k] #由第k个模型所得的y的概率预测值（6个模型，6次大循环）
  #针对同一模型不同阈值的小循环（1000个阈值，1000次小循环）
  for(i in 1:ngrid){
    p0 <- i/ngrid #阈值
    y.hat <- 1*(prob > p0) #由所建模型得到的y的预测值
    TPR[i] <- sum(y.true*y.hat)/sum(y.true)
    Sensitivity[i] <- TPR[i] #灵敏度指数=TPR (1-犯第一类错误)
    FPR[i] <- sum((1-y.true)*y.hat)/sum(1-y.true)
    Specificity[i] <- 1-FPR[i] #特异度指数=1-FPR （1-犯第二类错误）
    Youden[i] <- Sensitivity[i]+Specificity[i]-1 #约登指数 （1-犯两类错误总和）
    Wrong[i] <- mean(y.true!=y.hat) #错分率
  }
  Crivalue1[k] <- order(Wrong)[1]/ngrid #由总体预测精度求第k个模型的最优阈值
  Crivalue2[k] <- order(-Youden)[1]/ngrid #由约登指数求第k个模型的最优阈值
  points(a,Youden,type="b",col=k,lty=k,pch=k) #youden曲线描点 
}
legend("topright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

Crivalue1
Crivalue2

#选择第3个模型，阈值取最优阈值0.556

p0 <- 0.556 #阈值
prob <- p[,3]
y.hat <- 1*(prob > p0)
right <- 1-mean(y.true!=y.hat)#正确率
table(as.data.frame(cbind(y.hat,y.true)))
