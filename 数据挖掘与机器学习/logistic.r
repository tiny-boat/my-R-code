rm(list=ls())##����ڴ�
library(glmpath)
data(heart.data)
attach(heart.data)
data=data.frame(cbind(as.matrix(heart.data$x),y))#�������ݿ�
table(data$y)#ͳ��Ƶ��

pairs(data)#������ɢ��ͼ�������Ա������ϵ������

#���������������������ڲ�ͬ���Ա����ϵĶԱȣ�����������붨���Ա������ϵ������

attach(data)
W1 <- xtabs(~y+famhist)
W1
chisq.test(W1)

#����ͼ����(�����������ڲ�ͬ���������ϵĶԱȣ�����������붨���Ա������ϵ����)

par(mfrow=c(2,4))
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")
boxplot(sbp~y,main="sbp")

#�����ȡ80%��������Ϊѵ������������Ϊ���Լ�

set.seed(9)
n <- nrow(data)
ss <- sample(n,n*0.8,replace=FALSE)
datatr <- data[ss,]
datate <- data[-ss,]

#��ѵ�������ݹ���logistic�ع�ģ�͡�probit�ع�ģ�͡��𲽻ع�ģ�ͣ�AIC/BIC��

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

##��4�����ߣ�����ģ��ѡ��

#��6��ģ�͵ĸ���Ԥ��ֵ
attach(datate)
p1 <- matrix(0,length(datate[,1]),6) #����n*0.2��6�пվ���,��������ģ�͵�Ԥ����
p1[,1] <- predict(logit,datate)
p1[,2] <- predict(probit,datate)
p1[,3] <- predict(AICl,datate)
p1[,4] <- predict(AICp,datate)
p1[,5] <- predict(BICl,datate)
p1[,6] <- predict(BICp,datate)

p1 <- exp(p1)/(1+exp(p1))
p <- as.data.frame(p1)
names(p) <- c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit")
y.true <- datate$y #���Լ��б���y��ֵ��y.true

#100����ֵ��6��ģ��ѡ��ĳ�ʼ��
ngrid <- 100
FPR <- rep(0,ngrid) #����Ϊngrid�Ŀ�����
Sensitivity <- rep(0,ngrid)
TPR <- rep(0,ngrid) #����Ϊngrid�Ŀ�����
Specificity <- rep(0,ngrid)
Youden <- rep(0,ngrid)
Wrong <- rep(0,ngrid)
Crivalue1 <- rep(0,6)
Crivalue2 <- rep(0,6)
a <- seq(0.01,1,0.01) #��ֵ

par(mfrow=c(2,2),cex=0.4,mai=c(0.9,0.8,0.3,0.1))

#ͼ1��ROC���ߣ��������ǡ�1-��һ������롰1-�ڶ�����󡱵��໥��ϵ���������һ�������ΪԭΪ1��Ԥ��Ϊ0���ڶ��������ΪԭΪ0��Ԥ��Ϊ1.
plot(c(0,1),c(0,1),type="l",xlab="FPR",ylab="TPR")
#��Բ�ͬģ�͵Ĵ�ѭ��
for(k in 1:6){
  prob <- p[,k] #�ɵ�k��ģ�����õ�y�ĸ���Ԥ��ֵ��6��ģ�ͣ�6�δ�ѭ����
  #���ͬһģ�Ͳ�ͬ��ֵ��Сѭ����1000����ֵ��1000��Сѭ����
  for(i in 1:ngrid){
    p0 <- i/ngrid #��ֵ
    y.hat <- 1*(prob > p0) #������ģ�͵õ���y��Ԥ��ֵ
    TPR[i] <- sum(y.true*y.hat)/sum(y.true) #ԭΪ1��Ԥ��Ϊ1�ĸ��ʣ�True Positive Rate��
    FPR[i] <- sum((1-y.true)*y.hat)/sum(1-y.true) #ԭΪ0��Ԥ��Ϊ1�ĸ��ʣ�False Positive Rate��
  }
  points(FPR,TPR,type="b",col=k,lty=k,pch=k) #ROC�������
}
legend("bottomright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

#ͼ2�����������ߣ��������ǡ�1-��һ������桰��ֵ���ı仯��ϵ��
plot(c(0,1),c(0,1),type="l",xlab="Critical value",ylab="Sensitivity",col="white")
#��Բ�ͬģ�͵Ĵ�ѭ��
for(k in 1:6){
  prob <- p[,k] #�ɵ�k��ģ�����õ�y�ĸ���Ԥ��ֵ��6��ģ�ͣ�6�δ�ѭ����
  #���ͬһģ�Ͳ�ͬ��ֵ��Сѭ����1000����ֵ��1000��Сѭ����
  for(i in 1:ngrid){
    p0 <- i/ngrid #��ֵ
    y.hat <- 1*(prob > p0) #������ģ�͵õ���y��Ԥ��ֵ
    TPR[i] <- sum(y.true*y.hat)/sum(y.true)
    Sensitivity[i] <- TPR[i] #������ָ��=TPR
  }
  points(a,Sensitivity,type="b",col=k,lty=k,pch=k) #Sensitivity������� 
}
legend("topright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

#ͼ3������ȶ����ߣ��������ǡ�1-�ڶ�������桰��ֵ���ı仯��ϵ��
plot(c(0,1),c(0,1),type="l",xlab="Critical value",ylab="Specificity",col="white")
#��Բ�ͬģ�͵Ĵ�ѭ��
for(k in 1:6){
  prob <- p[,k] #�ɵ�k��ģ�����õ�y�ĸ���Ԥ��ֵ��6��ģ�ͣ�6�δ�ѭ����
  #���ͬһģ�Ͳ�ͬ��ֵ��Сѭ����1000����ֵ��1000��Сѭ����
  for(i in 1:ngrid){
    p0 <- i/ngrid #��ֵ
    y.hat <- 1*(prob > p0) #������ģ�͵õ���y��Ԥ��ֵ
    FPR[i] <- sum((1-y.true)*y.hat)/sum(1-y.true)
    Specificity[i] <- 1-FPR[i] #�����ָ��=1-FPR
  }
  points(a,Specificity,type="b",col=k,lty=k,pch=k) #Specificity�������
}
legend("bottomright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

#ͼ4��Լ�����ߣ��������ǡ�1-��������桰��ֵ���ı仯��ϵ�����ɸ����߿ɵø�ģ�͵�������ֵ�����߶����Ӧ��ֵ��
plot(c(0,1),c(0,1),type="l",ylim=c(0,0.6),xlab="Critical value",ylab="youden",col="white")
#��Բ�ͬģ�͵Ĵ�ѭ��
for(k in 1:6){
  prob <- p[,k] #�ɵ�k��ģ�����õ�y�ĸ���Ԥ��ֵ��6��ģ�ͣ�6�δ�ѭ����
  #���ͬһģ�Ͳ�ͬ��ֵ��Сѭ����1000����ֵ��1000��Сѭ����
  for(i in 1:ngrid){
    p0 <- i/ngrid #��ֵ
    y.hat <- 1*(prob > p0) #������ģ�͵õ���y��Ԥ��ֵ
    TPR[i] <- sum(y.true*y.hat)/sum(y.true)
    Sensitivity[i] <- TPR[i] #������ָ��=TPR (1-����һ�����)
    FPR[i] <- sum((1-y.true)*y.hat)/sum(1-y.true)
    Specificity[i] <- 1-FPR[i] #�����ָ��=1-FPR ��1-���ڶ������
    Youden[i] <- Sensitivity[i]+Specificity[i]-1 #Լ��ָ�� ��1-����������ܺͣ�
    Wrong[i] <- mean(y.true!=y.hat) #�����
  }
  Crivalue1[k] <- order(Wrong)[1]/ngrid #������Ԥ�⾫�����k��ģ�͵�������ֵ
  Crivalue2[k] <- order(-Youden)[1]/ngrid #��Լ��ָ�����k��ģ�͵�������ֵ
  points(a,Youden,type="b",col=k,lty=k,pch=k) #youden������� 
}
legend("topright",c("logit","probit","AIClogit","AICprobit","BIClogit","BICprobit"),lty=c(1:6),col=c(1:6),pch=c(1:6))

Crivalue1
Crivalue2

#ѡ���3��ģ�ͣ���ֵȡ������ֵ0.556

p0 <- 0.556 #��ֵ
prob <- p[,3]
y.hat <- 1*(prob > p0)
right <- 1-mean(y.true!=y.hat)#��ȷ��
table(as.data.frame(cbind(y.hat,y.true)))
