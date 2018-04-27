rm(list=ls())

library(mvstats)

da <- read.csv("D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\互联网.csv",header=T)

rownames(da) <- da[,1] #对行命名
da <- da[,-1] #去掉列名列

##多元数据正态性检验

mean.da <- apply(da,2,mean) #均值

DEV <- function(x){
	x-mean.da
}

dev <- apply(da,1,DEV) #离差矩阵

cov <- (dev %*% t(dev))/(nrow(da)-1) #协方差矩阵
 
icov <- solve(cov) #协方差矩阵逆

STA.dev <- function(x){
	t(x) %*% icov %*% x #马氏距离计算函数
}

or.st <- sort(apply(dev,2,STA.dev)) #马氏距离顺序统计量

per.tr <- function(x){
	(which(or.st==x)-0.5)/nrow(da) #实际分位数计算函数
}

pertr <- sapply(or.st,per.tr) #实际分位数

perth <- pchisq(or.st,ncol(da)) #理论分为数

png("D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\基于卡方统计量的QQ图.png",height=768,width=1024,pointsize=18)

plot(perth,pertr,xlab="理论分位数",ylab="样本分为数",main="基于卡方统计量的 Q-Q 图")

lines(seq(-0.1,1.1,0.01),seq(-0.1,1.1,0.01),lty=2)

dev.off()


##数据预处理

da[,7] <- 1/da[,7] #极小指标极大化
da <- scale(da[,-1]) #标准化处理
da <- as.data.frame(da)


##多元正态性检验未通过，使用主成分法估计因子

RS2 <- factpc(da,2,rot="varimax") #基于方差最大的因子旋转

fac.var.de <- RS2$Vars #方差贡献率
write.csv(fac.var.de,"D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\方差贡献率.csv")

fac.lo <- RS2$loadings #因子载荷

write.csv(fac.lo,"D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\因子载荷矩阵.csv")

fac.sc <- RS2$scores #因子得分
write.csv(fac.sc,"D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\因子得分.csv")

fac.so <- RS2$Rank 

fac.ra <- fac.so[order(fac.so[,2]),] #排序结果
write.csv(fac.ra,"D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\排序结果.csv")

par(mfcol=c(1,1))

png("D:\\kaoyan2\\统计学\\0.my note\\其他\\多元统计\\因子得分图.png",height=768,width=1024,pointsize=18)

plot.text(RS2$scores) #因子得分图

dev.off()
biplot(RS2$scores,RS2$loading) #信息重叠图

list("方差贡献率"=fac.var.de,"因子载荷"=fac.lo,"因子得分"=fac.sc,"样品排序"=fac.ra)

