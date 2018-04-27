rm(list=ls())

library(fBasics)

da <-  read.table("G:\\时间序列分析\\chapter1\\ch1data\\d-mmm-0111.txt",header=T)
mmm <- da[,2]

hist(mmm,nclass=30) #nclass代表分组数

dl <-  density(mmm) #密度估计
range(mmm)

x <- seq(-0.1,0.1,0.001)
y1 <- dnorm(x,mean(mmm),stdev(mmm)) #正态密度

#估计密度与正态密度对比（前提：二者均值、方差相等）
plot(dl$x,dl$y,xlab="rtn",ylab="density",type="l")
lines(x,y1,lty=2)

#加载用于绘制股票条形图（Bar Chart）的脚本
source("G:\\时间序列分析\\chapter1\\ohlc.R")

X <- read.csv("D:\\kaoyan2\\统计学\\0.my note\\其他\\时间序列\\苹果公司\\苹果公司.csv",header=T)
xx <- X[1008:1132,2:5]

ohlc(xx,xl="days",yl="price",title="苹果股票")

#加载用于绘制移动平均图（Moving-average chart）的脚本
source("G:\\时间序列分析\\chapter1\\ma.R")

x1 <- X[767:1244,5]
ma(x1,21) #21为移动平均阶数

#散点图与最小二乘拟合
da <- read.table("G:\\时间序列分析\\chapter1\\ch1data\\m-ibmsp-2611.txt",header=T)
ibm <- log(da$ibm+1)
sp <- log(da$sp+1)
lmcoe <- lm(ibm~sp)$coefficient
plot(sp,ibm,cex=0.8,main="IBM股票与S&P股票月对数收益率散点图及最小二乘拟合")
abline(lmcoe[1],lmcoe[2])