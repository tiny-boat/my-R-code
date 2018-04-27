#非参数统计第一章实验1——经验分布的置信区间
library(MASS) #加载MASS程序包
data(geyser) #加载数据geyser

duration <- geyser[,2] #将geyser数据集中第2列数据赋给duration变量
duration.sort <- sort(duration) #对duration变量内元素做升序排列
duration.rank <- rank(duration.sort) #求已排序的duration变量内各元素的秩
duration.cdf <- duration.rank/length(duration) #求duration变量的经验分布函数值

plot(duration.sort, duration.cdf) #画duration变量（x）与经验分布函数值（y）的二维散点图
N <- length(duration) #将duration变量长度赋给变量N
segments(duration.sort[1:(N-1)], duration.cdf[1:(N-1)],
duration.sort[2:N], duration.cdf[1:(N-1)]) 
#通过连点成线段，最终完成duration变量经验分布函数图形的绘制

alpha <- 0.5 #给定经验分布函数置信区间的置信度为95%，将0.5赋值给alpha
band <- sqrt(1/(2*N))*log(2/alpha) #求经验分布函数半个置信区间的长度，并将所求值赋给band
Lower.95 <- duration.cdf - band #求经验分布函数的置信下限，并将所求值付给Lower.95
Upper.95 <- duration.cdf + band #求经验分布函数的置信上限，并将所求值付给Upper.95
lines(duration.sort, Lower.95, lty=2) #在原经验分布函数图形上画出置信下限（虚线）
lines(duration.sort, Upper.95) #在原经验分布函数图形上画出置信上限（实线）

alpha2 <- seq(0.01, 0.1, by=0.001) #以0.01为初始值，0.1为最大值，0.001为步长生成等差数列将该数列赋给alpha
band2 <- 2*sqrt(1/(2*N))*log(2/alpha2) #将置信区间长度赋给band2#
plot(alpha2,band2)#画（1-置信度，即alpha）与置信区间宽度之间的关系