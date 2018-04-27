#Bootstrap估计统计量的方差——以中位数为例

conintervalbyboot <- function(mydata,N,n,stat,alpha)
 
#mydata为原始样本数据，N为重抽样次数
#stat为要估计方差的统计量，它是样本（向量x）的函数，须事先给出

{
  x <- mydata

  STATISTICS <- NULL  #自助统计量存储向量，初值为空
  SD.STATISTICS <- NULL  #自助统计量的样本标准差存储向量，初值为空
  SAMPLESIZE <- NULL  #自助统计量的样本量存储向量，初值为空

  for(mu in 1:N) #循环N次，产生N个自助统计量，将它们的标准差作为该统计量标准差的估计
  {
    xr <- sample(x,n,T) 
    #对原始样本数据有放回重抽样，得到一个量为n的新样本

    Statistics <- stat(xr) 
    #计算新样本的统计量
    STATISTICS <- c(STATISTICS,Statistics) 
    #将计算得到的新样本统计量存入自助统计量存储向量

    samplesize <- mu 
    #将自助统计量的样本量mu赋值给samplesize
    SAMPLESIZE <- c(SAMPLESIZE,samplesize) 
    #将samplesize值存入自助统计量的样本量存储向量

    sd.Statistics <- sd(STATISTICS) 
    #计算自助统计量存储向量的标准差
    SD.STATISTICS <- c(SD.STATISTICS,sd.Statistics) 
    #将计算得到的标准差存入自助统计量标准差存储向量
  }
  
  originalstat <- stat(x)
  
  Norm.interval.lower <- originalstat-qnorm(alpha/2,0,1,F)*sd.Statistics
  Norm.interval.upper <- originalstat+qnorm(alpha/2,0,1,F)*sd.Statistics
  Norm.interval <- c(Norm.interval.lower, Norm.interval.upper)
  #基于正态分布的自助置信区间

  Pivotal.interval.lower <- 2*originalstat-quantile(STATISTICS,1-alpha/2)
  Pivotal.interval.upper <- 2*originalstat-quantile(STATISTICS,alpha/2)
  Pivotal.interval <- c(Pivotal.interval.lower, Pivotal.interval.upper)
  #基于枢轴量的自助置信区间

  Quatile.interval.lower <- quantile(STATISTICS,alpha/2)
  Quatile.interval.upper <- quantile(STATISTICS,1-alpha/2)
  Quatile.interval <- c(Quatile.interval.lower, Quatile.interval.upper)
  #基于自助统计量分位数的自助置信区间

  hist(STATISTICS, border = T, col = "gray") 
  #输出循环N次后得到的所有自助统计量的直方图
  plot(SAMPLESIZE,SD.STATISTICS) 
  #输出自助统计量标准差随自助统计量的样本量的变化图
  list(STATISTICS = originalstat, sd.Statistics = sd.Statistics, Norm.interval = Norm.interval, Pivotal.interval = Pivotal.interval, Quatile.interval = Quatile.interval ) 
  #以列表形式显示原始数据统计量值及用自助法估计的该统计量的方差
  
}

mydata <- c(as.numeric(apply(read.table("nerve.txt"),2,as.numeric)))
#读取文本数据并实现对文本数据的强制数值化

stat <- function(x){median(x)} #指定统计量为中位数

#mydata <- c(0.424136656,0.111067449,0.045779206,-0.089951921,-0.838992649,
#-0.416450927,-0.45805788,0.21670288,0.479832185,0.525935001)

conintervalbyboot(mydata,1000,40,stat,0.05) #循环1000次