#利用U统计量检验数据分布的对称性(可推广到任意单总体数据)
#基本原理是：若简单随机样本来自一个对称分布，则有P（2X1>X2+X3)-P(2X1<X2+X3)=0
#P（2X1>X2+X3)-P(2X1<X2+X3)是一个自由度为3的可估参数，其核为sgn（2X1-X2-X3)
#由其核构造对称核h（X1，X2，X3)=(1/3)*[sgn（2X1-X2-X3)+sgn（2X2-X1-X3）+sgn（2X3-X1-X2)]
#而这个对称核实际上等于X1，X2，X3的中位数与均值之差的符号函数的1/3
#由该对称核我们可以构造出U统计量，它是n！/[3!*(n-3)!]个对称核的平均
#根据U统计量的性质我们知道，如果数据服从对称分布，当n足够大时，U统计量将近似服从于均值为0的正态分布

rm(list=ls())##清空内存
ExamsymbyUSTAT <- function(mydata,N) #N为U统计量个数,N-1为重抽样次数
{
  USTAT <- NULL #存储U统计量的向量，初值为空

  for(mu in 1:N-1)
  {
    x <- mydata #实际数据
    n1 <- length(x) #实际数据的样本量

    if(mu>1){x <- sample(x,n1,T)} #重抽样

    H <- NULL #存储对称核的向量，初值为空

    for(i in 1:(n1-2))
    for(j in (i+1):(n1-1))
    for(k in (j+1):n1)
    {
      a1 <- sign(2*x[i]-x[j]-x[k])
      a2 <- sign(2*x[j]-x[i]-x[k])
      a3 <- sign(2*x[k]-x[i]-x[j])
      h <- (1/3)*(a1+a2+a3) #计算对称核
      H <- c(H,h)
    }

    Ustat <- (1/choose(length(x),3))*sum(H) #计算U统计量
    USTAT <- c(USTAT,Ustat)
    
  }

  hist(USTAT, border = T, col = "gray") #U统计量直方图

  Umean <- mean(USTAT)
  Uvar <- var(USTAT)
  Pvalue <- 2*pnorm(abs(Umean/sqrt(Uvar/N)),0,1,F)

  if(Pvalue < 0.05)
  {print("有足够证据表明数据不服从对称分布")}
  else{print("没有证据表明数据不服从对称分布")}

  list(Umean = Umean, Uvar = Uvar, Pvalue = Pvalue) #输出n个U统计量的均值和方差 
}

mydata <- c(25,26,28,31,32,32,32,33,35,35,35,36,36,40,41,87)
###数据来自非参数统计第二章例2.1，excel分析显示数据明显偏离正态

ExamsymbyUSTAT(mydata,10000)