#Kolmogorov-Smirnov Goodness-of-fit Test 科莫戈洛夫—斯米洛夫拟合优度检验
#Dn是Kolmogorov统计量，定义为经验分布函数与总体累积分布函数之差的上确界
#注意：以下检验由于使用了极限分布，不适合于小样本

Kolmogtest <- function(data,CDF,alpha) ##CDF为待检验的假设的总体分布cdf,alpha为显著性水平
{
  datarank <- rank(data,ties.method="max") ##求数据的秩,重复数据取最大秩
  datasize <- length(data)
  EDF <- datarank/datasize ##计算经验分布函数
  D <- max(abs(EDF-CDF(data)))
  Dn <- sqrt(datasize)*D

  ##计算Kolmogorov统计量

  Koldis <- function(x) ##Kolmogorov统计量的极限累积分布函数
  {
    DIS <- 0
    for(j in 1:100000) ##事实上这是一个无穷求和，这里用100000近似
    {
      dis <- (-1)^(j-1) * exp(-2 * j^2 * x^2)
      DIS <- DIS + dis
    }
    koldis <- 1-2*DIS  ##Kolmogorov统计量极限分布表达式参见本科统计学英文教材最后一章的内容
  }

  Pvalue <- 1-Koldis(Dn)
  if(Pvalue < alpha){result <- c(c("有足够证据认为数据不服从假设的分布"),c("检验的显著性水平为，或者说拒绝原假设，认为数据不服从假设分布的犯错概率不大于"),c(alpha))}
  else{result <- c(c("没有足够证据认为数据不服从假设的分布"),c("检验的显著性水平为"),c(alpha))}
  list("样本量"=datasize,"经验分布与假设分布最大距离D"=D,"Kolmogorov统计量Dn"=Dn,"P值"=Pvalue,"结论"=result)
}

data <- c(87,77,92,68,80,78,84,77,81,80,80,77,92,86,76,80,81,75,77,72,81,72,84,86,80,68,77,87,76,77,78,92,75,80,78)##给定数据
CDF <- function(x)
{
  pnorm(x,80,6)##定义总体分布为均值80，标准差6的正态分布
}
alpha <- 0.05
Kolmogtest(data,CDF,alpha)

