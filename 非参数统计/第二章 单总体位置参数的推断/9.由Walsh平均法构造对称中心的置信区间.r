##利用Walsh平均值构造对称中心的置信区间
##因为(n+1)n/2个Walsh平均值的中位数是对称中心的一个估计
##因此可以应用Walsh平均值的顺序统计量构造对称中心的置信区间
##它的原理与直接用样本的顺序统计量构造对称中心（中位数）的置信区间一致，因而程序只需做适当修改
##结果显示使用Walsh平均法构造的置信区间较直接用顺序统计量构造的置信区间精确

##data为原始数据，alpha为1-置信度，quatile对应所需构造的quatile分位数的置信区间

conintervalbywalsh <- function(data,alpha){ 

  walsh <- NULL
  N <- length(data)
  for(i in 1:N)
  for(j in i:N)
  {
    a <- (data[i]+data[j])/2  ###求Walsh平均值，共n(n+1)/2个
    walsh <- c(walsh,a)
  }

  hist(walsh,border=T,col="gray")    

  oralwalsh <- function(x)
  {
    a <- sort(walsh)          ###函数oralwalsh用来返回秩为x的数据
    a[x]
  }
  
  n <- length(walsh)

  for(k in 1:n) ###计算所有由次序统计量构成的对称区间，返回其中置信度大于等于1-alpha的区间
  {
    conf <- pbinom(n-k,n,0.5)-pbinom(k-1,n,0.5) ###计算置信度
    lower <- k ###置信下限统计量的秩
    upper <- n-k+1 ###置信上限统计量的秩
    coninterval <- c(oralwalsh(lower),oralwalsh(upper)) ###由oralwalsh函数反求原始数据，得到置信区间
    if(conf>=1-alpha) 
    print(coninterval) ###输出置信度符合要求的对称置信区间  
  }

}

data <- c(9.5, 14, 12, 21, 7.5, 9.5, 2, 17.5, 7.5, 14, 17.5, 24, 26, 19.5, 5.5,
1, 27, 16, 25, 14, 19.5, 22.5, 3, 4, 22.5, 11, 5.5)

conintervalbywalsh(data,0.05)