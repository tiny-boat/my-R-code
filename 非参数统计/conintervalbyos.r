##利用顺序统计量(OS)构造分位数的置信区间
##以中位数的置信区间为例
##这是一种不依赖于分布的统计推断方法
##在分布中位数之前至少有i个样本点、至多有j-1个样本点的概率的计算只用到二项分布和中位数对应累积概率0.5
##寻找使这个概率大于等于95%的对应第i个顺序统计量、第j个顺序统计量
##在这些得到的区间中寻找置信度最接近95%的对称区间作为最终的结果


##data为原始数据，alpha为1-置信度，quatile对应所需构造的quatile%分位数的置信区间

conintervalbyos <- function(data,alpha,quatile){ 

  oraldata <- function(x)
  {
    a <- sort(data)          ###函数oraldata用来返回秩为x的数据
    a[x]
  }
  
  n <- length(data)

  for(k in 1:n) ##计算所有由次序统计量构成的对称区间，返回其中置信度大于等于1-alpha的区间
  {
    conf <- pbinom(n-k,n,quatile)-pbinom(k-1,n,quatile) #计算置信度
    lower <- k #置信下限统计量的秩
    upper <- n-k+1 #置信上限统计量的秩
    coninterval <- c(oraldata(lower),oraldata(upper)) ##由oraldata函数反求原始数据，得到置信区间
    if(conf>=1-alpha) 
    print(coninterval) #输出置信度符合要求的对称置信区间  
  }

}
