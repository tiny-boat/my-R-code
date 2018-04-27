rm(list=ls())##清空内存

n <- 100 #指定向量长度

a <- runif(n,0,100) #生成n个最小值为0，最大值为100均匀随机数，构成向量a
b <- runif(n,0,100) #生成n个最小值为0，最大值为100的均匀随机数，构成向量b

#下面的步骤是将向量a，b转换为不相关的向量。想法是把a，b变成相互正交的向量，因为正交向量不相关。
#为此可以保持a不变，然后转换b，通过改变向量b中的1个元素，就可以使得向量a和向量b正交
#具体操作是：假设要改变向量b中第k个元素的值，可以把第k个元素设为一个待定系数x
#由条件a，b正交（a,b内积=0）即可构造关于x的一个等式，解出x即可

c <- a*b #向量a，b对应位置元素相乘，构成向量c，这一步为计算内积作准备

B <- matrix(0,n,n) #生成一个n×n的空矩阵，用于存储b的n个转换向量
Cor <- rep(0,n) #生成一个长度为n的空向量，用于存储a和n个b的转换向量计算得到的n个相关系数

#下面使用for循环，是对b中每一个位置的元素改变一次，得到n个与a近似正交的向量
#然后找其中与a的相关系数最接近0的那个向量作为对b的最终转换结果
#之所以使用循环，是因为程序是数值运算，很难得到一个与a完全正交的向量
#因而也就很难得到一个与a完全线性无关的向量，只得从中找一个相关性最小的向量

for(k in 1:n){
  b0 <- b  #将向量b赋值给b0
  d <- c[-k] #去掉c向量的第k个元素，构成向量d
  b0[k] <- (0-sum(d))/a[k] #改变向量b中第k个元素
  B[k,] <- b0 #将第k个b的转换向量赋值给矩阵B的第k行
  Cor[k] <- cor(a,B[k,]) #计算第k个b的转换向量与a的相关系数，将其作为向量Cor的第k个元素
}

#寻找n个转换向量中与a相关性最小的向量并将其作为b的最终转换向量
Minloc <- which(abs(Cor)==min(abs(Cor))) 
Minloc
b <- B[Minloc,]

#输出a和新的b，以及它们的相关系数
a
b
cor(a,b)