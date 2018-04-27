rm(list=ls()) #清空内存

##一、数据导入

da <- read.csv("D:\\kaoyan2\\统计学\\0.my note\\综合评价.csv")
rownames(da) <- da[,1] #将第一列作为行名
da <- da[,-1] #去掉第一列


##二、数据指标极大化与数据标准化

da[,2] <- max(da[,2])-da[,2] #极小指标极大化
p <- ncol(da) #变量数

STA <- function(x){
	(x-mean(x))/(sqrt(5/6)*sd(x)) 
	#注：此处遵循教材，教材上使用的是总体标准差
}

A <- apply(da,2,STA) #数据标准化


##三、求相关系数矩阵，判断目标函数是否为凸函数（对二次函数，转化为判断Hessian矩阵的正定性）

H <- t(A) %*% A #相关系数矩阵
Hessian <- 2*H #Hessian 矩阵为二次型矩阵的2倍
Eig.He <- eigen(Hessian) #Hessian 矩阵的特征值与特征向量，用来判定矩阵正定性


##四、综合评价

#评价函数，da为数据，W为权重
SORT <- function(da,W){
	W <- W/sum(W)
	SUM <- function(x){
		sum(t(x) * W)
	}
	va <- apply(da,1,SUM)
	rank(-va)
}

#目标函数计算函数
OBF <- function(W){
	W <- W/sum(W)
	as.vector(round(t(W)%*%H%*%(W),3))
}

#数据及权重的导入
da <- A
W <- read.csv("D:\\kaoyan2\\统计学\\0.my note\\综合评价权重（来自matlab）.csv",header=F)
W1 <- W[,1] #由 matlab 中的 fmincon 函数求得的 带入主观信息 的 拉开档次法 的 权重
W2 <- W[,2] #由 matlab 中的 fmincon 函数求得的 拉开档次法 的 权重
W3 <- W[,3] #课本上给出的 带入主观信息 的 拉开档次法 的 权重
W4 <- W[,4] #课本上给出的 拉开档次法 的权重

#评价结果输出
list("方法"="带入主观信息 的 拉开档次法","权重"=round(W1,3),"排序结果"=SORT(da,W1),"目标函数值"=OBF(W1))
list("方法"="带入主观信息 的 拉开档次法（课本）","权重"=W3,"排序结果"=SORT(da,W3),"目标函数值"=OBF(W3))

list("方法"="拉开档次法","权重"=round(W2,3),"排序结果"=SORT(da,W2),"目标函数值"=OBF(W2))
list("方法"="拉开档次法（课本）","权重"=W4,"排序结果"=SORT(da,W4),"目标函数值"=OBF(W4))