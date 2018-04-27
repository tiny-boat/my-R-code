rm(list=ls())

###——————————
###Cheque.sample函数，第一个参数n代表模拟次数，第二个参数is.reselect代表我是否重新选择
###——————————

Cheque.sample <- function(n,is.reselect){

	##1.给定支票代号和真支票代号
	cheque <- c(1:4)
	true.cheque <- sample(cheque,1)

	##2.创建用于存储抽样结果的n行3列空矩阵，矩阵初始元素全为0
	##三列分别存储我第一次抽的支票、主持人抽的支票、我最终抽的支票
	da <- matrix(0,n,3,dimnames=list(c(),c("我第一次抽的支票","主持人抽的支票","我最终抽的支票")))

	##3.进行n次模拟
	for(i in 1:n){
		if(is.reselect==TRUE){
			da[i,] <- sample(cheque,3,replace=FALSE)
			#重新选择时，就是对4张支票作3次不放回随机抽样
		}else{
			a <- sample(cheque,2,replace=FALSE)
			#不重新选择时，就是对4张支票作2次不放回随机抽样
			da[i,] <- c(a,a[1]) 
			#此时，我最终抽的支票就是我第一次抽的支票
		}
	}

	##4.将抽样结果输出为外部文件
	if(is.reselect==TRUE){
		write.csv(da,"D:\\支票抽样结果矩阵(重新选择).csv",row.names=FALSE)
	}else{
		write.csv(da,"D:\\支票抽样结果矩阵(不重新选择).csv",row.names=FALSE)
	}

	##5.计算我的胜率（用“我最终抽的支票”为真支票的数目除以“主持人抽的支票”为假支票的数目）
	win.rate <- sum(da[,3]==true.cheque)/sum(da[,2]!=true.cheque)
	
	##6.将有关结果以列表形式输出
	if(is.reselect==TRUE){
		list("是否重新选择"="是","真支票代号"=true.cheque,"抽样结果矩阵(前6行)"=head(da),"我的胜率"=win.rate,"备注"="全部抽样结果详见D盘根目录下的csv文件")
	}else{
		list("是否重新选择"="否","真支票代号"=true.cheque,"抽样结果矩阵(前6行)"=head(da),"我的胜率"=win.rate,"备注"="全部抽样结果详见D盘根目录下的csv文件")
	}

}

###执行函数
Cheque.sample(100000,FALSE)
Cheque.sample(100000,TRUE)
