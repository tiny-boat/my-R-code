rm(list=ls()) #清空内存

###Fisher 线性判别由 Fisher 于1936年提出，本程序可用来实现 Fisher 线性判别；
###本程序的编写没有调用外包，完全使用 R 的内置函数实现；
###本程序使用的判别准则 等价于 总体协方差阵相等下的马氏距离判别；
###本程序可能存在未知漏洞，期待进一步的优化

###本程序对 row.data、new.sample 的格式要求如下：
###1.保证二者表头一致，且应满足要求2、要求3中的一条；
###2.第1列存储样品名、第2列存储样品类别信息，其余列存储变量信息；
###3.第1列存储样品类别信息，其余列存储变量信息；
###4.新样本的类别信息空缺即可，但不可无类别信息一列；
###5.如有缺失值或异常值，请先对数据作预处理；
###6.除样品名、样品类别信息、变量信息外不得有其它多余数据。


###一、模型的搭建

DC <- function(row.data,rd.name=TRUE){

	#如果第1列为样品名，删除第1列
	if(rd.name==TRUE){
		data <- row.data[,-1]	
	}else{
		data <- row.data
	}

	group.mean <- aggregate(data,list(class=data$G),mean)[,-1] #组均值 (类别数*(p+1)数据框，去掉第一列是因为第一列是无用的class列)
	total.mean <- apply(data[,-1],2,mean) #总均值 (p维带名称的double型向量)
	p <- ncol(data)-1  #变量数 (1维double型向量)
	G.number <- as.vector(table(data$G))  #各类别频数 (p维integer型向量)

	##1.组内离差平方矩阵 SSE 的计算函数 (使用了 EDEV1 函数)
	SSE <- function(da){
		
		#EDEV1 函数是为 edev 的计算定制的
		EDEV1 <- function(x){
			x[-1] <- x[-1]-as.numeric(subset(group.mean,G==x[1]))[-1]
			#单个样品的离差
			x[-1] %*% t(x[-1])
			#离差列向量与离差行向量的乘积，得到单个样品的离差平方矩阵
		}

		edev <- apply(da,1,EDEV1) 
		#每个样品的离差平方矩阵（实际为1列）构成的 变量数平方*样品数 矩阵
		sse <- apply(edev,1,sum) 
		#每个样品离差平方矩阵 加和而成 组内离差平方矩阵（实际为向量）

		matrix(sse,p,p,byrow=T) #将向量 sse 转换成矩阵

	}

	##2.组间离差平方矩阵 SSA 的计算函数 (使用了 EDEV2 函数)

	SSA <- function(da){

		#EDEV2 函数是为 edev 的计算定制的
		EDEV2 <- function(x){
			x <- as.matrix(x) #数据框不能进行矩阵乘法运算
			x[-1] <- x[-1]-total.mean
			#单个组的离差
			x[-1] %*% t(x[-1])
			#离差列向量与离差行向量的乘积，得到单个组的离差平方矩阵
		}

		edev <- t(t(apply(da,1,EDEV2))*G.number) 
		#每个组的离差平方矩阵（实际为1列）构成的 变量数平方*组数 矩阵
		ssa <- apply(edev,1,sum)
		#每个组的离差平方矩阵 加和而成 组内离差平方矩阵（实际为向量）

		matrix(ssa,p,p,byrow=T) #将向量 ssa 转换成矩阵

	}

	##3.Fisher 线性判别函数的系数向量 (使用了 SSE、SSA 函数)
	
	A <- SSE(data) #组内离差平方矩阵A 
	B <- SSA(group.mean) #组间离差平方矩阵B
	Ai.B <- solve(A) %*% B #组内离差平方矩阵A的逆 与 组间离差平方矩阵B的逆 的乘积
	Eig <- eigen(Ai.B) #矩阵 A^(-1)*B 的所有特征值和特征向量
	
	dcf <- cumsum(Eig$values)*100/(cumsum(Eig$values)[p]) #累积判别力
	
	list("变量数"=p,"样本量"=nrow(row.data),"各类别频数(注意是否存在样本量过少的类别)"=table(data$G),"组均值(组均值间存在显著差异，进行判别分析才有意义)"=group.mean,"总均值"=total.mean,"累积判别力(%，前几个就应该足够大了，否则说明各变量相关性较小)"=dcf,"特征向量(判别函数的系数)"=Eig$vectors)
}

###二、模型的预测

PRDC <- function(row.data,new.sample,rd.name=TRUE,ns.name=TRUE,us.npr=FALSE,cro.val=FALSE){

	if(ns.name==TRUE){
		sample <- new.sample[,-1]
	}else{
		sample <- new.sample
	}
	
	DC <- DC(row.data,rd.name) #建模结果列表
	p <- DC$变量数
	
	# m 为样品矩阵，k 代表第 k 个 Fisher 线性判别函数
	DF <- function(m,k){
		apply(t(m)*DC$特征向量[,k],2,sum) #样品的判别函数值
	}

	##4.Fisher 线性判别结果输出(使用了 DF、LC 函数)

	dfvalue.G <- DF(DC$组均值[,-1],1) #各总体的 第一 判别函数值
	dfvalue.N <- DF(sample[,-1],1) #新样品矩阵的 第一 判别函数值
    
	#样品与哪个总体的判别函数值接近，就判为哪个总体
	LC <- function(x){
		bias <- abs(x-dfvalue.G) #单个样品判别函数值 与 所有总体判别函数值 的 绝对差
		loc <- which(bias==min(bias)) #返回 最小绝对差 所在位置（位置i对应总体i）
		if(length(loc)==1){loc}else{0} 
		#如果所在位置不止1个（表明该判别函数无法区分这些样品），返回0，否则返回原位置
	}

	preG <- sapply(dfvalue.N,LC) #对每个新样品的 第一 判别函数值 应用判别准则函数 得到判别结果

	#对第 mu-1 个判别函数值无法区分的样品 使用第 mu 个判别函数得到判别结果
	
	dcn <- 1
	for(mu in 2:p){
		if(any(preG)==0){
			loc <- which(preG==0) #第 (mu-1)个 判别函数值无法区分的样品序号
			dfvalue.G <- DF(DC$组均值[,-1],mu) #各总体的第 mu 个判别函数值
			dfvalue.N <- DF(sample[loc,-1],mu) #新样品矩阵的第 mu 个判别函数值
			preG[loc] <- sapply(dfvalue.N,LC)
			dcn <- dcn + 1
		}else{
			break #终止循环
		}
	}

	if(us.npr==TRUE){
	
        if(ns.name==TRUE){
			new.sample[,2] <- preG
		}else{
			new.sample[,1] <- preG
		}
		
		new.sample
		
	}else{
	
		errs <- mean(sign(abs(preG-sample$G))) #错分率
		
		if(cro.val==FALSE){
		
			if(ns.name==TRUE){
				rs <- cbind(new.sample[,1],sample,preG)
			}else{
				rs <- cbind(sample,preG)
			} #组合原数据框与判别结果
			
			Con.matrix <- xtabs(~rs$G+rs$preG) #给出判别效果表格
			
			if(all(row.data[,3] == new.sample[,3])==TRUE){
				list("使用判别函数的个数(若个数太多，应重新考虑变量或使用其他判别法)"=dcn,"样本内判别结果"=rs,"样本内混淆矩阵"=Con.matrix,"样本内错分率"=errs)
			}else{
				list("使用判别函数的个数(若个数太多，应重新考虑变量或使用其他判别法)"=dcn,"判别结果"=rs,"混淆矩阵"=Con.matrix,"错分率"=errs)
			}
			
		}else{
			return(errs)
		}	
	}
}


###二、模型的交叉验证错分率

#k折交叉验证误差计算函数（da为数据，k为折次，errfunc为误差计算函数，d为随机种子点）
Cro.val.err <- function(da,k,errfunc,d=1){
	
	#将样本随机分成等大小（或近似等大小）的k份
	n <- nrow(da) #总数据量
	ss1 <- if((round(n/k)-(n/k))>0){round(n/k)-1}else{round(n/k)} #第一部分抽样样本量[n/k]
	ss2 <- ss1+1 #第二部分抽样样本量[n/k]
	k1 <- k-(n-k*ss1) #第一部分抽样样本数
	train <- vector("list",k) #k份样本的样本序号存储列表
	sn <- c(1:n) #待抽样样本序号
	set.seed(d) #设立种子点
	if(k1 == k){
		for(j in 1:(k-1)){
			train[[j]] <- sample(sn,ss1,F) #第j份样本的样本序号，存于train列表的第j个组件
			sn <- setdiff(sn,train[[j]]) #剩余的可抽样本序号
		}
		train[[k]] <- sn
	}else{
		for(j in 1:k1){
			train[[j]] <- sample(sn,ss1,F) #第j份样本的样本序号，存于train列表的第j个组件
			sn <- setdiff(sn,train[[j]]) #剩余的可抽样本序号
		}
		if(k1==k-1){
			train[[k]] <- sn
		}else{
			for(j in (k1+1):(k-1)){
				train[[j]] <- sample(sn,ss2,F) #第j份样本的样本序号，存于train列表的第j个组件
				sn <- setdiff(sn,train[[j]]) #剩余的可抽样本序号
			}
			train[[k]] <- sn	
		}
	}

	#计算k折交叉验证平均误差
	error <- rep(0,k)
	for(i in 1:k){
		trda <- da[-train[[i]],] #训练数据
		teda <- da[train[[i]],] #测试数据
		error[i] <- errfunc(trda,teda) #测试误差
	}
	mean(error)#平均测试误差
}


###三、模型的应用

##1.函数参数设定

row.data <- read.csv("D:\\kaoyan2\\统计学\\0.my note\\判别分析2.csv")#建模样本数据

simu.ns <- row.data[c(4,9,12),]
simu.ns[,1] <- 0
rownames(simu.ns) <- NULL
new.sample <- simu.ns #新样本数据(注：此处为模拟新样本，注意更换)

k <- 8 #折次

#定义误差计算函数，使用PRDC函数，PRDC函数cro.val参数设为T
#务必根据实际情况决定是否对该误差计算函数进行修改
errfunc <- function(trda,teda){
    PRDC(trda,teda,rd.name=FALSE,ns.name=FALSE,cro.val=TRUE)
} 

dc.model <- DC(row.data,rd.name=FALSE) #建模信息列表

cro.val.err <- Cro.val.err(row.data,k,errfunc) #交叉验证平均误差

pred.row.data <- PRDC(row.data,row.data,rd.name=FALSE,ns.name=FALSE) #样本内预测列表, 样品内us.npr=F，为默认参数，无需设置

pred.new.sample <- PRDC(row.data,new.sample,rd.name=FALSE,ns.name=FALSE,us.npr=TRUE) #新样本预测结果, 新样本us.npr=T

##2.结果输出

rs <- dc.model
rs[["交叉验证错分率(模型选择的重要指标)"]] <- cro.val.err
rs[9:12] <- pred.row.data 
attributes(rs)$names[9:12] <- attributes(pred.row.data)$names 
rs$新样本预测结果 <- pred.new.sample
rs$提醒 <- c("输入 preo，查看样本内所有样品的判别结果","输入 pren，查看新样本所有样品的判别结果","输入 look(preo,样品名),查看样本内某个样品的判别结果","输入 look(pren,样品名),查看新样本某个样品的判别结果","输入 other，查看其它信息","注意输入 样品名 时要加双引号，若无样品名列，输入样品序号")  

preo <- rs[[10]] #样本内预测结果
pren <- rs[[13]] #新样本预测结果
other <- rs[c(1,2,5,11:12)]

#查找 样本内(way=preo)/新样本(way=pren) 中某个样品的预测结果
look <- function(way,sn){
	if(colnames(way)[1]=="G"){
		way[sn,]
	}else{
		way[way[,1]==sn,]
	}
}

rs[c(3:4,6:9,14)] #输出列表的部分主要信息