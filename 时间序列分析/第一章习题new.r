rm(list=ls()) #清空内存

##函数创建（共7个，其中第7个函数的编写使用了第1、2、4个函数，第5、6个函数的编写使用了第3个函数）

#1.偏度计算函数
Ske <- function(da){
    n <- length(da)
	mea <- mean(da)
	dev <- sd(da)
	sum((da-mea)^3)/((n-1)*dev^3)
}

#2.超额峰度计算函数
Kur <- function(da){
    n <- length(da)
	mea <- mean(da)
	dev <- sd(da)
	sum((da-mea)^4)/((n-1)*dev^4)-3
}

#3.双侧检验中标准正态统计量的p值计算函数
Snorm.p <- function(stat,n=4,sci=F){
    format(round(2*min(1-pnorm(stat),pnorm(stat)),n),scientific=sci)
} #默认保留4位小数，且不使用科学计数形式

#4.t检验函数
Ttefunc <- function(da,ob){
    Ttest <- t.test(da)
    Ttep <- format(round(Ttest$p.value,4),scientific=F)
	#p值保留4位小数且不使用科学计数法
	ifelse(Ttep < 0.05,paste("p值","=",Ttep,",","在5%的显著性水平下，认为",ob,"的均值不等于0"),paste("p值","=",Ttep,",","在5%的显著性水平下，没有证据表明",ob,"的均值不等于0"))
}

#5.偏度检验函数
Stefunc <- function(da,ob){
	ske <- Ske(da)
	n <- length(da)
	stat <- ske/sqrt(6/n)
	Step <- Snorm.p(stat)
	ifelse(Step < 0.05,paste("p值","=",Step,",","在5%的显著性水平下，认为",ob,"的偏度不等于0"),paste("p值","=",Step,",","在5%的显著性水平下，没有证据表明",ob,"的偏度不等于0"))
}

#6.峰度检验函数
Ktefunc <- function(da,ob){
	kur <- Kur(da)
	n <- length(da)
	stat <- kur/sqrt(24/n)
	Ktep <- Snorm.p(stat)
	ifelse(Ktep < 0.05,paste("p值","=",Ktep,",","在5%的显著性水平下，认为",ob,"的峰度不等于3"),paste("p值","=",Ktep,",","在5%的显著性水平下，没有证据表明",ob,"的峰度不等于3"))
}

#统计指标计算函数
Statfunc <- function(da){
    c(mean(da),sd(da),Ske(da),Kur(da),min(da),max(da))
}

#7.第一、二问所用函数
Myfunc <- function(data1,number){
    data1 <- data1[,-1] #去掉第1列：日期
    data2 <- log(data1+1) #将 简单收益率 转化为 对数收益率

	a <- names(data1) #将数据框列名赋值给a
	coln <- c("均值","标准差","偏度","超额峰度","最小值","最大值")

	n <- nrow(data1)

	#第1问：简单收益率各项统计指标计算
	result1 <- apply(data1,2,Statfunc)
    rownames(result1) <- coln
    colnames(result1) <- c(a[1],a[2],a[3],a[4])

	#第2问：对数收益率各项统计指标计算
	result2 <- apply(data2,2,Statfunc)
    rownames(result2) <- coln
    colnames(result2) <- c(paste("log(",a[1],"+1",")"),paste("log(",a[2],"+1",")"),paste("log(",a[3],"+1",")"),paste("log(",a[4],"+1",")"))

	#第3问：对数收益率均值的t检验
	da <- data2[,1]
	ob <- ifelse(number == 1,"AXP股票对数收益率","GE股票对数收益率")
	result3 <- Ttefunc(da,ob)

	#结果输出
	list("题号"=paste("第",number,"题"),"简单收益率的统计指标"=result1,"对数收益率的统计指标"=result2,"对数收益率的均值检验结果"=result3)
}


##第一题（使用函数7）
da1 <- read.table("F:\\时间序列分析\\chapter1\\ch1data\\d-axp3dx-0111.txt",header=T) #载入数据
nu1 <- c(1) #题号
R1 <- Myfunc(da1,nu1) #将第一题结果保存到对象R1中

##第二题（使用函数7）
da <- read.table("F:\\时间序列分析\\chapter1\\ch1data\\m-ge3dx-40112.txt",header=T) #载入数据
da$sp <- as.numeric(as.character(da$sp)) #将因子型sp变量强制转换为数值型变量
da2 <- da
nu2 <- c(2)
R2 <- Myfunc(da2,nu2) #将第二题结果保存到对象R2中

##第三题（使用函数4、5、6）
da <- da2[,5]
ob <- c("S&P月股票收益率")
rs1 <- Ttefunc(da,ob)
rs2 <- Stefunc(da,ob)
rs3 <- Ktefunc(da,ob)
R3 <- list("题号"=paste("第","3","题"),"S&P月股票收益率均值检验结果"=rs1,"S&P月股票收益率偏度检验结果"=rs2,"S&P月股票收益率峰度检验结果"=rs3)#将第三题结果保存到对象R3中

##第四题（使用函数5、6）
da <- log(da1[,2]+1)
ob <- c("运通公司(AXP)对数收益率")
rs1 <- Stefunc(da,ob)
rs2 <- Ktefunc(da,ob)
R4 <- list("题号"=paste("第","4","题"),"运通公司(AXP)对数收益率偏度检验结果"=rs1,"运通公司(AXP)对数收益率峰度检验结果"=rs2)#将第四题结果保存到对象R3中

##所有结果输出
R1
R2
R3
R4