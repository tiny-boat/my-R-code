rm(list=ls())

##—————————————
##第一部分：股票数据的获取
##—————————————

#1.创建股票数据的下载（load）和输出（output）函数 financial.lo
#financial.name 为股票名称，financial.symbol 为股票代码
#start.time为起始时间，end.time为截止时间，outloc为输出路径

financial.lo <- function(financial.name,financial.symbol,start.time,end.time,outloc){

	#加载“quantmod”程序包（由 Jeffry 开发）	
	#利用该包，可从雅虎财经（yahoo）、谷歌财经（google）、圣路易斯联邦储备银行的联邦储备经济数据库（PRED）下载金融数据
	
	library(quantmod)
	
	#由getSymbols函数下载股票数据
	setSymbolLookup(OBJ = list(name=financial.symbol,src="yahoo",from=start.time,to=end.time))
	getSymbols("OBJ")

	#将股票数据输出为一个csv文件
	da <- as.matrix(OBJ) #将 xts、zoo 对象转换为 matrix 对象
	colnames(da) <- c("开盘价","最高价","最低价","收盘价","成交量","调整后收盘价") #更改原英文列名
	write.csv(da,paste(outloc,financial.name,".csv",sep=""))

}

#2.下载并输出北京银行从2007.09.19到2017.05.04的股票数据，文件保存在D盘北京银行目录下

financial.lo("北京银行","601169.SS","2007-09-19","2017-05-04","D:\\北京银行\\")


##—————————————————————
##第二部分：股票数据的读取及分析前的准备工作
##—————————————————————

library(xts) #加载 xts 包，用于将 data.frame 类型数据转换成 xts/zoo 类型数据

#1.读入股票数据

da <- read.csv("D:\\北京银行\\北京银行.csv",header=T) 

#2.将数据框转换成 xts/zoo 类型并提取第5列（收盘价）数据作为待分析数据

date <- as.character(da[,1]) #将股票数据第一列（时间）转变为字符串类型
da <- xts(da[,5],order.by=as.Date(date,format="%Y/%m/%d")) 

#3.设定图形、文本输出路径

outloc <- "D:\\北京银行\\" 

#4.创建图形输出函数，i为图形标号，name为图形名称的核心部分

PNG <- function(i,name){
	png(paste(outloc,i,".北京银行股票收盘价",name,".png",sep=""),width=1024,height=768,pointsize=18)	
}


##———————————————————————————————————
##第三部分：股票收盘价时序图，纯随机性检验p值图，自相关图，偏自相关图
##———————————————————————————————————

#1.股票收盘价时序图

PNG(1,"时序图")
plot(da,xlab="日期 (date)",ylab="收盘价 (close value)",main="北京银行股票收盘价时序图")
dev.off()

#2.股票收盘价纯随机性检验p值图

Bote.p <- rep(0,12)
lag <- 1:12
for(i in 1:12){
	Bote.p[i] <- Box.test(da,lag=i,type="Ljung")$p.value
}
x <- seq(0,13,0.01)
y <- rep(0.05,length(x))
PNG(2,"纯随机性检验p值图")
plot(lag,Bote.p,xlab="滞后期数 (lag)",ylab="检验p值 (p value)",ylim=c(0,1),main="北京银行股票收盘价纯随机性检验 p 值图")
lines(x,y,lty=2)
dev.off()

#3.股票收盘价自相关图

PNG(3,"自相关图")
acf(da,lag=100,xlab="滞后期数 (lag)",ylab="自相关系数 (ACF)",main="北京银行股票收盘价自相关图")
dev.off()

#4.股票收盘价偏自相关图

PNG(4,"偏自相关图")
pacf(da,lag=100,xlab="滞后期数 (lag)",ylab="偏自相关系数 (PACF)",main="北京银行股票收盘价偏自相关图")
dev.off()


##————————————————————————————————————
##第四部分：平稳性统计检验，由第一部分偏自相关图确定增广单位根检验滞后阶数
##————————————————————————————————————

#股票收盘价单位根检验(基于第三部分的偏自相关图，选择AR(3)模型作扩展单位根检验)

library(tseries)
adt.p <- adf.test(da,"stationary",3)$p.value
if(adt.p >= 0.05){
	ters <- paste("扩展单位根检验p值 = ",adt.p,"，不能拒绝单位根假设，认为序列非平稳")
}else{
	ters <- paste("扩展单位根检验p值 = ",adt.p,"，拒绝单位根假设，认为序列平稳")
}
write.table(ters,paste(outloc,"5.股票收盘价单位根检验结果.txt"))


##———————————————————————————————————————
##第五部分：股票收盘价差分序列时序图、纯随机性检验p值图、自相关图、偏自相关图
##———————————————————————————————————————

#1.一阶差分

dda <- diff(da)[-1]

#2.一阶差分时序图

PNG(6,"差分序列时序图")
plot(dda,xlab="日期 (date)",ylab="收盘价差分 (difference of close value)",main="北京银行股票收盘价差分序列时序图")
dev.off()

#3.一阶差分纯随机性检验p值图

for(i in 1:12){
	Bote.p[i] <- Box.test(dda,lag=i,type="Ljung")$p.value
}
PNG(7,"差分序列纯随机性检验p值图")
plot(lag,Bote.p,xlab="滞后期数 (lag)",ylab="检验p值 (p value)",ylim=c(0,1),main="北京银行股票收盘价差分序列纯随机性检验 p 值图")
lines(x,y,lty=2)
dev.off()

#4.一阶差分自相关图

PNG(8,"差分序列自相关图")
acf(dda,lag=100,xlab="滞后期数 (lag)",ylab="自相关系数 (ACF)",main="北京银行股票收盘价差分序列自相关图")
dev.off()

#5.一阶差分偏自相关图

PNG(9,"差分序列偏自相关图")
pacf(dda,lag=100,xlab="滞后期数 (lag)",ylab="偏自相关系数 (PACF)",ylim=c(-0.2,0.2),main="北京银行股票收盘价差分序列偏自相关图")
dev.off()


##————————————————————————————————————————
##第六部分：差分序列平稳性统计检验，由第五部分偏自相关图确定增广单位根检验滞后阶数
##————————————————————————————————————————

#一阶差分单位根检验

library(tseries)
dadt.p <- adf.test(dda,"stationary",1)$p.value
if(dadt.p >= 0.05){
	dters <- paste("扩展单位根检验p值 = ",dadt.p,"，不能拒绝单位根假设，认为序列非平稳")
}else{
	dters <- paste("扩展单位根检验p值 = ",dadt.p,"，拒绝单位根假设，认为序列平稳")
}
write.table(dters,paste(outloc,"10.股票收盘价差分序列单位根检验结果.txt"))


##—————————————————————
##第七部分：ARIMA 自动定阶及待选模型的确定
##—————————————————————

#1.观察ACF图、PACF图，定ARIMA模型阶数为1,1,0

library(forecast)
pdq <- auto.arima(da)$arma
pdq <- c(pdq[1],1,pdq[2])
names(pdq) <- c("p","d","q")
write.table(pdq,paste(outloc,"11.ARIMA自动定阶函数定阶结果.txt"))

#2.自动识别ARIMA模型阶数为4,1,2
#3.另外增加一个指数平滑模型，其等价于阶数为0,1,1的ARIMA模型


##———————————————
##第八部分：建模与模型充分性检验
##———————————————

#1.建模

ARIMA1 <- arima(da,order=c(1,1,0))
ARIMA2 <- arima(da,order=c(4,1,2))
ARIMA3 <- arima(da,order=c(0,1,1))

#2.模型充分性检验

PNG(12.1,"模型1充分性检验结果")
tsdiag(ARIMA1,pointsize=18)
dev.off()
PNG(12.2,"模型2充分性检验结果")
tsdiag(ARIMA2)
dev.off()
PNG(12.3,"模型3充分性检验结果")
tsdiag(ARIMA3)
dev.off()


##——————————
##第九部分：模型选择
##——————————

#1.回测检验函数（da为原始数据，cr.mo为建模函数，tesi为初始测试集样本量）
back.test <- function(da,cr.mo,tesi=100){
	n <- nrow(da)
	trda <- da[1:(n-tesi),] #初始训练数据集
	teda <- da[(n-tesi+1):n,] #初始测试数据集
	sfe <- rep(0,tesi) #平方误差存储向量
	afe <- rep(0,tesi) #绝对误差存储向量
	bias <- rep(0,tesi) #偏差存储向量
	library(forecast) #加载forecast包
	for(i in 1:(tesi-2)){
		model <- cr.mo(trda) #建模
		prs <- forecast(model,h=2) #预测
		sfe[i] <- (prs$mean[2]-teda[2])^2 #平方误差
		afe[i] <- abs(prs$mean[2]-teda[2]) #绝对误差
		bias[i] <- prs$mean[2]-teda[2] #偏差
		trda <- da[1:(n-tesi+i)] #更新训练数据集
		teda <- teda[-1,] #更新测试数据集
	}
	msfe <- mean(sfe) #均方误差
	mafe <- mean(afe) #平均绝对误差
	mbias <- mean(bias) #平均偏差
	ml <- cr.mo(da) #原始数据建模
	par.nu <- length(ml$coe) #未知参数个数
	#模型AIC值(已在建模列表ml中，直接输出)
	AIC <- ml$aic 
	#模型BIC值(未在建模列表ml中，计算公式为：-2*对数似然+log(样本量)*未知参数个数)
	BIC <- -2*ml$log+log(n)*par.nu
	#预测效果指标输出
	c(msfe,mafe,mbias,AIC,BIC)
}

#2.模型个数
k <- 3 

#3.各模型预测效果指标存储矩阵RS
RS <- matrix(0,k,5,dimnames=list(rep("",k),c("预测均方误差","预测平均绝对误差","预测平均偏差","AIC","BIC"))) #模型预测效果存储矩阵

#4.模型1名称及建模函数
mdn1 <- "ARIMA(1,1,0)" 
md1 <- function(da){
	arima(da,order=c(1,1,0))
}

#5.模型2名称及建模函数
mdn2 <- "ARIMA(4,1,2)"
md2 <- function(da){
	arima(da,order=c(4,1,2))
}

#6.模型3名称及建模函数
mdn3 <- "ARIMA(0,1,1)"
md3 <- function(da){
	arima(da,order=c(0,1,1))
}

#7.由回测检验函数计算各模型预测效果
for(i in 1:3){
	#计算各预测效果指标
	RS[i,] <- back.test(da,eval(parse(text = paste("md",i,sep=""))))
	#将模型名称作为RS矩阵行名
	rownames(RS)[i] <- eval(parse(text = paste("mdn",i,sep="")))
}

#8.输出RS矩阵到外部文件：模型选择.csv
write.csv(RS,paste(outloc,"13.模型选择.csv",sep=""))

#9.利用拉开档次法对模型进行综合评价
sel <- read.csv(paste(outloc,"13.模型选择.csv",sep=""),header=T)
rownames(sel) <- sel[,1]
sel <- sel[,-1]
sel <- scale(sel)[,-3]
H <- t(sel) %*% sel

W <- c(0.5774,0.5773,0.0000,0.5773)
W <- W/sum(W)#拉开档次法得到的各指标权重
PRO <- function(x){
	sum(t(as.matrix(x))*W)
} 
sel <- cbind(sel,apply(sel,1,PRO))
colnames(sel)[5] <- "综合评价值"
sel[order(sel[,5]),]

#10.综合各指标，决定选择模型3：ARIAM(1,1,0)


##——————————
##第十部分：模型预测
##——————————

#用ARIMA(0,1,1)模型预测
library(forecast)
forecast(ARIMA1,h=2)