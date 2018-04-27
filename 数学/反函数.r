rm(list=ls())
options(scipen=200)
options(warn=-1)

Fhs <- function(fuc){

	#获取函数定义域上下边界fdo,fup,该边界由fuc函数体中的超赋值运算符给出
	fuc(0)

	#原函数坐标
	x1 <- seq(fdo,fup,0.001)
	y1 <- fuc(x1)

	#函数值域上下界
	rup <- max(ifelse(abs(y1)==Inf,NA,y1),na.rm=T)
	rdo <- min(ifelse(abs(y1)==Inf,NA,y1),na.rm=T)

	#坐标轴上界
	zbsj <- max(abs(c(fup,fdo,rup,rdo)))
	if(zbsj>10*max(c(fup,fdo))) zbsj <- quantile(abs(c(fup,fdo,rup,rdo)),0.25) 

	#反函数坐标
	y2 <- x1
	x2 <- y1

	#y=x直线坐标
	x3 <- seq(-zbsj-100,zbsj+100,0.1)
	y3 <- x3

	#y=0直线坐标
	x4 <- x3
	y4 <- rep(0,length(x3))

	#x=0直线坐标
	x5 <- y4
	y5 <- x4

	#导函数坐标
	x6 <- x1[-length(x1)]
	y6 <- diff(y1)/diff(x1)

	#相关信息
	htqydx <- paste("[",-zbsj,",",zbsj,"]×[",-zbsj,",",zbsj,"]",sep="")
	gddyy <- paste("[",fdo,",",fup,"]",sep="")
	wdy <- x1[which(y1==Inf | is.na(y1))]
	if(length(wdy)==0) wdy <- "无"
	zdzxz <- c(rup,rdo)
	if(all(round(fuc(y1),2)==round(x1,2) | y1==Inf,na.rm=T)) sfdyyhs <- "是" else sfdyyhs <- "否"

	#依次画函数图像、反函数图像、y=x、y=0、x=0、导函数图像
	plot(x1,y1,type="l",xlim=c(-zbsj,zbsj),ylim=c(-zbsj,zbsj),col="blue",main="原函数、反函数、导函数图像")
	lines(x2,y2,type="l",col="green")
	lines(x3,y3,type="l",col="red",lty=2)
	lines(x4,y4,type="l")
	lines(x5,y5,type="l")
	lines(x6,y6,type="l",col="maroon")
	legend("bottomright",legend=c("原函数","反函数","导函数"),lty=c(1,1,1),col=c("blue","green","maroon"))

	list("1.原函数定义"=fuc,"2.绘图区域大小"=htqydx,"3.原函数的给定定义域（可能包含无定义的点）"=gddyy,"4.原函数在给定定义域内无定义的点"=wdy,"5.原函数在给定定义域内的最大最小值"=zdzxz,"6.反函数是否等于原函数"=sfdyyhs)
}

#函数：(1-x)/(1+x)
#该函数为无界、非单调、非周期、奇、一一对应函数
#反函数与原函数相等，故其图像关于直线y=x对称
#在(-∞,-1)上单调递减，其在-∞处极限为-1，在-1处广义左极限为-∞
#在(-1,+∞)上单调递减，其在+∞处极限为-1，在-1处广义右极限为+∞
#综上，该函数存在一个无穷间断点-1，一条水平渐近线y=-1，一条垂直渐近线 x=-1
fuc1 <- function(x){
	fup <<- 10
	fdo <<- -10
	(1-x)/(1+x)
}

#函数：|x+1|-|x-1|
#该函数为有界、单调、非周期、奇、非一一对应函数
#在[-1,1]从-2单调递增到2，其余区域恒为2，故反函数只在[-1,1]上存在
#函数几何意义：x与-1的距离减去x与1的距离
fuc2 <- function(x){
	fup <<- 2
	fdo <<- -2
	abs(x+1)-abs(x-1)
}

#函数：sgn[x(2-x)(3x+1)]
#该函数为有界、非单调、非周期、非奇非偶、非一一对应函数
#该函数在(-∞,-1/3)以及(2,+∞)上恒为1，在(-1/3,0)以及(0,2)上恒为-1，在-1/3,0,2上恒为0
fuc3 <- function(x){
	fup <<- 3
	fdo <<- -3
	y <- x*(2-x)*(3*x+1)
	ifelse(y>0,1,ifelse(y<0,-1,0))
}

#函数：arcsin(cosx)
#该函数为有界、非单调、周期、偶、非一一对应函数
#上界：π/2,下界：-π/2
#在(2kπ-π,2kπ)上单调递增，(2kπ,2kπ+π)上单调递减，在kπ（k=0,±1,±2）处不可导
#基本周期：2π
fuc4 <- function(x){
	fup <<- 4*pi
	fdo <<- -4*pi
	asin(cos(x))
}

#函数：x-[x]
#该函数上界为1，下界为0
#该函数为周期非奇函非偶函数，基本周期为1
#该函数在(kn,kn+1)上单调递增，kπ（k=0,±1,±2）为该函数的跳跃间断点（左右极限存在但不等）
fuc5 <- function(x){
	fup <<- 10
	fdo <<- -10
	x-ifelse(round(x)<x,round(x),round(x)-1)
}

#函数：双曲正弦函数
Sinh <- function(x){
	fup <<- 2
	fdo <<- -2
	(exp(x)-exp(-x))/2
}

#函数：双曲余弦函数
Cosh <- function(x){
	fup <<- 2
	fdo <<- -2
	(exp(x)+exp(-x))/2
}

#函数：双曲正切函数
Tanh <- function(x){
	fup <<- 2
	fdo <<- -2
	(exp(x)-exp(-x))/(exp(x)+exp(-x))
}

#函数：双曲正切函数
Coth <- function(x){
	fup <<- 5
	fdo <<- -5
	(exp(x)+exp(-x))/(exp(x)-exp(-x))
}

fuc6 <- function(x){
	fup <<- 10
	fdo <<- -10
	(x^2+2*x-1)/3
}

fuc7 <- function(x){
	fup <<- 10
	fdo <<- -10
	((1-x)^2+2*(1-x)-1)/3
}

fuc8 <- function(x){
	fup <<- 10
	fdo <<- -10
	a <- x/(1+x^2)
	a/(1+a^2)
}

fuc9 <- function(x){
	fup <<- 9
	fdo <<- -7
	a <- 1
	b <- 4
	d <- 1/2
	e <- 1
	d*abs(x-a)-e*abs(x-b)
}

fuc10 <- function(x){
	fup <<- 10
	fdo <<- -10
	ifelse(x==0,0,exp(-1/x^2))
}

Fhs(fuc10)

a <- seq(-10,10,0.01)
lines(a,2*fuc6(a),type="l",col="black")
lines(a,fuc7(a),type="l",col="yellow")
lines(a,a^2,type="l",col="pink")


