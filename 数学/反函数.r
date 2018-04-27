rm(list=ls())
options(scipen=200)
options(warn=-1)

Fhs <- function(fuc){

	#��ȡ�������������±߽�fdo,fup,�ñ߽���fuc�������еĳ���ֵ���������
	fuc(0)

	#ԭ��������
	x1 <- seq(fdo,fup,0.001)
	y1 <- fuc(x1)

	#����ֵ�����½�
	rup <- max(ifelse(abs(y1)==Inf,NA,y1),na.rm=T)
	rdo <- min(ifelse(abs(y1)==Inf,NA,y1),na.rm=T)

	#�������Ͻ�
	zbsj <- max(abs(c(fup,fdo,rup,rdo)))
	if(zbsj>10*max(c(fup,fdo))) zbsj <- quantile(abs(c(fup,fdo,rup,rdo)),0.25) 

	#����������
	y2 <- x1
	x2 <- y1

	#y=xֱ������
	x3 <- seq(-zbsj-100,zbsj+100,0.1)
	y3 <- x3

	#y=0ֱ������
	x4 <- x3
	y4 <- rep(0,length(x3))

	#x=0ֱ������
	x5 <- y4
	y5 <- x4

	#����������
	x6 <- x1[-length(x1)]
	y6 <- diff(y1)/diff(x1)

	#�����Ϣ
	htqydx <- paste("[",-zbsj,",",zbsj,"]��[",-zbsj,",",zbsj,"]",sep="")
	gddyy <- paste("[",fdo,",",fup,"]",sep="")
	wdy <- x1[which(y1==Inf | is.na(y1))]
	if(length(wdy)==0) wdy <- "��"
	zdzxz <- c(rup,rdo)
	if(all(round(fuc(y1),2)==round(x1,2) | y1==Inf,na.rm=T)) sfdyyhs <- "��" else sfdyyhs <- "��"

	#���λ�����ͼ�񡢷�����ͼ��y=x��y=0��x=0��������ͼ��
	plot(x1,y1,type="l",xlim=c(-zbsj,zbsj),ylim=c(-zbsj,zbsj),col="blue",main="ԭ��������������������ͼ��")
	lines(x2,y2,type="l",col="green")
	lines(x3,y3,type="l",col="red",lty=2)
	lines(x4,y4,type="l")
	lines(x5,y5,type="l")
	lines(x6,y6,type="l",col="maroon")
	legend("bottomright",legend=c("ԭ����","������","������"),lty=c(1,1,1),col=c("blue","green","maroon"))

	list("1.ԭ��������"=fuc,"2.��ͼ�����С"=htqydx,"3.ԭ�����ĸ��������򣨿��ܰ����޶���ĵ㣩"=gddyy,"4.ԭ�����ڸ������������޶���ĵ�"=wdy,"5.ԭ�����ڸ����������ڵ������Сֵ"=zdzxz,"6.�������Ƿ����ԭ����"=sfdyyhs)
}

#������(1-x)/(1+x)
#�ú���Ϊ�޽硢�ǵ����������ڡ��桢һһ��Ӧ����
#��������ԭ������ȣ�����ͼ�����ֱ��y=x�Գ�
#��(-��,-1)�ϵ����ݼ�������-�޴�����Ϊ-1����-1����������Ϊ-��
#��(-1,+��)�ϵ����ݼ�������+�޴�����Ϊ-1����-1�������Ҽ���Ϊ+��
#���ϣ��ú�������һ�������ϵ�-1��һ��ˮƽ������y=-1��һ����ֱ������ x=-1
fuc1 <- function(x){
	fup <<- 10
	fdo <<- -10
	(1-x)/(1+x)
}

#������|x+1|-|x-1|
#�ú���Ϊ�н硢�����������ڡ��桢��һһ��Ӧ����
#��[-1,1]��-2����������2�����������Ϊ2���ʷ�����ֻ��[-1,1]�ϴ���
#�����������壺x��-1�ľ����ȥx��1�ľ���
fuc2 <- function(x){
	fup <<- 2
	fdo <<- -2
	abs(x+1)-abs(x-1)
}

#������sgn[x(2-x)(3x+1)]
#�ú���Ϊ�н硢�ǵ����������ڡ������ż����һһ��Ӧ����
#�ú�����(-��,-1/3)�Լ�(2,+��)�Ϻ�Ϊ1����(-1/3,0)�Լ�(0,2)�Ϻ�Ϊ-1����-1/3,0,2�Ϻ�Ϊ0
fuc3 <- function(x){
	fup <<- 3
	fdo <<- -3
	y <- x*(2-x)*(3*x+1)
	ifelse(y>0,1,ifelse(y<0,-1,0))
}

#������arcsin(cosx)
#�ú���Ϊ�н硢�ǵ��������ڡ�ż����һһ��Ӧ����
#�Ͻ磺��/2,�½磺-��/2
#��(2k��-��,2k��)�ϵ���������(2k��,2k��+��)�ϵ����ݼ�����k�У�k=0,��1,��2�������ɵ�
#�������ڣ�2��
fuc4 <- function(x){
	fup <<- 4*pi
	fdo <<- -4*pi
	asin(cos(x))
}

#������x-[x]
#�ú����Ͻ�Ϊ1���½�Ϊ0
#�ú���Ϊ���ڷ��溯��ż��������������Ϊ1
#�ú�����(kn,kn+1)�ϵ���������k�У�k=0,��1,��2��Ϊ�ú�������Ծ��ϵ㣨���Ҽ��޴��ڵ����ȣ�
fuc5 <- function(x){
	fup <<- 10
	fdo <<- -10
	x-ifelse(round(x)<x,round(x),round(x)-1)
}

#������˫�����Һ���
Sinh <- function(x){
	fup <<- 2
	fdo <<- -2
	(exp(x)-exp(-x))/2
}

#������˫�����Һ���
Cosh <- function(x){
	fup <<- 2
	fdo <<- -2
	(exp(x)+exp(-x))/2
}

#������˫�����к���
Tanh <- function(x){
	fup <<- 2
	fdo <<- -2
	(exp(x)-exp(-x))/(exp(x)+exp(-x))
}

#������˫�����к���
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


