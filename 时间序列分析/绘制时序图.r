rm(list=ls())

##��������������������������
##��һ���֣���Ʊ���ݵĻ�ȡ
##��������������������������

#1.������Ʊ���ݵ����أ�load���������output������ financial.lo
#financial.name Ϊ��Ʊ���ƣ�financial.symbol Ϊ��Ʊ����
#start.timeΪ��ʼʱ�䣬end.timeΪ��ֹʱ�䣬outlocΪ���·��

financial.lo <- function(financial.name,financial.symbol,start.time,end.time,outloc){

	#���ء�quantmod����������� Jeffry ������	
	#���øð����ɴ��Ż��ƾ���yahoo�����ȸ�ƾ���google����ʥ·��˹��������е�������������ݿ⣨PRED�����ؽ�������
	
	library(quantmod)
	
	#��getSymbols�������ع�Ʊ����
	setSymbolLookup(OBJ = list(name=financial.symbol,src="yahoo",from=start.time,to=end.time))
	getSymbols("OBJ")

	#����Ʊ�������Ϊһ��csv�ļ�
	da <- as.matrix(OBJ) #�� xts��zoo ����ת��Ϊ matrix ����
	colnames(da) <- c("���̼�","��߼�","��ͼ�","���̼�","�ɽ���","���������̼�") #����ԭӢ������
	write.csv(da,paste(outloc,financial.name,".csv",sep=""))

}

#2.���ز�����������д�2007.09.19��2017.05.04�Ĺ�Ʊ���ݣ��ļ�������D�̱�������Ŀ¼��

financial.lo("��������","601169.SS","2007-09-19","2017-05-04","D:\\��������\\")


##������������������������������������������
##�ڶ����֣���Ʊ���ݵĶ�ȡ������ǰ��׼������
##������������������������������������������

library(xts) #���� xts �������ڽ� data.frame ��������ת���� xts/zoo ��������

#1.�����Ʊ����

da <- read.csv("D:\\��������\\��������.csv",header=T) 

#2.�����ݿ�ת���� xts/zoo ���Ͳ���ȡ��5�У����̼ۣ�������Ϊ����������

date <- as.character(da[,1]) #����Ʊ���ݵ�һ�У�ʱ�䣩ת��Ϊ�ַ�������
da <- xts(da[,5],order.by=as.Date(date,format="%Y/%m/%d")) 

#3.�趨ͼ�Ρ��ı����·��

outloc <- "D:\\��������\\" 

#4.����ͼ�����������iΪͼ�α�ţ�nameΪͼ�����Ƶĺ��Ĳ���

PNG <- function(i,name){
	png(paste(outloc,i,".�������й�Ʊ���̼�",name,".png",sep=""),width=1024,height=768,pointsize=18)	
}


##����������������������������������������������������������������������
##�������֣���Ʊ���̼�ʱ��ͼ��������Լ���pֵͼ�������ͼ��ƫ�����ͼ
##����������������������������������������������������������������������

#1.��Ʊ���̼�ʱ��ͼ

PNG(1,"ʱ��ͼ")
plot(da,xlab="���� (date)",ylab="���̼� (close value)",main="�������й�Ʊ���̼�ʱ��ͼ")
dev.off()

#2.��Ʊ���̼۴�����Լ���pֵͼ

Bote.p <- rep(0,12)
lag <- 1:12
for(i in 1:12){
	Bote.p[i] <- Box.test(da,lag=i,type="Ljung")$p.value
}
x <- seq(0,13,0.01)
y <- rep(0.05,length(x))
PNG(2,"������Լ���pֵͼ")
plot(lag,Bote.p,xlab="�ͺ����� (lag)",ylab="����pֵ (p value)",ylim=c(0,1),main="�������й�Ʊ���̼۴�����Լ��� p ֵͼ")
lines(x,y,lty=2)
dev.off()

#3.��Ʊ���̼������ͼ

PNG(3,"�����ͼ")
acf(da,lag=100,xlab="�ͺ����� (lag)",ylab="�����ϵ�� (ACF)",main="�������й�Ʊ���̼������ͼ")
dev.off()

#4.��Ʊ���̼�ƫ�����ͼ

PNG(4,"ƫ�����ͼ")
pacf(da,lag=100,xlab="�ͺ����� (lag)",ylab="ƫ�����ϵ�� (PACF)",main="�������й�Ʊ���̼�ƫ�����ͼ")
dev.off()


##������������������������������������������������������������������������
##���Ĳ��֣�ƽ����ͳ�Ƽ��飬�ɵ�һ����ƫ�����ͼȷ�����㵥λ�������ͺ����
##������������������������������������������������������������������������

#��Ʊ���̼۵�λ������(���ڵ������ֵ�ƫ�����ͼ��ѡ��AR(3)ģ������չ��λ������)

library(tseries)
adt.p <- adf.test(da,"stationary",3)$p.value
if(adt.p >= 0.05){
	ters <- paste("��չ��λ������pֵ = ",adt.p,"�����ܾܾ���λ�����裬��Ϊ���з�ƽ��")
}else{
	ters <- paste("��չ��λ������pֵ = ",adt.p,"���ܾ���λ�����裬��Ϊ����ƽ��")
}
write.table(ters,paste(outloc,"5.��Ʊ���̼۵�λ��������.txt"))


##������������������������������������������������������������������������������
##���岿�֣���Ʊ���̼۲������ʱ��ͼ��������Լ���pֵͼ�������ͼ��ƫ�����ͼ
##������������������������������������������������������������������������������

#1.һ�ײ��

dda <- diff(da)[-1]

#2.һ�ײ��ʱ��ͼ

PNG(6,"�������ʱ��ͼ")
plot(dda,xlab="���� (date)",ylab="���̼۲�� (difference of close value)",main="�������й�Ʊ���̼۲������ʱ��ͼ")
dev.off()

#3.һ�ײ�ִ�����Լ���pֵͼ

for(i in 1:12){
	Bote.p[i] <- Box.test(dda,lag=i,type="Ljung")$p.value
}
PNG(7,"������д�����Լ���pֵͼ")
plot(lag,Bote.p,xlab="�ͺ����� (lag)",ylab="����pֵ (p value)",ylim=c(0,1),main="�������й�Ʊ���̼۲�����д�����Լ��� p ֵͼ")
lines(x,y,lty=2)
dev.off()

#4.һ�ײ�������ͼ

PNG(8,"������������ͼ")
acf(dda,lag=100,xlab="�ͺ����� (lag)",ylab="�����ϵ�� (ACF)",main="�������й�Ʊ���̼۲�����������ͼ")
dev.off()

#5.һ�ײ��ƫ�����ͼ

PNG(9,"�������ƫ�����ͼ")
pacf(dda,lag=100,xlab="�ͺ����� (lag)",ylab="ƫ�����ϵ�� (PACF)",ylim=c(-0.2,0.2),main="�������й�Ʊ���̼۲������ƫ�����ͼ")
dev.off()


##��������������������������������������������������������������������������������
##�������֣��������ƽ����ͳ�Ƽ��飬�ɵ��岿��ƫ�����ͼȷ�����㵥λ�������ͺ����
##��������������������������������������������������������������������������������

#һ�ײ�ֵ�λ������

library(tseries)
dadt.p <- adf.test(dda,"stationary",1)$p.value
if(dadt.p >= 0.05){
	dters <- paste("��չ��λ������pֵ = ",dadt.p,"�����ܾܾ���λ�����裬��Ϊ���з�ƽ��")
}else{
	dters <- paste("��չ��λ������pֵ = ",dadt.p,"���ܾ���λ�����裬��Ϊ����ƽ��")
}
write.table(dters,paste(outloc,"10.��Ʊ���̼۲�����е�λ��������.txt"))


##������������������������������������������
##���߲��֣�ARIMA �Զ����׼���ѡģ�͵�ȷ��
##������������������������������������������

#1.�۲�ACFͼ��PACFͼ����ARIMAģ�ͽ���Ϊ1,1,0

library(forecast)
pdq <- auto.arima(da)$arma
pdq <- c(pdq[1],1,pdq[2])
names(pdq) <- c("p","d","q")
write.table(pdq,paste(outloc,"11.ARIMA�Զ����׺������׽��.txt"))

#2.�Զ�ʶ��ARIMAģ�ͽ���Ϊ4,1,2
#3.��������һ��ָ��ƽ��ģ�ͣ���ȼ��ڽ���Ϊ0,1,1��ARIMAģ��


##������������������������������
##�ڰ˲��֣���ģ��ģ�ͳ���Լ���
##������������������������������

#1.��ģ

ARIMA1 <- arima(da,order=c(1,1,0))
ARIMA2 <- arima(da,order=c(4,1,2))
ARIMA3 <- arima(da,order=c(0,1,1))

#2.ģ�ͳ���Լ���

PNG(12.1,"ģ��1����Լ�����")
tsdiag(ARIMA1,pointsize=18)
dev.off()
PNG(12.2,"ģ��2����Լ�����")
tsdiag(ARIMA2)
dev.off()
PNG(12.3,"ģ��3����Լ�����")
tsdiag(ARIMA3)
dev.off()


##��������������������
##�ھŲ��֣�ģ��ѡ��
##��������������������

#1.�ز���麯����daΪԭʼ���ݣ�cr.moΪ��ģ������tesiΪ��ʼ���Լ���������
back.test <- function(da,cr.mo,tesi=100){
	n <- nrow(da)
	trda <- da[1:(n-tesi),] #��ʼѵ�����ݼ�
	teda <- da[(n-tesi+1):n,] #��ʼ�������ݼ�
	sfe <- rep(0,tesi) #ƽ�����洢����
	afe <- rep(0,tesi) #�������洢����
	bias <- rep(0,tesi) #ƫ��洢����
	library(forecast) #����forecast��
	for(i in 1:(tesi-2)){
		model <- cr.mo(trda) #��ģ
		prs <- forecast(model,h=2) #Ԥ��
		sfe[i] <- (prs$mean[2]-teda[2])^2 #ƽ�����
		afe[i] <- abs(prs$mean[2]-teda[2]) #�������
		bias[i] <- prs$mean[2]-teda[2] #ƫ��
		trda <- da[1:(n-tesi+i)] #����ѵ�����ݼ�
		teda <- teda[-1,] #���²������ݼ�
	}
	msfe <- mean(sfe) #�������
	mafe <- mean(afe) #ƽ���������
	mbias <- mean(bias) #ƽ��ƫ��
	ml <- cr.mo(da) #ԭʼ���ݽ�ģ
	par.nu <- length(ml$coe) #δ֪��������
	#ģ��AICֵ(���ڽ�ģ�б�ml�У�ֱ�����)
	AIC <- ml$aic 
	#ģ��BICֵ(δ�ڽ�ģ�б�ml�У����㹫ʽΪ��-2*������Ȼ+log(������)*δ֪��������)
	BIC <- -2*ml$log+log(n)*par.nu
	#Ԥ��Ч��ָ�����
	c(msfe,mafe,mbias,AIC,BIC)
}

#2.ģ�͸���
k <- 3 

#3.��ģ��Ԥ��Ч��ָ��洢����RS
RS <- matrix(0,k,5,dimnames=list(rep("",k),c("Ԥ��������","Ԥ��ƽ���������","Ԥ��ƽ��ƫ��","AIC","BIC"))) #ģ��Ԥ��Ч���洢����

#4.ģ��1���Ƽ���ģ����
mdn1 <- "ARIMA(1,1,0)" 
md1 <- function(da){
	arima(da,order=c(1,1,0))
}

#5.ģ��2���Ƽ���ģ����
mdn2 <- "ARIMA(4,1,2)"
md2 <- function(da){
	arima(da,order=c(4,1,2))
}

#6.ģ��3���Ƽ���ģ����
mdn3 <- "ARIMA(0,1,1)"
md3 <- function(da){
	arima(da,order=c(0,1,1))
}

#7.�ɻز���麯�������ģ��Ԥ��Ч��
for(i in 1:3){
	#�����Ԥ��Ч��ָ��
	RS[i,] <- back.test(da,eval(parse(text = paste("md",i,sep=""))))
	#��ģ��������ΪRS��������
	rownames(RS)[i] <- eval(parse(text = paste("mdn",i,sep="")))
}

#8.���RS�����ⲿ�ļ���ģ��ѡ��.csv
write.csv(RS,paste(outloc,"13.ģ��ѡ��.csv",sep=""))

#9.�����������η���ģ�ͽ����ۺ�����
sel <- read.csv(paste(outloc,"13.ģ��ѡ��.csv",sep=""),header=T)
rownames(sel) <- sel[,1]
sel <- sel[,-1]
sel <- scale(sel)[,-3]
H <- t(sel) %*% sel

W <- c(0.5774,0.5773,0.0000,0.5773)
W <- W/sum(W)#�������η��õ��ĸ�ָ��Ȩ��
PRO <- function(x){
	sum(t(as.matrix(x))*W)
} 
sel <- cbind(sel,apply(sel,1,PRO))
colnames(sel)[5] <- "�ۺ�����ֵ"
sel[order(sel[,5]),]

#10.�ۺϸ�ָ�꣬����ѡ��ģ��3��ARIAM(1,1,0)


##��������������������
##��ʮ���֣�ģ��Ԥ��
##��������������������

#��ARIMA(0,1,1)ģ��Ԥ��
library(forecast)
forecast(ARIMA1,h=2)