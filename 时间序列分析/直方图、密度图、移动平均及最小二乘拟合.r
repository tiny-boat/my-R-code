rm(list=ls())

library(fBasics)

da <-  read.table("G:\\ʱ�����з���\\chapter1\\ch1data\\d-mmm-0111.txt",header=T)
mmm <- da[,2]

hist(mmm,nclass=30) #nclass���������

dl <-  density(mmm) #�ܶȹ���
range(mmm)

x <- seq(-0.1,0.1,0.001)
y1 <- dnorm(x,mean(mmm),stdev(mmm)) #��̬�ܶ�

#�����ܶ�����̬�ܶȶԱȣ�ǰ�᣺���߾�ֵ��������ȣ�
plot(dl$x,dl$y,xlab="rtn",ylab="density",type="l")
lines(x,y1,lty=2)

#�������ڻ��ƹ�Ʊ����ͼ��Bar Chart���Ľű�
source("G:\\ʱ�����з���\\chapter1\\ohlc.R")

X <- read.csv("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\ʱ������\\ƻ����˾\\ƻ����˾.csv",header=T)
xx <- X[1008:1132,2:5]

ohlc(xx,xl="days",yl="price",title="ƻ����Ʊ")

#�������ڻ����ƶ�ƽ��ͼ��Moving-average chart���Ľű�
source("G:\\ʱ�����з���\\chapter1\\ma.R")

x1 <- X[767:1244,5]
ma(x1,21) #21Ϊ�ƶ�ƽ������

#ɢ��ͼ����С�������
da <- read.table("G:\\ʱ�����з���\\chapter1\\ch1data\\m-ibmsp-2611.txt",header=T)
ibm <- log(da$ibm+1)
sp <- log(da$sp+1)
lmcoe <- lm(ibm~sp)$coefficient
plot(sp,ibm,cex=0.8,main="IBM��Ʊ��S&P��Ʊ�¶���������ɢ��ͼ����С�������")
abline(lmcoe[1],lmcoe[2])