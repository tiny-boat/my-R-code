rm(list=ls())

library(mvstats)

da <- read.csv("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\������.csv",header=T)

rownames(da) <- da[,1] #��������
da <- da[,-1] #ȥ��������

##��Ԫ������̬�Լ���

mean.da <- apply(da,2,mean) #��ֵ

DEV <- function(x){
	x-mean.da
}

dev <- apply(da,1,DEV) #������

cov <- (dev %*% t(dev))/(nrow(da)-1) #Э�������
 
icov <- solve(cov) #Э���������

STA.dev <- function(x){
	t(x) %*% icov %*% x #���Ͼ�����㺯��
}

or.st <- sort(apply(dev,2,STA.dev)) #���Ͼ���˳��ͳ����

per.tr <- function(x){
	(which(or.st==x)-0.5)/nrow(da) #ʵ�ʷ�λ�����㺯��
}

pertr <- sapply(or.st,per.tr) #ʵ�ʷ�λ��

perth <- pchisq(or.st,ncol(da)) #���۷�Ϊ��

png("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\���ڿ���ͳ������QQͼ.png",height=768,width=1024,pointsize=18)

plot(perth,pertr,xlab="���۷�λ��",ylab="������Ϊ��",main="���ڿ���ͳ������ Q-Q ͼ")

lines(seq(-0.1,1.1,0.01),seq(-0.1,1.1,0.01),lty=2)

dev.off()


##����Ԥ����

da[,7] <- 1/da[,7] #��Сָ�꼫��
da <- scale(da[,-1]) #��׼������
da <- as.data.frame(da)


##��Ԫ��̬�Լ���δͨ����ʹ�����ɷַ���������

RS2 <- factpc(da,2,rot="varimax") #���ڷ�������������ת

fac.var.de <- RS2$Vars #�������
write.csv(fac.var.de,"D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\�������.csv")

fac.lo <- RS2$loadings #�����غ�

write.csv(fac.lo,"D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\�����غɾ���.csv")

fac.sc <- RS2$scores #���ӵ÷�
write.csv(fac.sc,"D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\���ӵ÷�.csv")

fac.so <- RS2$Rank 

fac.ra <- fac.so[order(fac.so[,2]),] #������
write.csv(fac.ra,"D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\������.csv")

par(mfcol=c(1,1))

png("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\��Ԫͳ��\\���ӵ÷�ͼ.png",height=768,width=1024,pointsize=18)

plot.text(RS2$scores) #���ӵ÷�ͼ

dev.off()
biplot(RS2$scores,RS2$loading) #��Ϣ�ص�ͼ

list("�������"=fac.var.de,"�����غ�"=fac.lo,"���ӵ÷�"=fac.sc,"��Ʒ����"=fac.ra)

