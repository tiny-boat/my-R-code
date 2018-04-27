rm(list=ls())
library("xlsx")

#��������������������������������������������������������������������������������������������������������������������������������������������
# seting current work directory, output directory and some functions
#��������������������������������������������������������������������������������������������������������������������������������������������

# current work directory
setwd("D:\\���ݿ�ѧ��Data Science��\\2.ͳ��ѧ����\\1.������������ͳ��\\������ݷ���\\")

# output directory
outloc <- "D:\\���ݿ�ѧ��Data Science��\\2.ͳ��ѧ����\\1.������������ͳ��\\������ݷ���\\"

# funciton1: output xlsx file
Scxlsx <- function(obj,fn,sn,isap=TRUE){
	write.xlsx(obj,paste(outloc,fn,".xlsx",sep=""),sheetName=sn,append=isap,row.names=FALSE,showNA=FALSE)
}

# funciton2: output png image
Png <- function(name){
	png(paste(outloc,name,".png",sep=""),width=1024,height=768,pointsize=18)	
}

# funciton3: create contingency/cross table
Cct <- function(da,n,v1,v2,v3,is.mar=TRUE,is.prop=FALSE){
	if(n!=1 & n!=2 & n!=3) print("���棺�ú����ڶ����������Ϊ1,2��3")
	if(n==3){
		if(is.mar){
			if(is.prop){
				addmargins(prop.table(table(da[,v1],da[,v2],da[,v3])))
			}else{
				addmargins(table(da[,v1],da[,v2],da[,v3]))
			}
		}else{
			if(is.prop){
				prop.table(table(da[,v1],da[,v2],da[,v3]))
			}else{
				table(da[,v1],da[,v2],da[,v3])
			}
		}
	}else{
		if(n==2){
			if(is.mar){
				if(is.prop){
					addmargins(prop.table(table(da[,v1],da[,v2])))
				}else{
					addmargins(table(da[,v1],da[,v2]))
				}
			}else{
				if(is.prop){
					prop.table(table(da[,v1],da[,v2]))
				}else{
					table(da[,v1],da[,v2])
				}
			}
		}else{
			if(is.mar){
				if(is.prop){
					addmargins(prop.table(table(da[,v1])))
				}else{
					addmargins(table(da[,v1]))
				}
			}else{
				if(is.prop){
					prop.table(table(da[,v1]))
				}else{
					table(da[,v1])
				}
			}
		}
	} 
}


#����������������������������
# loading data
#����������������������������

load(".\\example2_1.RData")
da <- example2_1


#����������������������������������������������������������������������
# 1.Contingency table / Cross table
#����������������������������������������������������������������������

# 1.1 one-way contingency/cross table with absolute/relative frequency and marginal sum
ctaq1 <- Cct(da,1,"����")
ctaq2 <- Cct(da,1,"�Ա�")
ctaq3 <- Cct(da,1,"̬��")
ctrq1 <- Cct(da,1,"����",is.prop=T)
ctrq2 <- Cct(da,1,"�Ա�",is.prop=T)
ctrq3 <- Cct(da,1,"̬��",is.prop=T)

# 1.2 two-way contingency/cross table with absolute/relative frequency and marginal sum
ctaq12 <- Cct(da,2,"����","�Ա�")
ctaq21 <- Cct(da,2,"�Ա�","����")
ctaq13 <- Cct(da,2,"����","̬��")
ctaq31 <- Cct(da,2,"̬��","����")
ctaq23 <- Cct(da,2,"�Ա�","̬��")
ctaq32 <- Cct(da,2,"̬��","�Ա�")
ctrq12 <- Cct(da,2,"����","�Ա�",is.prop=T)
ctrq21 <- Cct(da,2,"�Ա�","����",is.prop=T)
ctrq13 <- Cct(da,2,"����","̬��",is.prop=T)
ctrq31 <- Cct(da,2,"̬��","����",is.prop=T)
ctrq23 <- Cct(da,2,"�Ա�","̬��",is.prop=T)
ctrq32 <- Cct(da,2,"̬��","�Ա�",is.prop=T)

# 1.3 three-way contingency/cross table with absolute/relative frequency and marginal sum
ctaq123 <- Cct(da,3,"����","�Ա�","̬��")
ctrq123 <- Cct(da,3,"����","�Ա�","̬��",is.prop=T)

# 1.4 output table
Scxlsx(ctaq1,"1.��������̬��������","����(Ƶ��)",isap=FALSE)
Scxlsx(ctrq1,"1.��������̬��������","����(Ƶ��)")
Scxlsx(ctaq2,"1.��������̬��������","�Ա�(Ƶ��)")
Scxlsx(ctrq2,"1.��������̬��������","�Ա�(Ƶ��)")
Scxlsx(ctaq3,"1.��������̬��������","̬��(Ƶ��)")
Scxlsx(ctrq3,"1.��������̬��������","̬��(Ƶ��)")
Scxlsx(ctaq12,"1.��������̬��������","����+�Ա�(Ƶ��)")
Scxlsx(ctrq12,"1.��������̬��������","����+�Ա�(Ƶ��)")
Scxlsx(ctaq13,"1.��������̬��������","����+̬��(Ƶ��)")
Scxlsx(ctrq13,"1.��������̬��������","����+̬��(Ƶ��)")
Scxlsx(ctaq23,"1.��������̬��������","�Ա�+̬��(Ƶ��)")
Scxlsx(ctrq23,"1.��������̬��������","�Ա�+̬��(Ƶ��)")
Scxlsx(ctaq123,"1.��������̬��������","����+�Ա�+̬��(Ƶ��)")
Scxlsx(ctrq123,"1.��������̬��������","����+�Ա�+̬��(Ƶ��)")


#������������������������
# 2.Bar plot
#������������������������

# ����2.0����
# Contingency table for plot
x1 <- ctaq1[-length(ctaq1)]
x2 <- ctaq2[-length(ctaq2)]
x3 <- ctaq3[-length(ctaq3)]
x12 <- ctaq12[-nrow(ctaq12),-ncol(ctaq12)]
x21 <- ctaq21[-nrow(ctaq21),-ncol(ctaq21)]
x13 <- ctaq13[-nrow(ctaq13),-ncol(ctaq13)]
x31 <- ctaq31[-nrow(ctaq31),-ncol(ctaq31)]
x23 <- ctaq23[-nrow(ctaq23),-ncol(ctaq23)]
x32 <- ctaq32[-nrow(ctaq32),-ncol(ctaq32)]
x123 <- Cct(da,3,"����","�Ա�","̬��",is.mar=FALSE)
x231 <- Cct(da,3,"�Ա�","̬��","����",is.mar=FALSE)
x312 <- Cct(da,3,"̬��","����","�Ա�",is.mar=FALSE)

# ����2.1���� 
# Simple bar plot, used for one categorical/classified/qualitative variable,
# includes two types: the vertical and the horizontal,
# and pareto plot, named by Italian economist V.Pareto, is its variant

# 2.1.1 simple vertical bar plot
Png("2.1.1 �򵥴�ֱ����ͼ (simple vertical bar plot)")
par(mfrow=c(1,3))
bp1_1 <- barplot(x1,xlab="����",ylab="Ƶ��",main="�����������ֲ�����ͼ")
bp2_1 <- barplot(x2,xlab="�Ա�",ylab="Ƶ��",main="���Ա������ֲ�����ͼ")
bp3_1 <- barplot(x3,xlab="̬��",ylab="Ƶ��",main="��̬�������ֲ�����ͼ")
dev.off()

# 2.1.2 simple horizontal bar plot
Png("2.1.2 ��ˮƽ����ͼ (simple horizontal bar plot)")
layout(matrix(c(1,2,3),nrow=3,ncol=1,byrow=T),heights=c(1.8,1.2,1.2))
bp1_2 <- barplot(x1,ylab="Ƶ��",xlab="Ƶ��",main="�����������ֲ�����ͼ",horiz=TRUE)
bp2_2 <- barplot(x2,ylab="Ƶ��",xlab="Ƶ��",main="���Ա������ֲ�����ͼ",horiz=TRUE)
bp3_2 <- barplot(x3,ylab="Ƶ��",xlab="Ƶ��",main="��̬�������ֲ�����ͼ",horiz=TRUE)
dev.off()

# 2.1.3 pareto plot
Png("2.1.3 ������ͼ��pareto plot��")
par(mai=c(1.5,1.5,1.5,1.5))
y1 <- cumsum(x1)/sum(x1)
bp1_3 <- barplot(x1,xlab="����",ylab="Ƶ��",main="�����������ֲ�������ͼ")
par(new=T)
plot(y1,type='b',lwd=1.5,pch=15,axes=FALSE,xlab=' ',ylab=' ',main=' ')
axis(4)
par(las=0)
mtext("�ۻ�Ƶ��",side=4,line=3)
mtext("�ۻ��ֲ�����",line=-3.5,cex=0.8,adj=0.75)
dev.off()

# ����2.2���� 
# Double bar plot, used for two categorical/classified/qualitative variable,
# includes two types: the juxtaposed and the stacked,
# and spine plot can be viewd as the variant of stacked bar plot 

# 2.2.1 juxtaposed bar plot

Png("2.2.1.1 ���и�ʽ����ͼ (juxtaposed double bar plot)")
par(mfrow=c(1,2))
bp12_1 <- barplot(x12,xlab="�Ա�",ylab="Ƶ��",main="���Ա�ͬ���������ֲ���������ͼ",beside=TRUE,legend=rownames(x12),args.legend=list(x=4,cex=0.9,bty='n'))
bp21_1 <- barplot(x21,xlab="����",ylab="Ƶ��",main="��������ͬ�Ա������ֲ���������ͼ",beside=TRUE,legend=rownames(x21),args.legend=list(x=11,cex=0.9,bty='n'))
dev.off()

Png("2.2.1.2 ���и�ʽ����ͼ (juxtaposed double bar plot)")
par(mfrow=c(1,2))
bp13_1 <- barplot(x13,xlab="̬��",ylab="Ƶ��",main="��̬�Ȳ�ͬ���������ֲ���������ͼ",beside=TRUE,legend=rownames(x13),args.legend=list(x=5,cex=0.9,bty='n'))
bp31_1 <- barplot(x31,xlab="����",ylab="Ƶ��",main="��������̬ͬ�������ֲ���������ͼ",beside=TRUE,legend=rownames(x31),args.legend=list(x=12,cex=0.9,bty='n'))
dev.off()

Png("2.2.1.3 ���и�ʽ����ͼ (juxtaposed double bar plot)")
par(mfrow=c(1,2))
bp23_1 <- barplot(x23,xlab="̬��",ylab="Ƶ��",main="��̬�Ȳ�ͬ�Ա������ֲ���������ͼ",beside=TRUE,legend=rownames(x23),args.legend=list(x=2,cex=0.9,bty='n'))
bp32_1 <- barplot(x32,xlab="�Ա�",ylab="Ƶ��",main="���Ա�̬ͬ�������ֲ���������ͼ",beside=TRUE,legend=rownames(x32),args.legend=list(x=2.2,cex=0.9,bty='n'))
dev.off()

# 2.2.2 stacked bar plot

Png("2.2.2.1 �ѵ���ʽ����ͼ (stacked double bar plot)")
par(mfrow=c(1,2))
bp12_2 <- barplot(x12,xlab="�Ա�",ylab="Ƶ��",main="���Ա�ͬ���������ֲ��ѵ�����ͼ",legend=rownames(x12),args.legend=list(x=0.85,y=45,cex=0.9,bty='n'))
bp21_2 <- barplot(x21,xlab="����",ylab="Ƶ��",main="��������ͬ�Ա������ֲ��ѵ�����ͼ",legend=rownames(x21),args.legend=list(x=4.8,cex=0.9,bty='n'))
dev.off()

Png("2.2.2.2 �ѵ���ʽ����ͼ (stacked double bar plot)")
par(mfrow=c(1,2))
bp13_2 <- barplot(x13,xlab="̬��",ylab="Ƶ��",main="��̬�Ȳ�ͬ���������ֲ��ѵ�����ͼ",legend=rownames(x13),args.legend=list(x=0.85,cex=0.9,bty='n'))
bp31_2 <- barplot(x31,xlab="����",ylab="Ƶ��",main="��������̬ͬ�������ֲ��ѵ�����ͼ",legend=rownames(x31),args.legend=list(x=4.8,cex=0.9,bty='n'))
dev.off()

Png("2.2.2.3 �ѵ���ʽ����ͼ (stacked double bar plot)")
par(mfrow=c(1,2))
bp23_2 <- barplot(x23,xlab="̬��",ylab="Ƶ��",main="��̬�Ȳ�ͬ�Ա������ֲ��ѵ�����ͼ",legend=rownames(x23),args.legend=list(x=0.65,cex=0.9,bty='n'))
bp32_2 <- barplot(x32,xlab="�Ա�",ylab="Ƶ��",main="���Ա�̬ͬ�������ֲ��ѵ�����ͼ",legend=rownames(x32),args.legend=list(x=0.8,cex=0.9,bty='n'))
dev.off()

# 2.2.3 spine plot

library("vcd")

Png("2.2.3.1 ����ͼ��spine plot��")
bp12_3 <- spine(x21,xlab="�Ա�",ylab="����",main="���Ա�ͬ���������ֲ�����ͼ")
dev.off()

Png("2.2.3.2 ����ͼ��spine plot��")
bp21_3 <- spine(x12,xlab="����",ylab="�Ա�",main="��������ͬ�Ա������ֲ�����ͼ")
dev.off()

Png("2.2.3.3 ����ͼ��spine plot��")
bp13_3 <- spine(x31,xlab="̬��",ylab="����",main="��̬�Ȳ�ͬ���������ֲ�����ͼ")
dev.off()

Png("2.2.3.4 ����ͼ��spine plot��")
bp31_3 <- spine(x13,xlab="����",ylab="̬��",main="��������̬ͬ�������ֲ�����ͼ")
dev.off()

Png("2.2.3.5 ����ͼ��spine plot��")
bp23_3 <- spine(x32,xlab="̬��",ylab="�Ա�",main="��̬�Ȳ�ͬ�Ա������ֲ�����ͼ")
dev.off()

Png("2.2.3.6 ����ͼ��spine plot��")
bp32_3 <- spine(x23,xlab="�Ա�",ylab="̬��",main="���Ա�̬ͬ�������ֲ�����ͼ")
dev.off()

detach("package:vcd")

# ����2.4���� 
# Mosaic plot, used for three categorical/classified/qualitative variable,
# can be viewd as the variant of juxtaposed bar plot,
# and every blocks' size is proportional to the frequency 

Png("2.2.4.1 ������ͼ��mosaic plot��")
bp123_1 <- mosaicplot(x123,xlab="����",ylab="�Ա�",main="��������ͬ�Ա𡢲�̬ͬ�������ֲ�������ͼ",col=c("grey50","grey80"))
dev.off()

Png("2.2.4.2 ������ͼ��mosaic plot��")
bp213_1 <- mosaicplot(x213,xlab="�Ա�",ylab="����",main="���Ա�ͬ��������̬ͬ�������ֲ�������ͼ",col=c("grey50","grey80"))
dev.off()

Png("2.2.4.3 ������ͼ��mosaic plot��")
bp312_1 <- mosaicplot(x312,xlab="̬��",ylab="����",main="��̬�Ȳ�ͬ��������ͬ�Ա������ֲ�������ͼ",col=c("grey50","grey80"))
dev.off()

#������������������������
# 3.Pie chart
#������������������������

# Most statisticians regard Pie chart as a terrible chart, 
# because people can estimate the length more correctly than the size.
# Therefore, they recommend barplot or point diagram to people instead of pie chart
# But in 2009, Lemon & Tyagi provided users with fan chart, a variant of pie chart.
# It can display the relative number and the relative difference simultaneously. 

# ����3.1����
# pie chart

Png("3.1 ��ͼ��pie chart��")
layout(matrix(c(2,3,1,1),nrow=2,ncol=2))
par(mai=c(0.5,1.2,0.5,1.2))
lbls1 <- names(x1)
pct1 <- round(prop.table(x1)*100,2)
lbls1 <- paste(lbls1," ",pct1,"%",sep="")
lbls2 <- names(x2)
pct2 <- round(prop.table(x2)*100,2)
lbls2 <- paste(lbls2," ",pct2,"%",sep="")
lbls3 <- names(x3)
pct3 <- round(prop.table(x3)*100,2)
lbls3 <- paste(lbls3," ",pct3,"%",sep="")
pi1_1 <- pie(x1,labels=lbls1,main="�����������ֲ���ͼ",col=rainbow(length(lbls1)))
pi2_1 <- pie(x2,labels=lbls2,main="���Ա������ֲ���ͼ",col=rainbow(length(lbls2)))
pi3_1 <- pie(x3,labels=lbls3,main="��̬�������ֲ���ͼ",col=rainbow(length(lbls3)))
dev.off()

# ����3.2����
# fan chart

library("plotrix")

Png("3.2.1 ��ͼ��fan chart��")
pi1_2 <- fan.plot(x1,labels=lbls1,main="�����������ֲ���ͼ")
dev.off()

Png("3.2.2 ��ͼ��fan chart��")
pi2_2 <- fan.plot(x2,labels=lbls2,main="���Ա������ֲ���ͼ")
dev.off()

Png("3.2.3 ��ͼ��fan chart��")
pi3_2 <- fan.plot(x3,labels=lbls3,main="��̬�������ֲ���ͼ")
dev.off()

detach("package:plotrix")


#����������������������������
# 4.chisq.test
#����������������������������

chisq.test(x12)
chisq.test(x13)
chisq.test(x23)
library(vcd)
assocstats(x12)
assocstats(x13)
assocstats(x23)
detach("package:vcd")