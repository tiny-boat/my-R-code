rm(list=ls())
library("xlsx")

#——————————————————————————————————————————————————————————————————————
# seting current work directory, output directory and some functions
#——————————————————————————————————————————————————————————————————————

# current work directory
setwd("D:\\数据科学（Data Science）\\2.统计学基础\\1.概率论与数理统计\\类别数据分析\\")

# output directory
outloc <- "D:\\数据科学（Data Science）\\2.统计学基础\\1.概率论与数理统计\\类别数据分析\\"

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
	if(n!=1 & n!=2 & n!=3) print("警告：该函数第二项参数必须为1,2或3")
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


#——————————————
# loading data
#——————————————

load(".\\example2_1.RData")
da <- example2_1


#———————————————————————————————————
# 1.Contingency table / Cross table
#———————————————————————————————————

# 1.1 one-way contingency/cross table with absolute/relative frequency and marginal sum
ctaq1 <- Cct(da,1,"社区")
ctaq2 <- Cct(da,1,"性别")
ctaq3 <- Cct(da,1,"态度")
ctrq1 <- Cct(da,1,"社区",is.prop=T)
ctrq2 <- Cct(da,1,"性别",is.prop=T)
ctrq3 <- Cct(da,1,"态度",is.prop=T)

# 1.2 two-way contingency/cross table with absolute/relative frequency and marginal sum
ctaq12 <- Cct(da,2,"社区","性别")
ctaq21 <- Cct(da,2,"性别","社区")
ctaq13 <- Cct(da,2,"社区","态度")
ctaq31 <- Cct(da,2,"态度","社区")
ctaq23 <- Cct(da,2,"性别","态度")
ctaq32 <- Cct(da,2,"态度","性别")
ctrq12 <- Cct(da,2,"社区","性别",is.prop=T)
ctrq21 <- Cct(da,2,"性别","社区",is.prop=T)
ctrq13 <- Cct(da,2,"社区","态度",is.prop=T)
ctrq31 <- Cct(da,2,"态度","社区",is.prop=T)
ctrq23 <- Cct(da,2,"性别","态度",is.prop=T)
ctrq32 <- Cct(da,2,"态度","性别",is.prop=T)

# 1.3 three-way contingency/cross table with absolute/relative frequency and marginal sum
ctaq123 <- Cct(da,3,"社区","性别","态度")
ctrq123 <- Cct(da,3,"社区","性别","态度",is.prop=T)

# 1.4 output table
Scxlsx(ctaq1,"1.社区居民态度列联表","社区(频数)",isap=FALSE)
Scxlsx(ctrq1,"1.社区居民态度列联表","社区(频率)")
Scxlsx(ctaq2,"1.社区居民态度列联表","性别(频数)")
Scxlsx(ctrq2,"1.社区居民态度列联表","性别(频率)")
Scxlsx(ctaq3,"1.社区居民态度列联表","态度(频数)")
Scxlsx(ctrq3,"1.社区居民态度列联表","态度(频率)")
Scxlsx(ctaq12,"1.社区居民态度列联表","社区+性别(频数)")
Scxlsx(ctrq12,"1.社区居民态度列联表","社区+性别(频率)")
Scxlsx(ctaq13,"1.社区居民态度列联表","社区+态度(频数)")
Scxlsx(ctrq13,"1.社区居民态度列联表","社区+态度(频率)")
Scxlsx(ctaq23,"1.社区居民态度列联表","性别+态度(频数)")
Scxlsx(ctrq23,"1.社区居民态度列联表","性别+态度(频率)")
Scxlsx(ctaq123,"1.社区居民态度列联表","社区+性别+态度(频数)")
Scxlsx(ctrq123,"1.社区居民态度列联表","社区+性别+态度(频率)")


#————————————
# 2.Bar plot
#————————————

# ——2.0——
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
x123 <- Cct(da,3,"社区","性别","态度",is.mar=FALSE)
x231 <- Cct(da,3,"性别","态度","社区",is.mar=FALSE)
x312 <- Cct(da,3,"态度","社区","性别",is.mar=FALSE)

# ——2.1—— 
# Simple bar plot, used for one categorical/classified/qualitative variable,
# includes two types: the vertical and the horizontal,
# and pareto plot, named by Italian economist V.Pareto, is its variant

# 2.1.1 simple vertical bar plot
Png("2.1.1 简单垂直条形图 (simple vertical bar plot)")
par(mfrow=c(1,3))
bp1_1 <- barplot(x1,xlab="社区",ylab="频数",main="各社区人数分布条形图")
bp2_1 <- barplot(x2,xlab="性别",ylab="频数",main="各性别人数分布条形图")
bp3_1 <- barplot(x3,xlab="态度",ylab="频数",main="各态度人数分布条形图")
dev.off()

# 2.1.2 simple horizontal bar plot
Png("2.1.2 简单水平条形图 (simple horizontal bar plot)")
layout(matrix(c(1,2,3),nrow=3,ncol=1,byrow=T),heights=c(1.8,1.2,1.2))
bp1_2 <- barplot(x1,ylab="频数",xlab="频数",main="各社区人数分布条形图",horiz=TRUE)
bp2_2 <- barplot(x2,ylab="频数",xlab="频数",main="各性别人数分布条形图",horiz=TRUE)
bp3_2 <- barplot(x3,ylab="频数",xlab="频数",main="各态度人数分布条形图",horiz=TRUE)
dev.off()

# 2.1.3 pareto plot
Png("2.1.3 帕累托图（pareto plot）")
par(mai=c(1.5,1.5,1.5,1.5))
y1 <- cumsum(x1)/sum(x1)
bp1_3 <- barplot(x1,xlab="社区",ylab="频数",main="各社区人数分布帕累托图")
par(new=T)
plot(y1,type='b',lwd=1.5,pch=15,axes=FALSE,xlab=' ',ylab=' ',main=' ')
axis(4)
par(las=0)
mtext("累积频率",side=4,line=3)
mtext("累积分布曲线",line=-3.5,cex=0.8,adj=0.75)
dev.off()

# ——2.2—— 
# Double bar plot, used for two categorical/classified/qualitative variable,
# includes two types: the juxtaposed and the stacked,
# and spine plot can be viewd as the variant of stacked bar plot 

# 2.2.1 juxtaposed bar plot

Png("2.2.1.1 并列复式条形图 (juxtaposed double bar plot)")
par(mfrow=c(1,2))
bp12_1 <- barplot(x12,xlab="性别",ylab="频数",main="各性别不同社区人数分布并列条形图",beside=TRUE,legend=rownames(x12),args.legend=list(x=4,cex=0.9,bty='n'))
bp21_1 <- barplot(x21,xlab="社区",ylab="频数",main="各社区不同性别人数分布并列条形图",beside=TRUE,legend=rownames(x21),args.legend=list(x=11,cex=0.9,bty='n'))
dev.off()

Png("2.2.1.2 并列复式条形图 (juxtaposed double bar plot)")
par(mfrow=c(1,2))
bp13_1 <- barplot(x13,xlab="态度",ylab="频数",main="各态度不同社区人数分布并列条形图",beside=TRUE,legend=rownames(x13),args.legend=list(x=5,cex=0.9,bty='n'))
bp31_1 <- barplot(x31,xlab="社区",ylab="频数",main="各社区不同态度人数分布并列条形图",beside=TRUE,legend=rownames(x31),args.legend=list(x=12,cex=0.9,bty='n'))
dev.off()

Png("2.2.1.3 并列复式条形图 (juxtaposed double bar plot)")
par(mfrow=c(1,2))
bp23_1 <- barplot(x23,xlab="态度",ylab="频数",main="各态度不同性别人数分布并列条形图",beside=TRUE,legend=rownames(x23),args.legend=list(x=2,cex=0.9,bty='n'))
bp32_1 <- barplot(x32,xlab="性别",ylab="频数",main="各性别不同态度人数分布并列条形图",beside=TRUE,legend=rownames(x32),args.legend=list(x=2.2,cex=0.9,bty='n'))
dev.off()

# 2.2.2 stacked bar plot

Png("2.2.2.1 堆叠复式条形图 (stacked double bar plot)")
par(mfrow=c(1,2))
bp12_2 <- barplot(x12,xlab="性别",ylab="频数",main="各性别不同社区人数分布堆叠条形图",legend=rownames(x12),args.legend=list(x=0.85,y=45,cex=0.9,bty='n'))
bp21_2 <- barplot(x21,xlab="社区",ylab="频数",main="各社区不同性别人数分布堆叠条形图",legend=rownames(x21),args.legend=list(x=4.8,cex=0.9,bty='n'))
dev.off()

Png("2.2.2.2 堆叠复式条形图 (stacked double bar plot)")
par(mfrow=c(1,2))
bp13_2 <- barplot(x13,xlab="态度",ylab="频数",main="各态度不同社区人数分布堆叠条形图",legend=rownames(x13),args.legend=list(x=0.85,cex=0.9,bty='n'))
bp31_2 <- barplot(x31,xlab="社区",ylab="频数",main="各社区不同态度人数分布堆叠条形图",legend=rownames(x31),args.legend=list(x=4.8,cex=0.9,bty='n'))
dev.off()

Png("2.2.2.3 堆叠复式条形图 (stacked double bar plot)")
par(mfrow=c(1,2))
bp23_2 <- barplot(x23,xlab="态度",ylab="频数",main="各态度不同性别人数分布堆叠条形图",legend=rownames(x23),args.legend=list(x=0.65,cex=0.9,bty='n'))
bp32_2 <- barplot(x32,xlab="性别",ylab="频数",main="各性别不同态度人数分布堆叠条形图",legend=rownames(x32),args.legend=list(x=0.8,cex=0.9,bty='n'))
dev.off()

# 2.2.3 spine plot

library("vcd")

Png("2.2.3.1 脊形图（spine plot）")
bp12_3 <- spine(x21,xlab="性别",ylab="社区",main="各性别不同社区人数分布脊形图")
dev.off()

Png("2.2.3.2 脊形图（spine plot）")
bp21_3 <- spine(x12,xlab="社区",ylab="性别",main="各社区不同性别人数分布脊形图")
dev.off()

Png("2.2.3.3 脊形图（spine plot）")
bp13_3 <- spine(x31,xlab="态度",ylab="社区",main="各态度不同社区人数分布脊形图")
dev.off()

Png("2.2.3.4 脊形图（spine plot）")
bp31_3 <- spine(x13,xlab="社区",ylab="态度",main="各社区不同态度人数分布脊形图")
dev.off()

Png("2.2.3.5 脊形图（spine plot）")
bp23_3 <- spine(x32,xlab="态度",ylab="性别",main="各态度不同性别人数分布脊形图")
dev.off()

Png("2.2.3.6 脊形图（spine plot）")
bp32_3 <- spine(x23,xlab="性别",ylab="态度",main="各性别不同态度人数分布脊形图")
dev.off()

detach("package:vcd")

# ——2.4—— 
# Mosaic plot, used for three categorical/classified/qualitative variable,
# can be viewd as the variant of juxtaposed bar plot,
# and every blocks' size is proportional to the frequency 

Png("2.2.4.1 马赛克图（mosaic plot）")
bp123_1 <- mosaicplot(x123,xlab="社区",ylab="性别",main="各社区不同性别、不同态度人数分布马赛克图",col=c("grey50","grey80"))
dev.off()

Png("2.2.4.2 马赛克图（mosaic plot）")
bp213_1 <- mosaicplot(x213,xlab="性别",ylab="社区",main="各性别不同社区、不同态度人数分布马赛克图",col=c("grey50","grey80"))
dev.off()

Png("2.2.4.3 马赛克图（mosaic plot）")
bp312_1 <- mosaicplot(x312,xlab="态度",ylab="社区",main="各态度不同社区、不同性别人数分布马赛克图",col=c("grey50","grey80"))
dev.off()

#————————————
# 3.Pie chart
#————————————

# Most statisticians regard Pie chart as a terrible chart, 
# because people can estimate the length more correctly than the size.
# Therefore, they recommend barplot or point diagram to people instead of pie chart
# But in 2009, Lemon & Tyagi provided users with fan chart, a variant of pie chart.
# It can display the relative number and the relative difference simultaneously. 

# ——3.1——
# pie chart

Png("3.1 饼图（pie chart）")
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
pi1_1 <- pie(x1,labels=lbls1,main="各社区人数分布饼图",col=rainbow(length(lbls1)))
pi2_1 <- pie(x2,labels=lbls2,main="各性别人数分布饼图",col=rainbow(length(lbls2)))
pi3_1 <- pie(x3,labels=lbls3,main="各态度人数分布饼图",col=rainbow(length(lbls3)))
dev.off()

# ——3.2——
# fan chart

library("plotrix")

Png("3.2.1 扇图（fan chart）")
pi1_2 <- fan.plot(x1,labels=lbls1,main="各社区人数分布扇图")
dev.off()

Png("3.2.2 扇图（fan chart）")
pi2_2 <- fan.plot(x2,labels=lbls2,main="各性别人数分布扇图")
dev.off()

Png("3.2.3 扇图（fan chart）")
pi3_2 <- fan.plot(x3,labels=lbls3,main="各态度人数分布扇图")
dev.off()

detach("package:plotrix")


#——————————————
# 4.chisq.test
#——————————————

chisq.test(x12)
chisq.test(x13)
chisq.test(x23)
library(vcd)
assocstats(x12)
assocstats(x13)
assocstats(x23)
detach("package:vcd")