rm(list=ls()) #清空内存

金融数据的下载（load）、输出（output）及时序图的绘制（draw）
financial.iod <- function(financial.name,financial.symbol,start.time,end.time,outloc,sour="yahoo",is.nd = FALSE,is.Chinese = TRUE){

	##1.加载“quantmod”程序包（由 Jeffry 开发）	
	##利用该包，可从雅虎财经（yahoo）、谷歌财经（google）、圣路易斯联邦储备银行的联邦储备经济数据库（PRED）下载金融数据
	
	library(quantmod)
	
	##2.获取金融数据并计算（简单/对数）收益率
	
	#如获取美国国债数据，国债名前需加^
	if(is.nd == TRUE){
		financial.sy <- paste("^",financial.symbol,sep="")
	}else{
		financial.sy <- financial.symbol
	}
	
	#如获取国内股票数据，应将 getSymbols 选项先组合成一个列表
	if(is.Chinese == TRUE){
		setSymbolLookup(OBJ = list(name=financial.symbol,src="yahoo",from=start.time,to=end.time))
		getSymbols("OBJ")
	}else{
		getSymbols(financial.sy,src=sour,from=start.time,to=end.time)
		OBJ <- eval(parse(text = financial.symbol)) #将字符串转换成命令执行
	}
	
	#如数据来源为联邦储备经济数据库（FRED），将只有一列且列名为金融数据代码
	#如数据为股票数据，最后一列列名为 ”股票代码.Adjusted“
	if(sour == "FRED"){
		add <- NULL
	}else{
		add <- ".Adjusted"
	}
	
	OBJ.lgrtn <- diff(log(OBJ[,paste(financial.symbol,add,sep="")])) #对数收益率
	OBJ.rtn <- diff(OBJ[,paste(financial.symbol,add,sep="")]) #简单收益率
	
	##3.将金融数据及其收益率数据输出为一个csv文件

	da <- cbind(as.matrix(OBJ),as.matrix(OBJ.rtn),as.matrix(OBJ.lgrtn)) #将 xts、zoo 对象转换为 matrix 对象
	#更改原英文列名
	if(sour == "FRED"){
		colnames(da) <- c(financial.name,"简单收益率","对数收益率") 
	}else{
		colnames(da) <- c("开盘价","最高价","最低价","收盘价","成交量","调整后收盘价","简单收益率","对数收益率") 
	}
	write.csv(da,paste(outloc,financial.name,".csv",sep=""))

	##4.输出金融数据收盘价及成交量时序图,图形背景设为白色（默认为黑）
	##如无需成交量时序图，可添加选项 TA=NULL

	png(paste(outloc,financial.name,".png",sep=""),width=1024,height=768)
	chartSeries(OBJ,theme="white",name=paste(financial.name,"(","金融数据代码：",financial.symbol,")",sep=""))
	dev.off()
	
	##5.输出金融数据对数收益率时序图
	
	png(paste(outloc,financial.name,"（对数收益率）",".png",sep=""),width=1024,height=768)
	chartSeries(OBJ.lgrtn,theme="white",name=paste(financial.name,"对数收益率","(","金融数据代码：",financial.symbol,")",sep=""))
	dev.off()
	
	##6.输出金融数据简单收益率时序图
	
	png(paste(outloc,financial.name,"（收益率）",".png",sep=""),width=1024,height=768)
	chartSeries(OBJ.rtn,theme="white",name=paste(financial.name,"收益率","(","金融数据代码：",financial.symbol,")",sep=""))
	dev.off()
}

##上证（.SS）、深城（.SZ）、港股（.HK）
##601169.SS 为北京银行股票

financial.name <- "北京银行" #股票名
financial.symbol <- "601169.SS" #股票代码
start.time <- "2007-04-29" #起始日期
end.time <- "2017-04-29" #截止日期V
#outloc <- paste("D:\\kaoyan2\\统计学\\0.my note\\其他\\时间序列\\",financial.name,"\\",sep="")
outloc <- "D:\\kaoyan2\\统计学\\0.my note\\其他\\时间序列\\北京银行\\"
# sour <- "FRED" #数据来源，默认值为yahoo
# is.nd <- FALSE #是否为美国国债，默认值为FALSE
# is.chinese <- FALSE #是否为国内股票，默认为TRUE

# 函数格式 financial.iod(financial.name,financial.symbol,start.time,end.time,outloc,sour,is.nd,is.Chinese)

financial.iod(financial.name,financial.symbol,start.time,end.time,outloc)