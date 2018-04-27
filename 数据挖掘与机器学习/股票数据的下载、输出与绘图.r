rm(list=ls()) #清空内存

##1.加载“quantmod”程序包（由 Jeffry 开发）
##利用该包，可从雅虎财经（yahoo）、谷歌财经（google）、圣路易斯联邦储备银行的联邦储备经济数据库（PRED）下载金融数据

library(quantmod)

##2.设定股票名（name）、获取来源（src）、起止时间(from,to)
##上证（.SS）、深城（.SZ）、港股（.HK）
##601169.SS 为北京银行股票

setSymbolLookup(BJYH = list(name="601169.SS",src="yahoo",from="2014-04-02",to="2017-04-29"))

##3.获取股票数据

getSymbols("BJYH")

##4.将股票数据输出为一个csv文件

da <- as.matrix(BJYH) #将 xts、zoo 对象转换为 matrix 对象
#更改原英文列名
colnames(da) <- c("开盘价","最高价","最低价","收盘价","成交量","调整后收盘价") 
write.csv(da,"D:\\kaoyan2\\统计学\\0.my note\\其他\\时间序列\\北京银行.csv")

##5.输出股票数据收盘价及成交量时序图,图形背景设为白色（默认为黑）
##如无需成交量时序图，可添加选项 TA=NULL

png("D:\\kaoyan2\\统计学\\0.my note\\其他\\时间序列\\北京银行.png")
chartSeries(BJYH,theme="white")
dev.off()