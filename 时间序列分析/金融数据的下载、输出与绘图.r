rm(list=ls()) #����ڴ�

�������ݵ����أ�load���������output����ʱ��ͼ�Ļ��ƣ�draw��
financial.iod <- function(financial.name,financial.symbol,start.time,end.time,outloc,sour="yahoo",is.nd = FALSE,is.Chinese = TRUE){

	##1.���ء�quantmod����������� Jeffry ������	
	##���øð����ɴ��Ż��ƾ���yahoo�����ȸ�ƾ���google����ʥ·��˹��������е�������������ݿ⣨PRED�����ؽ�������
	
	library(quantmod)
	
	##2.��ȡ�������ݲ����㣨��/������������
	
	#���ȡ������ծ���ݣ���ծ��ǰ���^
	if(is.nd == TRUE){
		financial.sy <- paste("^",financial.symbol,sep="")
	}else{
		financial.sy <- financial.symbol
	}
	
	#���ȡ���ڹ�Ʊ���ݣ�Ӧ�� getSymbols ѡ������ϳ�һ���б�
	if(is.Chinese == TRUE){
		setSymbolLookup(OBJ = list(name=financial.symbol,src="yahoo",from=start.time,to=end.time))
		getSymbols("OBJ")
	}else{
		getSymbols(financial.sy,src=sour,from=start.time,to=end.time)
		OBJ <- eval(parse(text = financial.symbol)) #���ַ���ת��������ִ��
	}
	
	#��������ԴΪ������������ݿ⣨FRED������ֻ��һ��������Ϊ�������ݴ���
	#������Ϊ��Ʊ���ݣ����һ������Ϊ ����Ʊ����.Adjusted��
	if(sour == "FRED"){
		add <- NULL
	}else{
		add <- ".Adjusted"
	}
	
	OBJ.lgrtn <- diff(log(OBJ[,paste(financial.symbol,add,sep="")])) #����������
	OBJ.rtn <- diff(OBJ[,paste(financial.symbol,add,sep="")]) #��������
	
	##3.���������ݼ����������������Ϊһ��csv�ļ�

	da <- cbind(as.matrix(OBJ),as.matrix(OBJ.rtn),as.matrix(OBJ.lgrtn)) #�� xts��zoo ����ת��Ϊ matrix ����
	#����ԭӢ������
	if(sour == "FRED"){
		colnames(da) <- c(financial.name,"��������","����������") 
	}else{
		colnames(da) <- c("���̼�","��߼�","��ͼ�","���̼�","�ɽ���","���������̼�","��������","����������") 
	}
	write.csv(da,paste(outloc,financial.name,".csv",sep=""))

	##4.��������������̼ۼ��ɽ���ʱ��ͼ,ͼ�α�����Ϊ��ɫ��Ĭ��Ϊ�ڣ�
	##������ɽ���ʱ��ͼ�������ѡ�� TA=NULL

	png(paste(outloc,financial.name,".png",sep=""),width=1024,height=768)
	chartSeries(OBJ,theme="white",name=paste(financial.name,"(","�������ݴ��룺",financial.symbol,")",sep=""))
	dev.off()
	
	##5.����������ݶ���������ʱ��ͼ
	
	png(paste(outloc,financial.name,"�����������ʣ�",".png",sep=""),width=1024,height=768)
	chartSeries(OBJ.lgrtn,theme="white",name=paste(financial.name,"����������","(","�������ݴ��룺",financial.symbol,")",sep=""))
	dev.off()
	
	##6.����������ݼ�������ʱ��ͼ
	
	png(paste(outloc,financial.name,"�������ʣ�",".png",sep=""),width=1024,height=768)
	chartSeries(OBJ.rtn,theme="white",name=paste(financial.name,"������","(","�������ݴ��룺",financial.symbol,")",sep=""))
	dev.off()
}

##��֤��.SS������ǣ�.SZ�����۹ɣ�.HK��
##601169.SS Ϊ�������й�Ʊ

financial.name <- "��������" #��Ʊ��
financial.symbol <- "601169.SS" #��Ʊ����
start.time <- "2007-04-29" #��ʼ����
end.time <- "2017-04-29" #��ֹ����V
#outloc <- paste("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\ʱ������\\",financial.name,"\\",sep="")
outloc <- "D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\ʱ������\\��������\\"
# sour <- "FRED" #������Դ��Ĭ��ֵΪyahoo
# is.nd <- FALSE #�Ƿ�Ϊ������ծ��Ĭ��ֵΪFALSE
# is.chinese <- FALSE #�Ƿ�Ϊ���ڹ�Ʊ��Ĭ��ΪTRUE

# ������ʽ financial.iod(financial.name,financial.symbol,start.time,end.time,outloc,sour,is.nd,is.Chinese)

financial.iod(financial.name,financial.symbol,start.time,end.time,outloc)