rm(list=ls()) #����ڴ�

##1.���ء�quantmod����������� Jeffry ������
##���øð����ɴ��Ż��ƾ���yahoo�����ȸ�ƾ���google����ʥ·��˹��������е�������������ݿ⣨PRED�����ؽ�������

library(quantmod)

##2.�趨��Ʊ����name������ȡ��Դ��src������ֹʱ��(from,to)
##��֤��.SS������ǣ�.SZ�����۹ɣ�.HK��
##601169.SS Ϊ�������й�Ʊ

setSymbolLookup(BJYH = list(name="601169.SS",src="yahoo",from="2014-04-02",to="2017-04-29"))

##3.��ȡ��Ʊ����

getSymbols("BJYH")

##4.����Ʊ�������Ϊһ��csv�ļ�

da <- as.matrix(BJYH) #�� xts��zoo ����ת��Ϊ matrix ����
#����ԭӢ������
colnames(da) <- c("���̼�","��߼�","��ͼ�","���̼�","�ɽ���","���������̼�") 
write.csv(da,"D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\ʱ������\\��������.csv")

##5.�����Ʊ�������̼ۼ��ɽ���ʱ��ͼ,ͼ�α�����Ϊ��ɫ��Ĭ��Ϊ�ڣ�
##������ɽ���ʱ��ͼ�������ѡ�� TA=NULL

png("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\����\\ʱ������\\��������.png")
chartSeries(BJYH,theme="white")
dev.off()