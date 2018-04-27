rm(list=ls()) #����ڴ�

##������������7�������е�7�������ı�дʹ���˵�1��2��4����������5��6�������ı�дʹ���˵�3��������

#1.ƫ�ȼ��㺯��
Ske <- function(da){
    n <- length(da)
	mea <- mean(da)
	dev <- sd(da)
	sum((da-mea)^3)/((n-1)*dev^3)
}

#2.�����ȼ��㺯��
Kur <- function(da){
    n <- length(da)
	mea <- mean(da)
	dev <- sd(da)
	sum((da-mea)^4)/((n-1)*dev^4)-3
}

#3.˫������б�׼��̬ͳ������pֵ���㺯��
Snorm.p <- function(stat,n=4,sci=F){
    format(round(2*min(1-pnorm(stat),pnorm(stat)),n),scientific=sci)
} #Ĭ�ϱ���4λС�����Ҳ�ʹ�ÿ�ѧ������ʽ

#4.t���麯��
Ttefunc <- function(da,ob){
    Ttest <- t.test(da)
    Ttep <- format(round(Ttest$p.value,4),scientific=F)
	#pֵ����4λС���Ҳ�ʹ�ÿ�ѧ������
	ifelse(Ttep < 0.05,paste("pֵ","=",Ttep,",","��5%��������ˮƽ�£���Ϊ",ob,"�ľ�ֵ������0"),paste("pֵ","=",Ttep,",","��5%��������ˮƽ�£�û��֤�ݱ���",ob,"�ľ�ֵ������0"))
}

#5.ƫ�ȼ��麯��
Stefunc <- function(da,ob){
	ske <- Ske(da)
	n <- length(da)
	stat <- ske/sqrt(6/n)
	Step <- Snorm.p(stat)
	ifelse(Step < 0.05,paste("pֵ","=",Step,",","��5%��������ˮƽ�£���Ϊ",ob,"��ƫ�Ȳ�����0"),paste("pֵ","=",Step,",","��5%��������ˮƽ�£�û��֤�ݱ���",ob,"��ƫ�Ȳ�����0"))
}

#6.��ȼ��麯��
Ktefunc <- function(da,ob){
	kur <- Kur(da)
	n <- length(da)
	stat <- kur/sqrt(24/n)
	Ktep <- Snorm.p(stat)
	ifelse(Ktep < 0.05,paste("pֵ","=",Ktep,",","��5%��������ˮƽ�£���Ϊ",ob,"�ķ�Ȳ�����3"),paste("pֵ","=",Ktep,",","��5%��������ˮƽ�£�û��֤�ݱ���",ob,"�ķ�Ȳ�����3"))
}

#ͳ��ָ����㺯��
Statfunc <- function(da){
    c(mean(da),sd(da),Ske(da),Kur(da),min(da),max(da))
}

#7.��һ���������ú���
Myfunc <- function(data1,number){
    data1 <- data1[,-1] #ȥ����1�У�����
    data2 <- log(data1+1) #�� �������� ת��Ϊ ����������

	a <- names(data1) #�����ݿ�������ֵ��a
	coln <- c("��ֵ","��׼��","ƫ��","������","��Сֵ","���ֵ")

	n <- nrow(data1)

	#��1�ʣ��������ʸ���ͳ��ָ�����
	result1 <- apply(data1,2,Statfunc)
    rownames(result1) <- coln
    colnames(result1) <- c(a[1],a[2],a[3],a[4])

	#��2�ʣ����������ʸ���ͳ��ָ�����
	result2 <- apply(data2,2,Statfunc)
    rownames(result2) <- coln
    colnames(result2) <- c(paste("log(",a[1],"+1",")"),paste("log(",a[2],"+1",")"),paste("log(",a[3],"+1",")"),paste("log(",a[4],"+1",")"))

	#��3�ʣ����������ʾ�ֵ��t����
	da <- data2[,1]
	ob <- ifelse(number == 1,"AXP��Ʊ����������","GE��Ʊ����������")
	result3 <- Ttefunc(da,ob)

	#������
	list("���"=paste("��",number,"��"),"�������ʵ�ͳ��ָ��"=result1,"���������ʵ�ͳ��ָ��"=result2,"���������ʵľ�ֵ������"=result3)
}


##��һ�⣨ʹ�ú���7��
da1 <- read.table("F:\\ʱ�����з���\\chapter1\\ch1data\\d-axp3dx-0111.txt",header=T) #��������
nu1 <- c(1) #���
R1 <- Myfunc(da1,nu1) #����һ�������浽����R1��

##�ڶ��⣨ʹ�ú���7��
da <- read.table("F:\\ʱ�����з���\\chapter1\\ch1data\\m-ge3dx-40112.txt",header=T) #��������
da$sp <- as.numeric(as.character(da$sp)) #��������sp����ǿ��ת��Ϊ��ֵ�ͱ���
da2 <- da
nu2 <- c(2)
R2 <- Myfunc(da2,nu2) #���ڶ��������浽����R2��

##�����⣨ʹ�ú���4��5��6��
da <- da2[,5]
ob <- c("S&P�¹�Ʊ������")
rs1 <- Ttefunc(da,ob)
rs2 <- Stefunc(da,ob)
rs3 <- Ktefunc(da,ob)
R3 <- list("���"=paste("��","3","��"),"S&P�¹�Ʊ�����ʾ�ֵ������"=rs1,"S&P�¹�Ʊ������ƫ�ȼ�����"=rs2,"S&P�¹�Ʊ�����ʷ�ȼ�����"=rs3)#�������������浽����R3��

##�����⣨ʹ�ú���5��6��
da <- log(da1[,2]+1)
ob <- c("��ͨ��˾(AXP)����������")
rs1 <- Stefunc(da,ob)
rs2 <- Ktefunc(da,ob)
R4 <- list("���"=paste("��","4","��"),"��ͨ��˾(AXP)����������ƫ�ȼ�����"=rs1,"��ͨ��˾(AXP)���������ʷ�ȼ�����"=rs2)#�������������浽����R3��

##���н�����
R1
R2
R3
R4