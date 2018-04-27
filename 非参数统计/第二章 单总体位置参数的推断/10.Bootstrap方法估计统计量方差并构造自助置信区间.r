#Bootstrap����ͳ�����ķ��������λ��Ϊ��

conintervalbyboot <- function(mydata,N,n,stat,alpha)
 
#mydataΪԭʼ�������ݣ�NΪ�س�������
#statΪҪ���Ʒ����ͳ��������������������x���ĺ����������ȸ���

{
  x <- mydata

  STATISTICS <- NULL  #����ͳ�����洢��������ֵΪ��
  SD.STATISTICS <- NULL  #����ͳ������������׼��洢��������ֵΪ��
  SAMPLESIZE <- NULL  #����ͳ�������������洢��������ֵΪ��

  for(mu in 1:N) #ѭ��N�Σ�����N������ͳ�����������ǵı�׼����Ϊ��ͳ������׼��Ĺ���
  {
    xr <- sample(x,n,T) 
    #��ԭʼ���������зŻ��س������õ�һ����Ϊn��������

    Statistics <- stat(xr) 
    #������������ͳ����
    STATISTICS <- c(STATISTICS,Statistics) 
    #������õ���������ͳ������������ͳ�����洢����

    samplesize <- mu 
    #������ͳ������������mu��ֵ��samplesize
    SAMPLESIZE <- c(SAMPLESIZE,samplesize) 
    #��samplesizeֵ��������ͳ�������������洢����

    sd.Statistics <- sd(STATISTICS) 
    #��������ͳ�����洢�����ı�׼��
    SD.STATISTICS <- c(SD.STATISTICS,sd.Statistics) 
    #������õ��ı�׼���������ͳ������׼��洢����
  }
  
  originalstat <- stat(x)
  
  Norm.interval.lower <- originalstat-qnorm(alpha/2,0,1,F)*sd.Statistics
  Norm.interval.upper <- originalstat+qnorm(alpha/2,0,1,F)*sd.Statistics
  Norm.interval <- c(Norm.interval.lower, Norm.interval.upper)
  #������̬�ֲ���������������

  Pivotal.interval.lower <- 2*originalstat-quantile(STATISTICS,1-alpha/2)
  Pivotal.interval.upper <- 2*originalstat-quantile(STATISTICS,alpha/2)
  Pivotal.interval <- c(Pivotal.interval.lower, Pivotal.interval.upper)
  #������������������������

  Quatile.interval.lower <- quantile(STATISTICS,alpha/2)
  Quatile.interval.upper <- quantile(STATISTICS,1-alpha/2)
  Quatile.interval <- c(Quatile.interval.lower, Quatile.interval.upper)
  #��������ͳ������λ����������������

  hist(STATISTICS, border = T, col = "gray") 
  #���ѭ��N�κ�õ�����������ͳ������ֱ��ͼ
  plot(SAMPLESIZE,SD.STATISTICS) 
  #�������ͳ������׼��������ͳ�������������ı仯ͼ
  list(STATISTICS = originalstat, sd.Statistics = sd.Statistics, Norm.interval = Norm.interval, Pivotal.interval = Pivotal.interval, Quatile.interval = Quatile.interval ) 
  #���б���ʽ��ʾԭʼ����ͳ����ֵ�������������Ƶĸ�ͳ�����ķ���
  
}

mydata <- c(as.numeric(apply(read.table("nerve.txt"),2,as.numeric)))
#��ȡ�ı����ݲ�ʵ�ֶ��ı����ݵ�ǿ����ֵ��

stat <- function(x){median(x)} #ָ��ͳ����Ϊ��λ��

#mydata <- c(0.424136656,0.111067449,0.045779206,-0.089951921,-0.838992649,
#-0.416450927,-0.45805788,0.21670288,0.479832185,0.525935001)

conintervalbyboot(mydata,1000,40,stat,0.05) #ѭ��1000��