##����˳��ͳ����(OS)�����λ������������
##����λ������������Ϊ��
##����һ�ֲ������ڷֲ���ͳ���ƶϷ���
##�ڷֲ���λ��֮ǰ������i�������㡢������j-1��������ĸ��ʵļ���ֻ�õ�����ֲ�����λ����Ӧ�ۻ�����0.5
##Ѱ��ʹ������ʴ��ڵ���95%�Ķ�Ӧ��i��˳��ͳ��������j��˳��ͳ����
##����Щ�õ���������Ѱ�����Ŷ���ӽ�95%�ĶԳ�������Ϊ���յĽ��


##dataΪԭʼ���ݣ�alphaΪ1-���Ŷȣ�quatile��Ӧ���蹹���quatile%��λ������������

conintervalbyos <- function(data,alpha,quatile){ 

  oraldata <- function(x)
  {
    a <- sort(data)          ###����oraldata����������Ϊx������
    a[x]
  }
  
  n <- length(data)

  for(k in 1:n) ##���������ɴ���ͳ�������ɵĶԳ����䣬�����������Ŷȴ��ڵ���1-alpha������
  {
    conf <- pbinom(n-k,n,quatile)-pbinom(k-1,n,quatile) #�������Ŷ�
    lower <- k #��������ͳ��������
    upper <- n-k+1 #��������ͳ��������
    coninterval <- c(oraldata(lower),oraldata(upper)) ##��oraldata��������ԭʼ���ݣ��õ���������
    if(conf>=1-alpha) 
    print(coninterval) #������Ŷȷ���Ҫ��ĶԳ���������  
  }

}
