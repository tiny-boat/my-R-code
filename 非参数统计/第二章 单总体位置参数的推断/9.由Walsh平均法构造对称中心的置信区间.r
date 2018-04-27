##����Walshƽ��ֵ����Գ����ĵ���������
##��Ϊ(n+1)n/2��Walshƽ��ֵ����λ���ǶԳ����ĵ�һ������
##��˿���Ӧ��Walshƽ��ֵ��˳��ͳ��������Գ����ĵ���������
##����ԭ����ֱ����������˳��ͳ��������Գ����ģ���λ��������������һ�£��������ֻ�����ʵ��޸�
##�����ʾʹ��Walshƽ������������������ֱ����˳��ͳ����������������侫ȷ

##dataΪԭʼ���ݣ�alphaΪ1-���Ŷȣ�quatile��Ӧ���蹹���quatile��λ������������

conintervalbywalsh <- function(data,alpha){ 

  walsh <- NULL
  N <- length(data)
  for(i in 1:N)
  for(j in i:N)
  {
    a <- (data[i]+data[j])/2  ###��Walshƽ��ֵ����n(n+1)/2��
    walsh <- c(walsh,a)
  }

  hist(walsh,border=T,col="gray")    

  oralwalsh <- function(x)
  {
    a <- sort(walsh)          ###����oralwalsh����������Ϊx������
    a[x]
  }
  
  n <- length(walsh)

  for(k in 1:n) ###���������ɴ���ͳ�������ɵĶԳ����䣬�����������Ŷȴ��ڵ���1-alpha������
  {
    conf <- pbinom(n-k,n,0.5)-pbinom(k-1,n,0.5) ###�������Ŷ�
    lower <- k ###��������ͳ��������
    upper <- n-k+1 ###��������ͳ��������
    coninterval <- c(oralwalsh(lower),oralwalsh(upper)) ###��oralwalsh��������ԭʼ���ݣ��õ���������
    if(conf>=1-alpha) 
    print(coninterval) ###������Ŷȷ���Ҫ��ĶԳ���������  
  }

}

data <- c(9.5, 14, 12, 21, 7.5, 9.5, 2, 17.5, 7.5, 14, 17.5, 24, 26, 19.5, 5.5,
1, 27, 16, 25, 14, 19.5, 22.5, 3, 4, 22.5, 11, 5.5)

conintervalbywalsh(data,0.05)