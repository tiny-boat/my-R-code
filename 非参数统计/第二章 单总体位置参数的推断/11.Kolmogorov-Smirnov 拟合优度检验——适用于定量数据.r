#Kolmogorov-Smirnov Goodness-of-fit Test ��Ī�����˹���������Ŷȼ���
#Dn��Kolmogorovͳ����������Ϊ����ֲ������������ۻ��ֲ�����֮�����ȷ��
#ע�⣺���¼�������ʹ���˼��޷ֲ������ʺ���С����

Kolmogtest <- function(data,CDF,alpha) ##CDFΪ������ļ��������ֲ�cdf,alphaΪ������ˮƽ
{
  datarank <- rank(data,ties.method="max") ##�����ݵ���,�ظ�����ȡ�����
  datasize <- length(data)
  EDF <- datarank/datasize ##���㾭��ֲ�����
  D <- max(abs(EDF-CDF(data)))
  Dn <- sqrt(datasize)*D

  ##����Kolmogorovͳ����

  Koldis <- function(x) ##Kolmogorovͳ�����ļ����ۻ��ֲ�����
  {
    DIS <- 0
    for(j in 1:100000) ##��ʵ������һ��������ͣ�������100000����
    {
      dis <- (-1)^(j-1) * exp(-2 * j^2 * x^2)
      DIS <- DIS + dis
    }
    koldis <- 1-2*DIS  ##Kolmogorovͳ�������޷ֲ����ʽ�μ�����ͳ��ѧӢ�Ľ̲����һ�µ�����
  }

  Pvalue <- 1-Koldis(Dn)
  if(Pvalue < alpha){result <- c(c("���㹻֤����Ϊ���ݲ����Ӽ���ķֲ�"),c("�����������ˮƽΪ������˵�ܾ�ԭ���裬��Ϊ���ݲ����Ӽ���ֲ��ķ�����ʲ�����"),c(alpha))}
  else{result <- c(c("û���㹻֤����Ϊ���ݲ����Ӽ���ķֲ�"),c("�����������ˮƽΪ"),c(alpha))}
  list("������"=datasize,"����ֲ������ֲ�������D"=D,"Kolmogorovͳ����Dn"=Dn,"Pֵ"=Pvalue,"����"=result)
}

data <- c(87,77,92,68,80,78,84,77,81,80,80,77,92,86,76,80,81,75,77,72,81,72,84,86,80,68,77,87,76,77,78,92,75,80,78)##��������
CDF <- function(x)
{
  pnorm(x,80,6)##��������ֲ�Ϊ��ֵ80����׼��6����̬�ֲ�
}
alpha <- 0.05
Kolmogtest(data,CDF,alpha)

