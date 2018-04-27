#����Uͳ�����������ݷֲ��ĶԳ���(���ƹ㵽���ⵥ��������)
#����ԭ���ǣ����������������һ���ԳƷֲ�������P��2X1>X2+X3)-P(2X1<X2+X3)=0
#P��2X1>X2+X3)-P(2X1<X2+X3)��һ�����ɶ�Ϊ3�Ŀɹ����������Ϊsgn��2X1-X2-X3)
#����˹���Գƺ�h��X1��X2��X3)=(1/3)*[sgn��2X1-X2-X3)+sgn��2X2-X1-X3��+sgn��2X3-X1-X2)]
#������Գƺ�ʵ���ϵ���X1��X2��X3����λ�����ֵ֮��ķ��ź�����1/3
#�ɸöԳƺ����ǿ��Թ����Uͳ����������n��/[3!*(n-3)!]���Գƺ˵�ƽ��
#����Uͳ��������������֪����������ݷ��ӶԳƷֲ�����n�㹻��ʱ��Uͳ���������Ʒ����ھ�ֵΪ0����̬�ֲ�

rm(list=ls())##����ڴ�
ExamsymbyUSTAT <- function(mydata,N) #NΪUͳ��������,N-1Ϊ�س�������
{
  USTAT <- NULL #�洢Uͳ��������������ֵΪ��

  for(mu in 1:N-1)
  {
    x <- mydata #ʵ������
    n1 <- length(x) #ʵ�����ݵ�������

    if(mu>1){x <- sample(x,n1,T)} #�س���

    H <- NULL #�洢�Գƺ˵���������ֵΪ��

    for(i in 1:(n1-2))
    for(j in (i+1):(n1-1))
    for(k in (j+1):n1)
    {
      a1 <- sign(2*x[i]-x[j]-x[k])
      a2 <- sign(2*x[j]-x[i]-x[k])
      a3 <- sign(2*x[k]-x[i]-x[j])
      h <- (1/3)*(a1+a2+a3) #����Գƺ�
      H <- c(H,h)
    }

    Ustat <- (1/choose(length(x),3))*sum(H) #����Uͳ����
    USTAT <- c(USTAT,Ustat)
    
  }

  hist(USTAT, border = T, col = "gray") #Uͳ����ֱ��ͼ

  Umean <- mean(USTAT)
  Uvar <- var(USTAT)
  Pvalue <- 2*pnorm(abs(Umean/sqrt(Uvar/N)),0,1,F)

  if(Pvalue < 0.05)
  {print("���㹻֤�ݱ������ݲ����ӶԳƷֲ�")}
  else{print("û��֤�ݱ������ݲ����ӶԳƷֲ�")}

  list(Umean = Umean, Uvar = Uvar, Pvalue = Pvalue) #���n��Uͳ�����ľ�ֵ�ͷ��� 
}

mydata <- c(25,26,28,31,32,32,32,33,35,35,35,36,36,40,41,87)
###�������Էǲ���ͳ�Ƶڶ�����2.1��excel������ʾ��������ƫ����̬

ExamsymbyUSTAT(mydata,10000)