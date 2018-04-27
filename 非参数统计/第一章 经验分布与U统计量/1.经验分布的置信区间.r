#�ǲ���ͳ�Ƶ�һ��ʵ��1��������ֲ�����������
library(MASS) #����MASS�����
data(geyser) #��������geyser

duration <- geyser[,2] #��geyser���ݼ��е�2�����ݸ���duration����
duration.sort <- sort(duration) #��duration������Ԫ������������
duration.rank <- rank(duration.sort) #���������duration�����ڸ�Ԫ�ص���
duration.cdf <- duration.rank/length(duration) #��duration�����ľ���ֲ�����ֵ

plot(duration.sort, duration.cdf) #��duration������x���뾭��ֲ�����ֵ��y���Ķ�άɢ��ͼ
N <- length(duration) #��duration�������ȸ�������N
segments(duration.sort[1:(N-1)], duration.cdf[1:(N-1)],
duration.sort[2:N], duration.cdf[1:(N-1)]) 
#ͨ��������߶Σ��������duration��������ֲ�����ͼ�εĻ���

alpha <- 0.5 #��������ֲ�����������������Ŷ�Ϊ95%����0.5��ֵ��alpha
band <- sqrt(1/(2*N))*log(2/alpha) #����ֲ����������������ĳ��ȣ���������ֵ����band
Lower.95 <- duration.cdf - band #����ֲ��������������ޣ���������ֵ����Lower.95
Upper.95 <- duration.cdf + band #����ֲ��������������ޣ���������ֵ����Upper.95
lines(duration.sort, Lower.95, lty=2) #��ԭ����ֲ�����ͼ���ϻ����������ޣ����ߣ�
lines(duration.sort, Upper.95) #��ԭ����ֲ�����ͼ���ϻ����������ޣ�ʵ�ߣ�

alpha2 <- seq(0.01, 0.1, by=0.001) #��0.01Ϊ��ʼֵ��0.1Ϊ���ֵ��0.001Ϊ�������ɵȲ����н������и���alpha
band2 <- 2*sqrt(1/(2*N))*log(2/alpha2) #���������䳤�ȸ���band2#
plot(alpha2,band2)#����1-���Ŷȣ���alpha��������������֮��Ĺ�ϵ