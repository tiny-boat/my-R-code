##������Wilcoxon�����ȼ��鵼����Hodges-Lemmann���������ƶԳƷֲ�������λ��
##����������еõ���������������ƽ����ɵĳ���Ϊn(n+1)/2�������ݳ�ΪWalshƽ��ֵ
##����֤��Wilcoxon������ͳ����W+�͵���ȡֵΪ����Walshƽ��ֵ�ĸ���
##������ͬ�ֲ�����������һ���ԳƷֲ�����walshƽ��ֵ����λ����Ϊ�ԳƷֲ����ĵ�Hodges-Lemmann������

a <- c(62,70,74,75,77,80,83,85,88)
walsh <- NULL
for(i in 1:length(a))
{
  for(j in i:length(a))
  walsh <- c(walsh,(a[i]+a[j])/2)
}
median(walsh)
