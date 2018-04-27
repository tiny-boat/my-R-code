##利用由Wilcoxon符号秩检验导出的Hodges-Lemmann估计量估计对称分布的中心位置
##简单随机样本中得到的任意两个数的平均组成的长度为n(n+1)/2的新数据称为Walsh平均值
##可以证明Wilcoxon符号秩统计量W+就等于取值为正的Walsh平均值的个数
##若独立同分布样本来自于一个对称分布，则walsh平均值的中位数即为对称分布中心的Hodges-Lemmann估计量

a <- c(62,70,74,75,77,80,83,85,88)
walsh <- NULL
for(i in 1:length(a))
{
  for(j in i:length(a))
  walsh <- c(walsh,(a[i]+a[j])/2)
}
median(walsh)
