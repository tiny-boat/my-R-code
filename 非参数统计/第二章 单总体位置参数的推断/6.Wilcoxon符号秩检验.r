##wilcoxon符号秩检验
##数据为19家大型公司的CEO的邮箱里每天收到的垃圾邮件数，检验垃圾邮件数量的中心位置是否超过320封
##数据直方图显示没有明显证据拒绝对称分布的假设，因此使用wilcoxon符号秩检验

spammail <- c(310,350,370,377,389,400,415,425,440,295,325,296,250,340,298,365,375,360,385)
hist(spammail,border=T,col="gray")
wilcox.test(spammail-320)

##符号检验
binom.test(sum(spammail>320),length(spammail),0.5)

##虽然在显著性水平0.1下两个检验均拒绝了原假设，但是由于wilcoxon检验增加了对称这个有效信息
##其p值明显小于符号检验的p值