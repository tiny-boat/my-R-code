##清空内存中所有对象

rm(list=ls())

#————————————————————————————————————————————————————————————————————————————

##1.R语言强大绘图功能演示

demo(graphics) 
demo(Hershey)
demo(persp)
demo(image)
demo()

#————————————————————————————————————————————————————————————————————————————

##2.获取帮助的有关命令，这里一些较熟悉的命令被注释掉了

#help()
#?help
#?"for"
#example("function")
#help.search("foo")
#??"foo"

help.start() #打开帮助文档首页
RSiteSearch("foo") #以“foo”为关键词搜索在线文档和邮件列表存档

apropos("foo",mode="function") #列出名称中含有foo的所有可用函数

data() #列出当前所加载包中所有可用的示例数据集
vignette() #列出当前所加载包中所有可用的简介（vignette）文档
vignette("foo") #为主题“foo”显示指定的简介文档

#————————————————————————————————————————————————————————————————————————————

##3.管理R工作空间和命令历史的函数

getwd() #显示当前工作目录（work directory）
setwd() #更改当前工作目录（work directory）

ls() #列出当前工作空间中的所有对象（object）
rm(objectlist) #移除一个或多个对象

options() #显示或设置当前选项
help(options) #显示可用选项含义

history(n) #显示最近使用过的n个命令，默认值为25
savehistory("command") #保存命令历史到command文件中，默认保存到.Rhistory文件
loadhistory("command") #载入命令历史文件command，默认载入.Rhistory文件

save.image("space") #保存工作空间到文件space中，默认保存到.RData文件
save(objectlist,file="myfile") #保存指定对象或对象列表到myfile文件中
load("myfile") #加载工作空间myfile到当前会话中，默认加载.RData文件

q() #退出R，询问是否保存工作空间

#————————————————————————————————————————————————————————————————————————————

##4.R语言代码的输入，分析的文本结果与图形结果的输出

source("myfile.r",echo=TRUE) #执行myfile文件中的R代码

sink("myfile.doc",append=TRUE,split=TRUE) #位于分析代码之前，表示其下代码的输出结果不仅显示在R控制台上，也将被写入word文档myfile中。append如为FALSE，分析结果将覆盖文档myfile中的原内容；split如为FALSE，分析结果将不会显示在R控制台上。sink可以称之为文本结果输出函数，可供选择的文本类型有.doc,.xls,.ppt,.txt,……（这4种文件类型时我暂时尝试的一些能够打开的文件类型，是否还支持其它类型不得而知，但pdf肯定不行）。如果分析的文本结果中有表格，建议输出为.xls文件，然后再利用excel的分列功能，这样省去自己绘制表格的工作。当然，.txt、.doc也是较好的输出格式，看个人习惯，但不要使用.ppt格式。

pdf("myfile.pdf")
png("myfile.png")
jpeg("myfile.jpeg")
bmp("myfile.bmp")
postscript("myfile.postscript")
#以上五行命令皆是图形结果输出命令，表示其下代码的图形输出将以pdf/png/jpeg/bmp/postscript格式保存，文件名为myfile

win.metafile("myfile")
#其下代码的图形输出将保存为windows图元文件，文件名为myfile

#————————————————————————————————————————————————————————————————————————————

##5.包的安装

install.packages("") #注意不能少了引号