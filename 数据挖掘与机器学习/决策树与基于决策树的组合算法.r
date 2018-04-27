rm(list=ls())
library(MASS)
data(biopsy)
biopsy <- biopsy[-1]
table(biopsy$class)
library(mice)
md.pattern(biopsy)#����ȱʧֵ����
set.seed(1234)
train <- sample(1:nrow(biopsy),round(0.8*nrow(biopsy)))#��ȡ80%��Ϊѵ����

library(tree)
bio.tree <- tree(formula=class~.-class, data=biopsy[train,])
summary(bio.tree)
bio.tree.pred <- predict(bio.tree, biopsy[-train,],type='class')
table(bio.tree.pred, biopsy[-train,'class'])

plot(bio.tree)
text(bio.tree)