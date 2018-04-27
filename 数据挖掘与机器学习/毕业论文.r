year <- function(x,p){
	v <- 93015*10000000
	a <- choose(x,p)
	y <- a/(v*60*60*24*365)
	y
}

year(10^3,8)*365
choose(1000,8)

rm(list=ls())

#高斯函数，用于计算[n/k]
gan <- function(x){
	if(round(x)>x){
		round(x)-1
	}else{
		round(x)
	}
}

#D(n,k)计算函数，1<k<n
prodv <- function(k,n){

	a <- c(n:(k+1)) #分子

	num <- gan(n/k) #[n/k]

	if(num>1){
		b <- c((num+1):2)
		d <- c(num:2) 
	}else{
		b <- 2
		d <- NULL
	} 
	b <- b^(n-k*num) #分母第一部分
	d <- d^(k-n+k*num) #分母第二部分

	b <- c(b,d) #分母

	#分子、分母向量长度统一化
	len <- abs(length(a)-length(b))
	if(length(a)>length(b)){
		b <- c(b,rep(1,len))
	}else{
		a <- c(a,rep(1,len))
	}

	b <- sort(b,decreasing=T) #分母向量递减排序

	#分两部分作除法，减少计算误差
	a1 <- a[which(a>=b)]
	b1 <- b[which(a>=b)]
	a2 <- a[which(a<b)]
	b2 <- b[which(a<b)]
	prod(a1/b1)/prod(b2/a2)

}

rs <- vector("list",98)
for(j in 3:100){
	n <- j #样本量
	Folds <- c(2:(n-1)) #折数
	D.N <- sapply(Folds,prodv,n=n) #固定样本量后，不同折数分法数
	fold <- Folds[order(D.N)] #折数按分法数递增排序
	if(j < 100){
		rs[[j-2]] <- c(fold,rep(0,98-length(fold)))
	}else{
		rs[[j-2]] <- fold
	}
}

RS <- matrix(unlist(rs),98,98,dimnames=list(c(),paste("n","=",c(3:100))))
RS[which(RS!=2 & RS!=3 & RS!=4 & RS!=5 & RS!=6 & RS!=7 & RS!=8 & RS!=9 & RS!=10 & RS!=11 & RS!=12 & RS!=13  & RS!=14 & RS!=15 & RS!=16 & RS!=17 & RS!=18 & RS!=19 & RS!=20,arr.ind=T)] <- 0

write.csv(RS,"D:\\毕业论文理论探讨2.csv")

#PRODV <- rep(0,n-2)
#for(k in 2:(n-1)){
#	PRODV[k-1] <- prodv(k,n)
#}
#PRODV



plot(Folds,D.N,xlab="折数",ylab="分法数",main=paste("n","=",n,seq=""))

pl <- function(n){
	c <- rep(0,n-2)
	for(i in 2:(n-1)){
		a <- prodv(n,i)$a
		b <- prodv(n,i)$b
		#c[i] <- prod(a/b)
		c[i] <- prod(a)/prod(b)
	}
	i <- 2:(n-1)
	plot(i,c[i])
}

pl(170)
prod(prodv(170,2)$b)
prod(prodv(170,2)$a)/prod(prodv(170,2)$b)
