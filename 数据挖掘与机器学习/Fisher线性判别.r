rm(list=ls()) #����ڴ�

###Fisher �����б��� Fisher ��1936������������������ʵ�� Fisher �����б�
###������ı�дû�е����������ȫʹ�� R �����ú���ʵ�֣�
###������ʹ�õ��б�׼�� �ȼ��� ����Э����������µ����Ͼ����б�
###��������ܴ���δ֪©�����ڴ���һ�����Ż�

###������� row.data��new.sample �ĸ�ʽҪ�����£�
###1.��֤���߱�ͷһ�£���Ӧ����Ҫ��2��Ҫ��3�е�һ����
###2.��1�д洢��Ʒ������2�д洢��Ʒ�����Ϣ�������д洢������Ϣ��
###3.��1�д洢��Ʒ�����Ϣ�������д洢������Ϣ��
###4.�������������Ϣ��ȱ���ɣ��������������Ϣһ�У�
###5.����ȱʧֵ���쳣ֵ�����ȶ�������Ԥ����
###6.����Ʒ������Ʒ�����Ϣ��������Ϣ�ⲻ���������������ݡ�


###һ��ģ�͵Ĵ

DC <- function(row.data,rd.name=TRUE){

	#�����1��Ϊ��Ʒ����ɾ����1��
	if(rd.name==TRUE){
		data <- row.data[,-1]	
	}else{
		data <- row.data
	}

	group.mean <- aggregate(data,list(class=data$G),mean)[,-1] #���ֵ (�����*(p+1)���ݿ�ȥ����һ������Ϊ��һ�������õ�class��)
	total.mean <- apply(data[,-1],2,mean) #�ܾ�ֵ (pά�����Ƶ�double������)
	p <- ncol(data)-1  #������ (1άdouble������)
	G.number <- as.vector(table(data$G))  #�����Ƶ�� (pάinteger������)

	##1.�������ƽ������ SSE �ļ��㺯�� (ʹ���� EDEV1 ����)
	SSE <- function(da){
		
		#EDEV1 ������Ϊ edev �ļ��㶨�Ƶ�
		EDEV1 <- function(x){
			x[-1] <- x[-1]-as.numeric(subset(group.mean,G==x[1]))[-1]
			#������Ʒ�����
			x[-1] %*% t(x[-1])
			#���������������������ĳ˻����õ�������Ʒ�����ƽ������
		}

		edev <- apply(da,1,EDEV1) 
		#ÿ����Ʒ�����ƽ������ʵ��Ϊ1�У����ɵ� ������ƽ��*��Ʒ�� ����
		sse <- apply(edev,1,sum) 
		#ÿ����Ʒ���ƽ������ �ӺͶ��� �������ƽ������ʵ��Ϊ������

		matrix(sse,p,p,byrow=T) #������ sse ת���ɾ���

	}

	##2.������ƽ������ SSA �ļ��㺯�� (ʹ���� EDEV2 ����)

	SSA <- function(da){

		#EDEV2 ������Ϊ edev �ļ��㶨�Ƶ�
		EDEV2 <- function(x){
			x <- as.matrix(x) #���ݿ��ܽ��о���˷�����
			x[-1] <- x[-1]-total.mean
			#����������
			x[-1] %*% t(x[-1])
			#���������������������ĳ˻����õ�����������ƽ������
		}

		edev <- t(t(apply(da,1,EDEV2))*G.number) 
		#ÿ��������ƽ������ʵ��Ϊ1�У����ɵ� ������ƽ��*���� ����
		ssa <- apply(edev,1,sum)
		#ÿ��������ƽ������ �ӺͶ��� �������ƽ������ʵ��Ϊ������

		matrix(ssa,p,p,byrow=T) #������ ssa ת���ɾ���

	}

	##3.Fisher �����б�����ϵ������ (ʹ���� SSE��SSA ����)
	
	A <- SSE(data) #�������ƽ������A 
	B <- SSA(group.mean) #������ƽ������B
	Ai.B <- solve(A) %*% B #�������ƽ������A���� �� ������ƽ������B���� �ĳ˻�
	Eig <- eigen(Ai.B) #���� A^(-1)*B ����������ֵ����������
	
	dcf <- cumsum(Eig$values)*100/(cumsum(Eig$values)[p]) #�ۻ��б���
	
	list("������"=p,"������"=nrow(row.data),"�����Ƶ��(ע���Ƿ�������������ٵ����)"=table(data$G),"���ֵ(���ֵ������������죬�����б������������)"=group.mean,"�ܾ�ֵ"=total.mean,"�ۻ��б���(%��ǰ������Ӧ���㹻���ˣ�����˵������������Խ�С)"=dcf,"��������(�б�����ϵ��)"=Eig$vectors)
}

###����ģ�͵�Ԥ��

PRDC <- function(row.data,new.sample,rd.name=TRUE,ns.name=TRUE,us.npr=FALSE,cro.val=FALSE){

	if(ns.name==TRUE){
		sample <- new.sample[,-1]
	}else{
		sample <- new.sample
	}
	
	DC <- DC(row.data,rd.name) #��ģ����б�
	p <- DC$������
	
	# m Ϊ��Ʒ����k ����� k �� Fisher �����б���
	DF <- function(m,k){
		apply(t(m)*DC$��������[,k],2,sum) #��Ʒ���б���ֵ
	}

	##4.Fisher �����б������(ʹ���� DF��LC ����)

	dfvalue.G <- DF(DC$���ֵ[,-1],1) #������� ��һ �б���ֵ
	dfvalue.N <- DF(sample[,-1],1) #����Ʒ����� ��һ �б���ֵ
    
	#��Ʒ���ĸ�������б���ֵ�ӽ�������Ϊ�ĸ�����
	LC <- function(x){
		bias <- abs(x-dfvalue.G) #������Ʒ�б���ֵ �� ���������б���ֵ �� ���Բ�
		loc <- which(bias==min(bias)) #���� ��С���Բ� ����λ�ã�λ��i��Ӧ����i��
		if(length(loc)==1){loc}else{0} 
		#�������λ�ò�ֹ1�����������б����޷�������Щ��Ʒ��������0�����򷵻�ԭλ��
	}

	preG <- sapply(dfvalue.N,LC) #��ÿ������Ʒ�� ��һ �б���ֵ Ӧ���б�׼���� �õ��б���

	#�Ե� mu-1 ���б���ֵ�޷����ֵ���Ʒ ʹ�õ� mu ���б����õ��б���
	
	dcn <- 1
	for(mu in 2:p){
		if(any(preG)==0){
			loc <- which(preG==0) #�� (mu-1)�� �б���ֵ�޷����ֵ���Ʒ���
			dfvalue.G <- DF(DC$���ֵ[,-1],mu) #������ĵ� mu ���б���ֵ
			dfvalue.N <- DF(sample[loc,-1],mu) #����Ʒ����ĵ� mu ���б���ֵ
			preG[loc] <- sapply(dfvalue.N,LC)
			dcn <- dcn + 1
		}else{
			break #��ֹѭ��
		}
	}

	if(us.npr==TRUE){
	
        if(ns.name==TRUE){
			new.sample[,2] <- preG
		}else{
			new.sample[,1] <- preG
		}
		
		new.sample
		
	}else{
	
		errs <- mean(sign(abs(preG-sample$G))) #�����
		
		if(cro.val==FALSE){
		
			if(ns.name==TRUE){
				rs <- cbind(new.sample[,1],sample,preG)
			}else{
				rs <- cbind(sample,preG)
			} #���ԭ���ݿ����б���
			
			Con.matrix <- xtabs(~rs$G+rs$preG) #�����б�Ч�����
			
			if(all(row.data[,3] == new.sample[,3])==TRUE){
				list("ʹ���б����ĸ���(������̫�࣬Ӧ���¿��Ǳ�����ʹ�������б�)"=dcn,"�������б���"=rs,"�����ڻ�������"=Con.matrix,"�����ڴ����"=errs)
			}else{
				list("ʹ���б����ĸ���(������̫�࣬Ӧ���¿��Ǳ�����ʹ�������б�)"=dcn,"�б���"=rs,"��������"=Con.matrix,"�����"=errs)
			}
			
		}else{
			return(errs)
		}	
	}
}


###����ģ�͵Ľ�����֤�����

#k�۽�����֤�����㺯����daΪ���ݣ�kΪ�۴Σ�errfuncΪ�����㺯����dΪ������ӵ㣩
Cro.val.err <- function(da,k,errfunc,d=1){
	
	#����������ֳɵȴ�С������Ƶȴ�С����k��
	n <- nrow(da) #��������
	ss1 <- if((round(n/k)-(n/k))>0){round(n/k)-1}else{round(n/k)} #��һ���ֳ���������[n/k]
	ss2 <- ss1+1 #�ڶ����ֳ���������[n/k]
	k1 <- k-(n-k*ss1) #��һ���ֳ���������
	train <- vector("list",k) #k��������������Ŵ洢�б�
	sn <- c(1:n) #�������������
	set.seed(d) #�������ӵ�
	if(k1 == k){
		for(j in 1:(k-1)){
			train[[j]] <- sample(sn,ss1,F) #��j��������������ţ�����train�б�ĵ�j�����
			sn <- setdiff(sn,train[[j]]) #ʣ��Ŀɳ��������
		}
		train[[k]] <- sn
	}else{
		for(j in 1:k1){
			train[[j]] <- sample(sn,ss1,F) #��j��������������ţ�����train�б�ĵ�j�����
			sn <- setdiff(sn,train[[j]]) #ʣ��Ŀɳ��������
		}
		if(k1==k-1){
			train[[k]] <- sn
		}else{
			for(j in (k1+1):(k-1)){
				train[[j]] <- sample(sn,ss2,F) #��j��������������ţ�����train�б�ĵ�j�����
				sn <- setdiff(sn,train[[j]]) #ʣ��Ŀɳ��������
			}
			train[[k]] <- sn	
		}
	}

	#����k�۽�����֤ƽ�����
	error <- rep(0,k)
	for(i in 1:k){
		trda <- da[-train[[i]],] #ѵ������
		teda <- da[train[[i]],] #��������
		error[i] <- errfunc(trda,teda) #�������
	}
	mean(error)#ƽ���������
}


###����ģ�͵�Ӧ��

##1.���������趨

row.data <- read.csv("D:\\kaoyan2\\ͳ��ѧ\\0.my note\\�б����2.csv")#��ģ��������

simu.ns <- row.data[c(4,9,12),]
simu.ns[,1] <- 0
rownames(simu.ns) <- NULL
new.sample <- simu.ns #����������(ע���˴�Ϊģ����������ע�����)

k <- 8 #�۴�

#���������㺯����ʹ��PRDC������PRDC����cro.val������ΪT
#��ظ���ʵ����������Ƿ�Ը������㺯�������޸�
errfunc <- function(trda,teda){
    PRDC(trda,teda,rd.name=FALSE,ns.name=FALSE,cro.val=TRUE)
} 

dc.model <- DC(row.data,rd.name=FALSE) #��ģ��Ϣ�б�

cro.val.err <- Cro.val.err(row.data,k,errfunc) #������֤ƽ�����

pred.row.data <- PRDC(row.data,row.data,rd.name=FALSE,ns.name=FALSE) #������Ԥ���б�, ��Ʒ��us.npr=F��ΪĬ�ϲ�������������

pred.new.sample <- PRDC(row.data,new.sample,rd.name=FALSE,ns.name=FALSE,us.npr=TRUE) #������Ԥ����, ������us.npr=T

##2.������

rs <- dc.model
rs[["������֤�����(ģ��ѡ�����Ҫָ��)"]] <- cro.val.err
rs[9:12] <- pred.row.data 
attributes(rs)$names[9:12] <- attributes(pred.row.data)$names 
rs$������Ԥ���� <- pred.new.sample
rs$���� <- c("���� preo���鿴������������Ʒ���б���","���� pren���鿴������������Ʒ���б���","���� look(preo,��Ʒ��),�鿴������ĳ����Ʒ���б���","���� look(pren,��Ʒ��),�鿴������ĳ����Ʒ���б���","���� other���鿴������Ϣ","ע������ ��Ʒ�� ʱҪ��˫���ţ�������Ʒ���У�������Ʒ���")  

preo <- rs[[10]] #������Ԥ����
pren <- rs[[13]] #������Ԥ����
other <- rs[c(1,2,5,11:12)]

#���� ������(way=preo)/������(way=pren) ��ĳ����Ʒ��Ԥ����
look <- function(way,sn){
	if(colnames(way)[1]=="G"){
		way[sn,]
	}else{
		way[way[,1]==sn,]
	}
}

rs[c(3:4,6:9,14)] #����б�Ĳ�����Ҫ��Ϣ