rm(list=ls())

Wyygdcl <- function(yhm){

	wjdqlj <- paste("D:\\���ݿ�ѧ��Data Science��\\5.��������\\�����Ƹ赥\\r",yhm,".csv",sep="")
	wjsclj <- paste("D:\\���ݿ�ѧ��Data Science��\\5.��������\\�����Ƹ赥\\",yhm,".csv",sep="")

	###����ֱ�Ӹ��Ƶ�����excel�������Ƹ赥����2018.03.29 15:44
	###���������������⣬����������

	##���������������֡�������
	##��ƴ��һ���Ӣ�İ��մ�д��ĸ�ֿ����罫"SomethingJustLikeThis"�ֿ���"Something Just Like This"
	Dxfc <- function(x){
		if(gsub("[[:punct:]]|[[:space:]]|[[:upper:]]|[[:lower:]]","",x) == "")
		{
			x.sp <- unlist(strsplit(x,"")) #��Ӣ�ķֳɵ�����ĸ
			dxlc <- which(!is.na(match(x.sp,toupper(letters)))) #ԭӢ���д�д��ĸ��λ��
			#toupper��Сд��ĸת��Ϊ��д��letters��R�����еĳ�������26����ĸ��ɵ�����
			if(length(dxlc)==0){
				x #���û�д�д��ĸ������ԭ�ַ�
			}else{
				if(dxlc[1]==1){
					dc <- substring(x,dxlc,c(dxlc[-1]-1,nchar(x))) #����д��ĸ�ֳɶ���ַ���
				}else{
					dc <- substring(x,c(1,dxlc),c(dxlc-1,nchar(x))) #����д��ĸ�ֳɶ���ַ���		
				}
				paste(dc,collapse=" ") #��ϵ���
			}
		}else{
			x #���ȥ����㡢�հס���д/Сд��ĸ����ַ����Բ�Ϊ�գ��򷵻�ԭ�ַ�
			#[[:punct:]]��[[:space:]]��[[:upper:]]��[[:lower:]]Ϊͨ���������ȫ����㡢ȫ���հס�ȫ����д��ĸ��ȫ��Сд��ĸ
		}	
	}


	##��������������򡪡�����

	da <- read.csv(wjdqlj,as.is=TRUE) #��ȡ�赥

	#���ƹ������������Ч�У����ɲ�һ������������
	a <- which(da[,1]!="")
	b <- c(a[-1],nrow(da)+1)-a
	dshh1 <- c(a+1,a[which(b==5)]+3,a[which(b==7)]+3,a[which(b==7)]+5) 

	da <- da[-dshh1,]
	a <- which(da[,1]!="")
	b <- c(a[-1],nrow(da)+1)-a

	#��ϸ��ƹ����б���ֵ����ݣ����Ƶ�excel���ʱ��һ�׸�ռ�ݼ��У�
	for(i in a){
		if(any(i==a[which(b==4)])){
			da[i,] <- paste(da[i,],da[i+1,],da[i+2,],da[i+3,])
		}else{
			if(any(i==a[which(b==3)])){
				da[i,] <- paste(da[i,],da[i+1,],da[i+2,])
			}else{
				da[i,] <- paste(da[i,],da[i+1,])
			}
		}	
		da[i,] <- gsub(" ","",da[i,])
		da[i,] <- sapply(strsplit(as.character(da[i,]),"?",fixed=TRUE),paste,collapse="")
	}

	#ɾ��ʣ�����õ���
	dshh2 <- which(da[,1]=="")
	da <- da[-dshh2,]

	#ɾ�������е�MV����
	da[,2] <- gsub("MV","",da[,2])

	#��Ӣ������һ��ĸ����ִʣ����޴�Ӣ�ĸ�����
	for(i in 2:ncol(da)){
		da[,i] <- sapply(da[,i],Dxfc)
	}

	#���������ĸ赥���������С���ף����Ѵ�Ϊ�Ĺۣ�
	write.csv(da,wjsclj,row.names=FALSE)	
	
}

fn <- list.files("D:\\���ݿ�ѧ��Data Science��\\5.��������\\�����Ƹ赥\\")
fn <- fn[which(substring(fn,1,1)=='r')]
yhmlb <- substring(fn,2,nchar(fn)-4)

for(i in yhmlb){
	Wyygdcl(i)
}
