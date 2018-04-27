rm(list=ls())

Wyygdcl <- function(yhm){

	wjdqlj <- paste("D:\\数据科学（Data Science）\\5.数据资料\\网易云歌单\\r",yhm,".csv",sep="")
	wjsclj <- paste("D:\\数据科学（Data Science）\\5.数据资料\\网易云歌单\\",yhm,".csv",sep="")

	###修正直接复制到本地excel的网易云歌单——2018.03.29 15:44
	###程序初步解决了问题，但并不完美

	##————函数部分————
	##对拼在一起的英文按照大写字母分开，如将"SomethingJustLikeThis"分开成"Something Just Like This"
	Dxfc <- function(x){
		if(gsub("[[:punct:]]|[[:space:]]|[[:upper:]]|[[:lower:]]","",x) == "")
		{
			x.sp <- unlist(strsplit(x,"")) #将英文分成单个字母
			dxlc <- which(!is.na(match(x.sp,toupper(letters)))) #原英文中大写字母的位置
			#toupper将小写字母转换为大写，letters是R语言中的常量，是26个字母组成的向量
			if(length(dxlc)==0){
				x #如果没有大写字母，返回原字符
			}else{
				if(dxlc[1]==1){
					dc <- substring(x,dxlc,c(dxlc[-1]-1,nchar(x))) #按大写字母分成多个字符串
				}else{
					dc <- substring(x,c(1,dxlc),c(dxlc-1,nchar(x))) #按大写字母分成多个字符串		
				}
				paste(dc,collapse=" ") #组合单词
			}
		}else{
			x #如果去掉标点、空白、大写/小写字母后的字符串仍不为空，则返回原字符
			#[[:punct:]]、[[:space:]]、[[:upper:]]、[[:lower:]]为通配符，代表全部标点、全部空白、全部大写字母、全部小写字母
		}	
	}


	##————主体程序————

	da <- read.csv(wjdqlj,as.is=TRUE) #读取歌单

	#复制过程中引入的无效行，规律不一定具有普适性
	a <- which(da[,1]!="")
	b <- c(a[-1],nrow(da)+1)-a
	dshh1 <- c(a+1,a[which(b==5)]+3,a[which(b==7)]+3,a[which(b==7)]+5) 

	da <- da[-dshh1,]
	a <- which(da[,1]!="")
	b <- c(a[-1],nrow(da)+1)-a

	#组合复制过程中被拆分的内容（复制到excel表格时，一首歌占据几行）
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

	#删掉剩余无用的行
	dshh2 <- which(da[,1]=="")
	da <- da[-dshh2,]

	#删掉歌名中的MV字样
	da[,2] <- gsub("MV","",da[,2])

	#对英文连在一起的歌名分词（仅限纯英文歌名）
	for(i in 2:ncol(da)){
		da[,i] <- sapply(da[,i],Dxfc)
	}

	#输出修正后的歌单（仍有许多小不妥，但已大为改观）
	write.csv(da,wjsclj,row.names=FALSE)	
	
}

fn <- list.files("D:\\数据科学（Data Science）\\5.数据资料\\网易云歌单\\")
fn <- fn[which(substring(fn,1,1)=='r')]
yhmlb <- substring(fn,2,nchar(fn)-4)

for(i in yhmlb){
	Wyygdcl(i)
}
