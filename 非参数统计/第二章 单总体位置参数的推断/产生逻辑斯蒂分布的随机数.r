a <- rlogis(100, location = 0, scale = 1)
write.csv(a,file="rlogis.csv")
hist(a,border=T,col="gray")