#计算整数向量中奇数的个数

oddcount <- function(x) {
  k <- 0
  for(n in x) {
    if (n %% 2 ==1) k <- k+1
  }
  return(k)
}