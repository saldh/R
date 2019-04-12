a <- 0
for(i in 1:50){
  a[i]= i*2 +3
}
a

b <- 5
i <- 1
while (b[i]<121) {
  i = i + 1
  b[i]=b[i-1]+2
}
b[i]

x <- 8
if(x >= 10){
  x <- x + 10
}else{
    x <- x +5
}
x

#R向量化编成
Data1 <- read.csv('d:/share/manifest.csv')
str(Data1)
#一般编程思路代码
NANum <- length(Data1[is.na(Data1)])
totalNum <- nrow(Data1)*ncol(Data1)
perNA <- NANum/totalNUM
#向量化编程思路
perNA <- mean(is.na(Data1))#求缺失值比例
perNA

a <- c(T,F,T,F)
a
as.numeric(a)
mean(a)
mean(as.numeric(a))
#数据DATA2中，如果x<=1或x>=8,则将data中的这些x值替换为NA
Data2 <- c(rep(4,3),1:11,rep(5,3),5:10)
Data2
#一般编程思路
for(i in 1:length(Data2)){
  if(Data2[i] <= 3 | Data2[i] >= 8){
    Data2[i] <- NA
  }
}
Data2
#向量化编程
NAIDX <- which(Data2[i] <= 3 | Data2[i] >= 8)
Data2[NAIDX] <- NA
Data2
#
expr <- function(x){
  if(x >= 10){
    x <- x + 10
  }else{
      x <- x + 5
  }
  return(x)
}
expr(x <- 20)
