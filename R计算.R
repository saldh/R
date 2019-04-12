#加速R语言矩阵计算
mat <- matrix(1:(4000*4000),4000)
system.time(temp <- mat%%mat)
openBLAS
#寻找列表元素的缺失值
a <- list(a = c(1,2),b = c(3),c = c(NA,NA) ,d = c(4,5))
a
which(is.na(a))

B <- lapply(a,mean,na.rm=T)
which(is.na(B))
#质数、最大公约数、最小公倍数
is.prime <- function(num){
  if (num ==2){
    TRUE
  }else if (any(num %% 2:(num-1) == 0)){
    FALSE
  }else{
    TRUE
  }
}

is.prime(num=11)
#或者
is.prime <- function(n){
  n ==2L || all(n %% 2L:floor(sqrt(n) != 0))
}
is.prime(n=17)
#外包函数
install.packages('gmp')
library(gmp)
is.prime = isprime(1:10)
data.frame(number = 1:10, is.prime = isprime(1:10) > 1)
#最大公约数、最小公倍数
library(gmp)
a = 12
b = 4 
gcd(a,b)
lcm.bigz(a,b)

#向量的加减乘除
A <- c(1:10);A
B <- c(1,3,13,18);B
#方法，循环加减
mat <- matrix(NA,length(A),length(B))
for(i in 1:length(A)){
  mat[i,] <- A[i] +B
}
mat
#转置相乘
t(A)
t(t(A))%% B
#数组外积
outer(A,B,'+')
outer(A,B,'-')
outer(A,B,'*')
outer(A,B,'/')
#性能比较
A <- c(1:10000)
B <- c(1:10000)
system.time(mat <- outer(A,B,'*'))
system.time(mat <- t(t(A)) %% B)

#对话框交互
winDialog(type='yesnocancel', message = 'calculation correct')
winDialogString(message ='input parameter',default = '')
#type='ok','okcancel','yesno','yesnocancel'
Result <- winDialogString(message = '1+2=?', default='')
if(as.numeric(Result)==3){
  winDialog(type = 'ok',message = 'Calculation correct')
}else{
  winDialog(type = 'ok' ,message = 'calculation error')
}

#数值向量中的频数
Vector <- c(2,3,4,65,78,4,2,4,65,7,43,23,43,54,6,7,4,2)
hist(Vector,breaks = 10)

freNum <- function(Vector){
  Table <- table(Vector)
  numeric <- as.numeric(Table)
  onlyone <- unique(numeric)
  ID <- which(numeric==max(onlyone))
  freqNum <- as.numeric(names(Table[ID]))
  return(freqNum)
}
freNum(Vector = Vector)

#数据中心话 标准化 极差标准化
Data <- c(1,2,3,4,5,3);Data
scale(Data, center = T,scale = F)
#数据标准化：（x-mean)/sd
scale(Data,center = T,scale = T)
(Data-min(Data))/(max(Data)-min(Data))
MinMaxScale <- function(x){
  Range <- range(x)
  centers <- Range[1]
  scales <- Range[2] - Range[1]
  x <- scale(x,center = centers,scale = scales)
  return(x)
}
MinMaxScale(x = Data)
#计算天数

numberofdays1 <- function(date){
  m <- format(data,format = '%m')
  day31 <- c('01','03','05','07','08','10','12')
  day30 <- c('04','06','09','11')
  day28 <- '02'
  if(m %in% day31) return(31)
  if(m %in% day30) return(30)
  if(m %in% day28) return(28)
}
data = as.Date('2004-08-09','%Y-%m-%d')
data = as.Date('2016-02-07','%Y-%m-%d')
numberofdays1(date)
#2
numberofdays2 <- function(date){
  m <- format(date,format = '%m')
  while (format(date,format= '%m') == m){
    date <- date + 1
  }
  return(as.integer(format(date -1,format = '%d')))
}
data = as.Date('2004-08-09','%Y-%m-%d')
data = as.Date('2016-02-07','%Y-%m-%d')
numberofdays2(date)

