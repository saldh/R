install.packages('SelectorGadget')
v=c(1,2,3,4,4,5,5,6,3)
v[c(2,3,4)]
v[2:4]
v[-2:-4]
which(v==3)
which.max(v)
set.seed(250)
a = runif(3,min=0,max=100)
floor(a)
ceiling(a)
round(a,4)
?set.seed
?rnorm()
rnorm(3)
#数据输入
data=read.csv(file='~/documents')
data2=read.table()
data3=read.csv('http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv')
View(data3)
attach(data3)
#graphs画图技巧
set.seed(123)
x=rnorm(100,mean=100,sd=10)
set.seed(234)
y=rnorm(100,mean = 100,sd=10)
hist(x,breaks = 20,col = 'blue')
plot(density(x))#密度图
plot(x,type = 'l')
plot(x,type = 'o')
boxplot(x,y)#箱图
boxplot(time~sex)
?qqnorm
qqnorm(x)
qqline(x)
qqplot(data)
#matrix
x=matrix(1:20,nrow = 5,ncol = 4,byrow = TRUE)
x
y=matrix(1:20,nrow = 5,ncol = 4,byrow = FALSE)
y
x[2,c(2,4)]
rnames=c('apple','banana','orange','melon','corn')
cnames=c('cat','dog','bird','pig')
x=matrix(1:20,nrow = 5,ncol = 4,byrow=TRUE)
rownames(x)=rnames
colnames(x)=cnames
x
#array
dim1=c('a1','a2')
dim2=c('b1','b2','b3')
dim3=c('c1','c2','c3','c4')
dim4=c('d1','d2','d3')
z=array(1:72,c(2,3,4,3),dimnames = list(dim1,dim2,dim3,dim4))
z
#data frame
patientID=c(1,2,3,4)
age=c(25,34,35,64)
diabetes=c('tape1','type2','type1','type2')
status=c('poor','improved','excellent','poor')
patientdata=data.frame(patientID,age,diabetes,status)
patientdata
plot(patientdata)
#attach
attach(mtcars)
layout(matrix(1,1,2,3),2,2,byrow=TRUE)
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#list
mylist = list(patientdata,x)
mylist
mylist[(1)]
#graphs
par(mfrow=c(2,2))#设定2×2网格，可以同时画4个图
plot(rnorm(50),pch=17)#pch符号
plot(rnorm(20),type = 'l',lty=5)#lty线形
plot(rnorm(100),cex=0.5)
plot(rnorm(200),lwd=2)#线粗细
#layout
attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#list
x=matrix(1:20,nrow = 5,ncol = 4,byrow = TRUE)
swim=read.csv('http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv')
mylist=list(swim,x)
mylist
mylist[[2]][2]
mylist[[2]][1:2,3]#第二部分第三列一到二
#operators > >= <= == != x|y 或 x&y and
#for loop
for (i in 1:10) {
  print(i)
  
}
#while loop
i=1
while (i<=10) {
  print(i)
  i=i+1
  
}
i=1
if(i==1){
  print('hello world')
}
i=2
if(i==1){
  print('hello')
}else{
  print('goodbye')
}
 feelings=c('sad','afraid')
 for (i in feelings) {
   print(
     switch(i,
            happy='i am glad you are happy',
            afraid='there is nothing to fear',
            sad='cheer up',
            angry='calm down'
            )
   )
 }
#user defined function
 myfunction=function(x,a,b,c){
   return(a*sin(x)^2 -b*x +c)
 }
 curve(myfunction(x,20,3,4),xlim = c(1,20))
curve(exp(x),xlim=c(1,20)) 

