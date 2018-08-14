a <- 10
a <- 'abc'
a <- TRUE
a <- vector(mode="logical", length=5)
a <- matrix(1:20,nrow=5,ncol=4,byrow=T)
is.matrix(a)
#查看或设置数组的维度向量
dim(a)
a <- 1:20
dim(a) <- c(5,4)#转换向量为矩阵
a
print(paste("矩阵a的行数", nrow(a)))
print(paste("矩阵a的列数", ncol(a)))
#查看或设置行列名
rownames(a)
rownames(a) <- c('a','b','c','d','e')
a
#R中获取一系列的字母
letters[1:4]
colnames(a) <- letters[1:4]
a
# is系列和as系列函数用来判断变量的属性和转换变量的属性
# 矩阵转换为data.frame
is.character(a)
is.numeric(a)
is.matrix(a)
is.data.frame(a)
is.data.frame(as.data.frame(a))
#R中的矩阵计算
#数据产生
#rnoorm(n,mean=0,sd=1)正态分布的随机数
#runif(n,min =0 ,sd =1)平均分布的随机数
#rep(1,5)把1重复5次
#scale(1:5)标准化数据
a <- c(rnorm(5), rnorm(5,1), runif(5), runif(5,-1,1), 1:5, rep(0,5), c(2,10,11,13,4), scale(1:5)[1:5])
a
a <- matrix(a, ncol = 5 ,byrow = T)
a
#求行的加和
rowSums(a)
#去除全部为0的行
a <- a[rowSums(abs(a))!=0,]
##a[rowSums(a==0)<ncol(a),]
a
# 矩阵运算，R默认针对整个数据进行常见运算
# 所有值都乘以2
a * 2
# 所有值取绝对值，再取对数 （取对数前一般加一个数避免对0或负值取对数）
log2(abs(a)+1)
#取出最大值、最小值、行数、列数
max(a)
min(a)
nrow(a)
ncol(a)
#增加一列或一行
#cbind:column bind
cbind(a,1:7)
cbind(a,seven=1:7)
#rbind:row bind
rbind(a,1:5)
# 计算每一行的mad (中值绝对偏差，一般认为比方差的鲁棒性更强，更少受异常值的影响，更能反映数据间的差异
apply(a,1, mad)
#计算每一行的var方差
#apply表示对数据（每一个参数）的每一行（第二个参数赋值为1）或每一列（2)操作,最后返回一个列表
apply(a,1,var)
#计算每一列的平均值
apply(a,2,mean)
#取出中值绝对偏差大于0.5的行
b = a[apply(a,2,mad)>0.5,]
b
#矩阵按照mad的大小降序排列
c = b[order(apply(b,1,mad),decreasing=T),]
c
rownames(c) <- paste('Gene', letters[1:7], sep="_")
colnames(c) <- toupper(letters[1:5])
c
#矩阵转换
expr = t(c)
expr
#矩阵值得替换
expr2 =expr
expr2[expr2<0]=0
expr2
#矩阵中只针对某一列替换、
#expr2是矩阵而不是数据框，不能使用列名字索引
# str是一个最为常用、好用的查看变量信息的工具，尤其是对特别复杂的变量
#可以看清层级结构，便于提取数据
str(expr2) 
#转换为数据库
expr2 <- as.data.frame(expr2)
str(expr2)
expr2[expr2$Gene_b<1, "Gene_b"] <- 1
expr2
#R中矩阵筛选合并
#读入样品信息
sampleInfo = "Samp;Group;Genotype
+ A;Control;WT
+ B;Control;WT
+ D;Treatment;Mutant
+ C;Treatment;Mutant
+ E;Treatment;WT
+ F;Treatment;WT"
phenoData = read.table(text=sampleInfo,sep=";", header=T, row.names=1, quote="")
phenoData
# 把样品信息按照基因表达矩阵中的样品信息排序，并只保留有基因表达信息的样品
# match() returns a vector of the positions of (first) matches of  its first argument in its second.
phenoData[match(rownames(expr), rownames(phenoData)),]
# ‘%in%’ is a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not for its left operand.
# 注意顺序，%in%比match更好理解一些
phenoData = phenoData[rownames(phenoData) %in% rownames(expr),]
phenoData
# 合并矩阵
# by=0 表示按照行的名字排序
# by=columnname 表示按照共有的某一列排序
# 合并后多出了新的一列Row.names
merge_data = merge(expr, phenoData, by=0, all.x=T)
merge_data
rownames(merge_data) <- merge_data$Row.names
merge_data
#去除一列；-1表示去除第一列
merge_data = merge_data[,-1]
merge_data
#提取所有数值列
merge_data[sapply(merge_data, is.numeric)]
#str用来告诉结果的构成方式，对于不少Bioconductor的包，或者复杂的R函数的输出，都是一堆列表的嵌套，str(complex_result)会输出每个列表的名字，方便提取对应的信息。
str(list(a = "A", L = as.list(1:100)), list.len = 9)
# 利用str查看pca的结果，具体的PCA应用查看http://mp.weixin.qq.com/s/sRElBMkyR9rGa4TQp9KjNQ
pca_result <- prcomp(expr)
pca_result
str(pca_result)
# 取出每个主成分解释的差异
pca_result$sdev
