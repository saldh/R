#install.packages('psych')
library(psych)
otu = read.table('otu_table.txt',header = T,row.names = 1)
otu
#计算otu间相关系数矩阵
occor = corr.test(otu,use = 'pairwise',method ='spearman',adjust = 'fdr',alpha = 0.05)
#取相关性矩阵R值
occor.r = occor$r
#取相关性矩阵p值
occor.p = occor$p
# 确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0
#将occor.r保存为csv
write.csv(occor.r,file="FH_CK_0.05_occor.csv")
install.packages('gcookbook')

R语言数据可视化教程（ggplot2）_数据塑形
# 15 数据塑型
# 在将数据转化为图形之前，需要对数据进行清理然后重新组织数据的结构
# R中的数据集常以数据框的形式存在。它们都是点形的二维数据结构，
# 每行代表一个具体对象（case）,每列代表一个描述对象的变量。
# 数据框本质上是由向量和因子组成的列表，其中每个向量或因子代表了数据的一列
library(gcookbook)
heightweight
str(heightweight)
# 因子和字符型向量在ggplot2中的处理方式相类似——主要区别在于，展示字符型向量代表的项目时，它们以字母表的顺序排列，而因子型的项目是按因子水平的顺序排列，这个顺序是可以由用户控制的。

# 15.1 创建数据框
# 把向量放置data.frame()里面
# 从两个简单的向量开始
g <- c("A","B","C")
x <- 1:3
dat <- data.frame(g,x)
dat
# 数据框本质上是由一堆向量和因子构成的列表，其中每个向量或者因子代表了一列。
# 如果向量在一个列表中，可以用as.data.frame()函数直接将它们转化成数据框
lst <- list(group = g,value=x) # 由向量组成的列表
lst
dat <- as.data.frame(lst)
dat
# 15.2 从数据框中提取信息
# 使用str()函数
str(ToothGrowth)
# 运行str()函数可以帮助分辨哪些是字符型向量，哪些是因子。
tg <- ToothGrowth
tg$supp <- as.character(tg$supp)
str(tg)
# 直接输出列
# 原始数据框（因子）
ToothGrowth$supp
# 新数据框（字符串）
tg$supp


# 15.3 向数据框中添加列
# 只需要把值赋到新的列即可。如果把单个值赋到一个新的列，则整个列都会被赋予这个值。
data$newcol <- NA
# 也可以把一个向量赋到新的一列
data$newcol <- vec
# 如果该向量的长度比数据框的行数小，则R会重复这个问题，直到所有的行被填充。
# 数据框的每一列都是一个向量或者因子。R在处理数据框的时候会和处理单独的向量时略有不同，因为在数据框中所有列的长度都是一样的。


# 15.4 从数据框中删除一列
# 把该列的值赋成NULL即可
data$badcol <- NULL
# 也可以使用subset()函数并将一个-（减号）置于待删除的列之前。
# 返回不包含badcol列的数据
data <- subset(data,select = -badcol)
# 排除badcol列和othercol列
data <- subset(data,select=c(-badcol,-othercol))


# 15.5 重命名数据框的列名
# 使用names(dat) <-函数即可
names(dat) <- c("name1","name2","name3")
# 通过列名重命名某一列
library(gcookbook)
names(anthoming) # 输出列名
names(anthoming)[names(anthoming)=="ctrl"] <- c("Control")
names(anthoming)[names(anthoming)=="expt"] <- c("Experimental")
names(anthoming)
# 通过名字的数值位置重命名
names(anthoming)[1] <- "Angle"
names(anthoming)


# 15.6 重排序数据框的列
# 通过列的数值位置重排序
dat <- dat[c(1,3,2)]
# 通过列的名称重排序
dat <- dat[c("col1",,"col3","col2")]
# 使用矩阵形式的索引
library(gcookbook)
anthoming
anthoming[c(1,3,2)] # 列表风格的索引
# 逗号之前的空白表示输出所有行
anthoming[,c(1,3,2)] # 矩阵风格的索引
# 当检索单独一列时，列表风格的索引会得到数据框，矩阵风格的索引会得到向量，除非加上参数drop=FALSE
anthoming[3] # 列表风格的索引
anthoming[,3] # 矩阵风格的索引
anthoming[,3,drop=FALSE] # 矩阵风格的索引，并添加参数drop=FALSE


# 15.7 从数据框提取子集
# 使用subset()函数，它可以筛选出符合一系列条件的行和选出特定的列。
library(gcookbook)
climate
subset(climate,Source=="Berkeley",select=c(Year,Anomaly10y))
# 还可以通过使用|（OR）和&（AND）操作符同时施加多种筛选条件。
subset(climate,Source=="Berkeley"&Year>=1900&Year<=2000,select = c(Year,Anomaly10y))
# 也可以在方括号里面加入索引得到子数据框。
climate[climate$Source=="Berkeley"& climate$Year>=1900&climate$Year<=2000,c("Year","Anomaly10y")]
# 如果用这种方式得到的结果只有一列，那么返回的是一个向量而非一个数据框，除非加上drop=FALSE.
climate[climate$Source=="Berkeley"& climate$Year>=1900&climate$Year<=2000,c("Year","Anomaly10y"),drop=FALSE]
# 还可以通过行和列的数值位置提取子数据框。
climate[1:100,c(2,5)]


# 15.8 改变因子水平的顺序
# 因子水平可以由函数factor()具体设定。
# 默认的因子水平是按字母排的
sizes <-factor(c("small","large","large","small","medium"))
sizes
# 改变因子水平的顺序
sizes <- factor(sizes,levels = c("small","medium","large"))
sizes
# 因子的顺序也可以在第一次创建因子时通过levels参数来指定。
# R中有两种因子：顺序因子（orderer factor）和常规因子（regular factor）。在两种类型中，因子水平都是按某种顺序排列的；
# 区别在于，对于顺序因子，因子水平的顺序是有意义的，而对于常规因子，因子水平的顺序却没什么意义——它仅仅是反映了数据是如何存储的。
# 因子水平的顺序会影响图形输出。
# 当一个因子变量被映射到ggplot2中的图形属性中，图形属性会采用因子水平的顺序。
# 如果因子被映射到x轴上，x轴的标签会按因子水平的顺序排列；如果因子被映射到颜色上，则图例会按因子水平的顺序排序。
# 如果要颠倒因子水平的顺序，可以使用函数rev(levels())
factor(sizes,levels = rev(levels(sizes)))
# 15.9 根据数据的值改变因子水平的顺序
# 使用函数reorder()，该函数有三个参数：因子、排序依据的数据和汇总数据的函数
# 复制一份数据
iss <- InsectSprays
iss$spray
iss$spray <- reorder(iss$spray,iss$count,FUN = mean)
iss$spray


# 15.10 改变因子水平的名称
# 使用plyr包中的revalue()函数或mapvalues()函数
sizes <- factor(c("small","large","large","small","medium"))
sizes
levels(sizes)
# 通过函数revalue()传递一组映射关系
library(plyr)
sizes1 <- revalue(sizes,c(small="S",medium="M",large="L"))
sizes1
# 也可以使用引号 - 如果原因子水平名称中存在空格等特殊字符
revalue(sizes,c("small"="S","medium"="M","large"="L"))
# mapvalues()函数使用两组向量，而不是一组映射关系向量
mapvalues(sizes,c("small","medium","large"),c("A","B","C"))
# 使用levels <- 函数
sizes <- factor(c("small","large","large","small","medium"))
# 通过水平原名称找到某个水平然后重命名
levels(sizes)[levels(sizes)=="large"] <- "L"
levels(sizes)[levels(sizes)=="medium"] <- "M"
levels(sizes)[levels(sizes)=="small"] <- "S"
sizes
# 改变所有水平的名称，可以给levels()传递一个list类型的参数
sizes <- factor(c("small","large","large","small","medium"))
levels(sizes) <- list(S="small",M="medium",L="large")
sizes
# 所有因子水平必须在一个list里面指定，如果这个list里面有任何的缺失，缺失的值最终会以NA代替
# 默认情况下，因子水平是按字母顺序排列的
sizes <- factor(c("small","large","large","small","medium"))
levels(sizes)[1] <- "L"
sizes
# 一次重命名所有的水平
levels(sizes) <- c("L","M","S")
sizes
# 通过因子水平的原始名称去改变因子水平的名称比通过位置改变更安全。
# 15.11 去掉因子中不再使用的水平
sizes <- factor(c("small","large","large","small","medium"))
sizes <- sizes[1:3]
sizes
# 为了删除这些不需要的水平，可以使用droplevels()函数
sizes <- droplevels(sizes)
sizes
# droplevels()函数保留了因子水平的顺序，也可使用except参数保留某个特定的水平。


# 15.12 在字符向量中改变元素的名称
# 用plyr包中的revalue()函数或者mapvalues()函数
sizes <- c("small","large","large","small","medium")
sizes
# 通过函数revalue(),传递一组映射关系
sizes1 <- revalue(sizes,c(small="S",medium = "M",large="L"))
sizes1
# 使用引号
revalue(sizes,c("small"="S","medium"="M","large"="L"))
# mapvalues()函数使用两组向量，而不是一组映射关系向量
mapvalues(sizes,c("small","medium","large"),c("A","B","C"))
# R中，也可通过方括号索引去选择元素然后对它们重命名
sizes <- c("small","large","large","small","medium")
sizes
sizes[sizes=="small"] <- "S"
sizes[sizes=="medium"] <- "M"
sizes[sizes=="large"] <- "L"
sizes


# 15.13 把一个分类变量转化从另一个分类变量
pg <- PlantGrowth[c(1,2,11,21,22),]
pg
# 使用match()函数
pg <- PlantGrowth
oldvals <- c("ctrl","trt1","trt2")
newvals <- factor(c("No","Yes","Yes"))
pg$treatment <- newvals[match(pg$group,oldvals)]
# 也可使用向量索引的方法
pg$treatment[pg$group=="ctr1"] <- "no"
pg$treatment[pg$group=="trt1"] <- "yes"
pg$treatment[pg$group=="trt2"] <- "yes"
# 转化为因子
pg$treatment <- factor(pg$treatment)
pg
# 通过使用&和|操作符，编码准则同样可以基于多个列的取值：
pg$newcol[pg$group=="ctrl"&pg$weight<5] <- "no_small"
pg$newcol[pg$group=="ctrl"&pg$weight>=5] <- "no_large"
pg$newcol[pg$group=="trt1"] <- "yes"
pg$newcol[pg$group=="trt2"] <- "yes"
pg$newcol <- factor(pg$newcol)
pg


# 使用interaction()函数把数据框中的两列组合成一列,该函数会在两个值得中间加上“."符号。
pg$weighttrt <- interaction(pg$weightcat,pg$treatment)
pg


# 15.14 连续变量转换为分类变量
pg <- PlantGrowth[c(1,2,11,21,22),]
pg
# 使用cut()函数把一个连续变量weight转化为分类变量wtclass:
pg$wtclass <- cut(pg$weight,breaks = c(0,5,6,Inf))
pg
# cut()函数的输出结果是一个因子，因子水平的名称是以生成的区间命名的。
# 为了改变因子水平的名称，可以使用cut()中的labels参数
pg$wtclass <- cut(pg$weight,breaks = c(0,5,6,Inf),labels = c("small","medium","large"))
pg
# cut()生成的区间是左开右闭的。对于值最小的一类，可以通过设定参数include.lowest=TRUE使得它同时包含最小值和最大值。
# 左闭右开区间可以设定参数righe=FALSE;
cut(pg$weight,breaks = c(0,5,6,Inf),right = FALSE)


# 15.15 变量转换
# 可以使用$操作符来引用新列并对其赋予新值。
library(gcookbook)
# 复制数据集
hw <- heightweight
hw
hw$heightCm <- hw$heightIn* 2.54
hw
# 使用transform()或plyr包的mutate()函数。
hw <- transform(hw,heightCm = heightIn*2.54,weightKg=weightLb/2.204)
library(plyr)
hw <- mutate(hw,heightCm=heightIn*2.54,weightKg=weightLb/2.204)
hw
# 也可以根据多个变量计算产生一个新的变量
hw <- transform(hw,bmi = weightKg/(heightCm/100)^2)
hw <- mutate(hw,bmi=weightKg/(heightCm/100)^2)
hw$bmi <- hw$weightKg/(hw$heightCm/100)^2
hw
# transform()和mutate()函数的最大区别是transform()会同时计算所有的新列，而mutate()将依次计算新列，这样在计算新列时就可以依赖之前的新列。
# 使用mutate()函数一次完成计算
hw <- heightweight
hw <- mutate(hw,heightCm=heightIn*2.54,weightKg=weightLb/2.204,bmi=weightKg/(heightCm/100)^2)
hw


# 15.16按组转换数据
# 使用plyr包中的ddply()函数，在参数中调用transform()，并指定运算
library(MASS)
library(plyr)
cb <- ddply(cabbages,"Cult",transform,DevWt=HeadWt-mean(HeadWt))
cb
transform(cabbages,DevWt = HeadWt-mean(HeadWt))
# 注意，ddply()函数和之前的transform()函数具有相同的参数列表，唯一的区别是ddply()略微调整了参数的位置，并且添加了分割变量
# 标准化前
library(ggplot2)
ggplot(cb,aes(x=Cult,y=HeadWt))+geom_boxplot()
# 标准化后
ggplot(cb,aes(x=Cult,y=DevWt))+geom_boxplot()
# 根据多个变量来分组、切割数据框，同时也可在多个变量上进行计算。
ddply(cabbages,c("Cult","Date"),transform,DevWt =HeadWt-mean(HeadWt),DevVitC = VitC-mean(VitC))


# 15.17分组汇总数据
# 配合summarise()函数使用plyr包中的ddply()函数，并指定要进行的操作即可
library(MASS)
library(plyr)
ddply(cabbages,c("Cult","Date"),transform,DevWt =mean(HeadWt),DevVitC = mean(VitC))
# 使用summarise()函数
library(plyr)
summarise(cabbages,Weight=mean(HeadWt))
ddply(cabbages,"Cult",summarise,Weight=mean(HeadWt))
# 根据多个变量（即多个列）切割数据框然后汇总;用一个包含多个变量名的向量即可
ddply(cabbages,c("Cult","Date"),summarise,Weight=mean(HeadWt),VitC=mean(VitC))
# 使用sd()计算标准差，length()计算频数
ddply(cabbages,c("Cult","Date"),summarise,weight=mean(HeadWt),sd=sd(HeadWt),n=length(HeadWt))
# 其他的函数，如min()、max()、median()等
# 处理缺失值
c1 <- cabbages
c1$HeadWt[c(1,20,45)] <-NA # 数据某些值赋值为NA
ddply(c1,c("Cult","Date"),summarise,weight=mean(HeadWt),sd=sd(HeadWt),n=length(HeadWt))
# 设置na.rm=TRUE,即可忽略缺失值
# length()函数并没有na.rm的选项，但可以用sum(!is.na(...))达到相同的效果
# is.na()返回一个逻辑向量，NA返回TRUE,非NA返回FALSE，用！取反后，再用sum()函数将TRUE的数量加起来，最终的结果就是非缺失值的频数
ddply(c1,c("Cult","Date"),summarise,Weight=mean(HeadWt,na.rm = TRUE),sd=sd(HeadWt,na.rm = TRUE),n=sum(!is.na(HeadWt)))
# 组合缺失
# 如果在分组变量中有任何“空组合”，它们就不会出现在汇总的数据框中。缺失组合会给绘图带来麻烦
# 赋值cabbages并移除同时包含c52和d21的行
c2 <- subset(c1,!(Cult=="c52"&Date=="d21"))
c2
c2a <- ddply(c2,c("Cult","Date"),summarise,Weight=mean(HeadWt,na.rm = TRUE),sd=sd(HeadWt,na.rm = TRUE),n=sum(!is.na(HeadWt)))
c2a
# 绘图
ggplot(c2a,aes(x=Date,fill=Cult,y=Weight))+geom_bar(position = "dodge")
# Error: stat_count() must not be used with a y aesthetic.
# 为了填充缺失的组合，在ddply()函数中使用.drop=FALSE即可
library(plyr)
c2b <- ddply(c2,c("Cult","Date"),.drop = FALSE,summarise,Weight=mean(HeadWt,na.rm = TRUE),sd=sd(HeadWt,na.rm = TRUE),n=sum(!is.na(HeadWt)))
c2b
# 绘图
library(ggplot2)
ggplot(c2a,aes(x=Date,fill=Cult,y=Weight))+geom_bar(position = "dodge")
# 使用stat_summary()计算均值


# 15.18使用标准误差和置信区间来汇总数据
# 计算均值的标准误差包括两步：首先计算各组的标准差和频数，然后用这些值来计算得到标准误差。各组的标准误差就是标准差除以样本量的平方根
library(MASS)
library(plyr)
ca <- ddply(cabbages,c("Cult","Date"),summarise,Weight=mean(HeadWt,na.rm = TRUE),sd=sd(HeadWt,na.rm = TRUE),n=sum(!is.na(HeadWt)),se=sd/sqrt(n))
ca
# 另一种方法是在ddply函数内部计算标准误差。
ddply(cabbages,c("Cult","Date"),summarise,Weight=mean(HeadWt,na.rm = TRUE),sd=sd(HeadWt,na.rm = TRUE),n=sum(!is.na(HeadWt)),se=sd/sqrt(n))
# 置信区间是通过均值的标准误差和自由度计算得到的。
# 要计算置信区间，首先使用qt()函数得到分位数，然后和标准误差相乘即可。给定概率值和自由度，qt()函数会计算出对应t分布的分位数。
# 对于95%的置信区间来说，应该使用0.975的概率值；对于钟形的t分布，这对应了曲线两端2.5%的面积。自由度是样本量大小减去1.
ciMult <- qt(.975,ca$n-1)
ciMult
ca$ci <- ca$se*ciMult
ca$ci
ca$ci95 <- ca$se*qt(.975,ca$n)
# 对于99%的置信区间，使用0.995的概率值。
# 误差条表示均值的标准误差，它和置信区间有相同的功能：给看图的人展示估计总体均值的好坏程度。
# 标准误差是抽样分布的标准差。
# 一并计算标准差、频数、标准误差和置信区间的函数，并且还可以处理缺失值和空缺组合。
# 默认使用95%的置信区间，也可通过conf.interval参数来改变
summarySE <- function(data=NULL,measurevar,groupvars = NULL,conf.interval=.95,na.rm=FALSE,.drop=TRUE){
  require(plyr)
  # 新版本的length可以处理缺失值：如果na.rm==T，则排除缺失值
  length2 <- function(x,na.rm=FALSE){
    if(na.rm) sum(!is.na(x))
    else length(x)
  }
  # 汇总
  datac <- ddply(data,groupvars,
                 .drop=.drop,
                 .fun = function(xx,col,na.rm){ 
                   c( n=length2(xx[,col],na.rm=na.rm),
                      mean=mean(xx[,col],na.rm=na.rm),
                      sd = sd(xx[,col],na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm)
  # 重命名“mean”列
  datac <- rename(datac,c("mean"=measurevar))
  datac$se <- datac$sd/sqrt(datac$n) # 计算均值的标准误差
  # 标准误差的置信区间乘数
  # 为置信区间计算t统计量：
  ciMult <- qt(conf.interval/2+.5,datac$n-1)
  datac$ci <- datac$se*ciMult
  return(datac)
}
# 移除c52和d21对应的所有行
c2 <- subset(cabbages,!(Cult=="c52"&Date=="d21"))
# 将一些值设置为NA
c2$HeadWt[c(1,20,45)] <- NA
summarySE(c2,"HeadWt",c("Cult","Date"),conf.interval = .99,na.rm = TRUE,.drop = FALSE)


# 15.19把数据框从“宽”边“长”
# 使用reshape2包中的melt()函数。
library(gcookbook)
anthoming
library(reshape2)
melt(anthoming,id.vars = "Angle",variable.name = "condition",value.name = "count")
# 在原始数据中，有标识变量（ID variable)和度量变量（measure variable)。标识变量表明哪些值要汇集在一起，即哪些值是描述同一个对象。
drunk
melt(drunk,id.vars = "sex",measure.vars = c("0-29","30-39"),variable.name = "age",value.name = "count")
# 用多列作为标识变量
plum_wide
melt(plum_wide,id.vars = c("length","time"),variable.name = "survival",value.name = "count")


# 创造数据的一个副本
co <- corneas
co
# 添加标识列
co$id <- 1:nrow(co)
melt(co,id.vars = "id",variable.name = "eye",value.name = "thickness")
# 用数值作为标识变量可能会给后续分析带来问题，可能要用as.character()函数把它转化为字符型的向量或者用factor()将其转化为因子
# stack()函数也可把数据框从“宽”变“长”.


# 15.20 把数据框从“长”变“宽”
# 使用reshape2包中的dcast()函数。
library(gcookbook)
plum
# 从“长”到“宽”的转化把一列中不重要的值提出来并用它们作为新列的名称，然后用另一列作为新列的数据源。
library(reshape2)
dcast(plum,length+time~survival,value.var = "count")
# dcast()函数要求指明标识变量（留下来的列）和可变边变量（variable variable)（会转化成新生成列的变量）.
# 用公式完成，波浪线~坐标表示标识变量，右边表示可变变量
# 当有多个可变变量时，生成的列名由下划线连接起来
dcast(plum,time~length+survival,value.var = "count")
# unstack()函数也可把数据框从“长”变“宽”。
# 15.21 把时间序列数据对象拆分成时间和数据
# time()函数可以得到每个观测的时间值，然后用as.numeric()函数将时间和该数据转化为数值形式。
# 查看时间序列对象nhtemp
nhtemp
# 得到每次观测的时间
as.numeric(time(nhtemp))
# 得到每次观测的值
as.numeric(nhtemp)
# 把它们放进一个数据框中
nht <- data.frame(year=as.numeric(time(nhtemp)),temp = as.numeric(nhtemp))
nht


presidents
# 转化为两列值得数据框
pres_rating <- data.frame(
  year=as.numeric(time(presidents)),
  rating = as.numeric(presidents)
)
pres_rating
pres_rating2 <- data.frame(
  year = as.numeric(time(presidents)),
  quarter = as.numeric(cycle(presidents)),
  rating = as.numeric(presidents)
)
pres_rating2
# zoo包在处理时间序列对象的时候也非常有用。