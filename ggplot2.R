library(ggplot2)

getOption('max.print')
ggplot(data = diamonds,
       mapping = aes(x = log(carat) ,y = log(price)) )+
  geom_point()

ggplot(data = diamonds,mapping = aes(x = carat ,y = x*y*z ))+
  geom_point()

#简化
set.seed(1410) 
dsmall <- diamonds[sample(nrow(diamonds),100),]#抽样
ggplot(data = dsmall ,
       mapping = aes(x = carat ,y = price,colour = color))+
  geom_point()
ggplot(data = dsmall ,
       mapping = aes(x = carat ,y = price,colour = color ,
                     shape = cut , alpha = I(1/10))) + 
  geom_point()  #颜色形状透明度
  

# geom 几何对象point散点图 smooth平滑曲线
# boxplot箱线图 path/line连线 histogram直方图 freqpoly频率多边形
# density密度曲线 bar柱形图
ggplot(data = dsmall ,
       mapping = aes(x = carat ,y = price,colour = color))+
  geom_point() +geom_smooth(span = 1)#span拟合方式（曲线弯曲度）

ggplot(data = dsmall ,
       mapping = aes(x = carat ,y = price,colour = color))+
  geom_point() +geom_smooth(methon = 'gam',formula =y ~s(x))


ggplot(data = diamonds,mapping = aes(color,price/carat)) +
  geom_boxplot()

ggplot(data = diamonds,mapping = aes(carat,fill = color)) +
  geom_histogram(bins = 30)

ggplot(data = economics,mapping = aes(date,unemploy/pop)) +
  geom_line()

ggplot(data = economics,
       mapping = aes(unemploy/pop,uempmed,colour = date)) +
  geom_path() + scale_size_area()

ggplot(data = dsmall,mapping = aes(carat,price )) + geom_point(xlab = "price",ylab ="weight") 

#ggplot2基础

library(ggplot2)
ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()  # 柱形图默认stack堆积

ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar(position = "fill")  # 百分比堆积

ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar(position = position_stack(reverse = TRUE))  # 翻转各组内部垂直堆叠顺序

# 散点图 + 折线图
series <- data.frame(time = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), type = rep(c("a", 
                                                                                        "b", "c", "d"), 4), value = rpois(16, 10))
ggplot(series, aes(time, value, group = type)) + geom_line(aes(colour = type)) + 
  geom_point(aes(colour = type))  # 默认identity

ggplot(series, aes(time, value, group = type)) + geom_line(aes(colour = type), 
                                                           position = "stack") + geom_point(aes(colour = type), position = "stack")  # stack堆积

ggplot(series, aes(time, value, group = type)) + geom_line(aes(colour = type), 
                                                           position = position_stack(vjust = 0.5)) + geom_point(aes(colour = type), 
                                                                                                                position = position_stack(vjust = 0.5))  # 向下移动半个单位，以最下面的元素为高度为基准

ggplot(series, aes(time, value, group = type)) + geom_line(aes(colour = type), 
                                                           position = position_stack(vjust = 0)) + geom_point(aes(colour = type), position = position_stack(vjust = 0))  # 向下移动到底，最下面的折线都拉直了
# 1、箱线图
library(ggplot2)
dsub <- diamonds[sample(nrow(diamonds), 1000), ]

ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) + geom_boxplot(outlier.size = 0) + 
  geom_point(shape = 23)  # 23号点形状为菱形

ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) + geom_boxplot(outlier.size = 0) + 
  geom_point(shape = 23, position = position_jitterdodge(dodge.width = 0.1))  # 点分布于各组箱子10%宽度上，默认点水平抖动错开

ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) + geom_boxplot(outlier.size = 0) + 
  geom_point(shape = 23, position = position_jitterdodge(dodge.width = 0.8))  # 点分布于各组箱子80%宽度上，默认点水平抖动错开

#stat_xxx()统计变换
library(ggplot2)
library(Hmisc)
#stat_summary
g <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
g + stat_summary(fun.data = "mean_cl_boot", color = "red", size = 2)  # 用mean_cl_bool对mpg进行运算，返回均值，最大值，最小值3个向量组成的矩阵

g + stat_summary(fun.y = "median", color = "red", size = 2, geom = "point")  # 计算各组中位数
g + stat_summary(fun.y = "mean", color = "red", size = 2, geom = "point")  # 计算各组均值
g + aes(color = factor(vs)) + stat_summary(fun.y = mean, geom = "line")  # 增加1组颜色变量映射，然后求均值并连线  
g + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, color = "red")  # 计算各组均值，最值

# stat_summary_bin
g1 <- ggplot(diamonds, aes(cut))
g1 + geom_bar()  # 条形图 ，只有1个映射的时候默认为计数
g1 + stat_summary_bin(aes(y = price), fun.y = "mean", geom = "bar")  # 分组计算均值

# stat_sum_df用矩形将最值与均值框起来
stat_sum_df <- function(fun, geom = "crossbar", ...) {
  stat_summary(fun.data = fun, color = "red", geom = geom, width = 0.2, ...)
}
g2 <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
g2 + stat_sum_df("mean_cl_boot", mapping = aes(group = cyl))  # 增加1个分组映射
g2 + stat_sum_df("mean_sdl", mapping = aes(group = cyl))
g2 + stat_sum_df("mean_sdl", fun.args = list(mult = 1), mapping = aes(group = cyl))
g2 + stat_sum_df("median_hilow", mapping = aes(group = cyl))
#stat_function
library(ggplot2)
set.seed(1492)
df <- data.frame(
  x = rnorm(100)
)
x <- df$x
base <- ggplot(df, aes(x)) + geom_density() # 核密度图，展示变量分布规律，与频率分布直方图原理相同
base + stat_function(fun = dnorm, color = "red") # dnorm表示正态分布密度函数
base + stat_function(fun = dnorm, colour = "red", args = list(mean = 3)) # args传参给fun，生成均值为3的正态分布密度图

ggplot(data.frame(x = c(0, 2)), aes(x)) + 
  stat_function(fun = exp, geom = "line") # 画e^x在(0, 2)区间的函数图形，数据点由插值产生
ggplot(data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm) # 画在区间(-5, 5)区间的正态分布密度图，数据点由插值产生
ggplot(data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 2, sd = .5)) # 画均值为2，标准差为0.5的正态分布密度图

f <- ggplot(data.frame(x = c(0, 10)), aes(x))
f + stat_function(fun = sin, color = "red") + # 绘制(0, 10)区间的正弦函数图形
  stat_function(fun = cos, color = "blue") # 绘制(0, 10)区间的余弦函数图形

myfunction <- function(x) {x^2 + x + 20}
f + stat_function(fun = myfunction) # 画自定义函数图像

fun1 <- function(x) {0.5 * x}
fun2 <- function(x) {x / (x +1)}
fun3 <- function(x) {0.5 * x - x*(x + 1)}
ggplot(data.frame(x = -5:5), aes(x)) + stat_function(fun = fun1, color = "red") +
  stat_function(fun = fun2, color = "blue") + 
  stat_function(fun = fun3, color = "yellow", size = 4)
#stat_smooth
library(ggplot2)

ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth() + stat_smooth(method = lm, 
                                                                          se = FALSE)  # 不显示置信区间
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = lm, formula = y ~ 
                                                            splines::bs(x, 3), se = FALSE)
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() + geom_smooth(se = FALSE, 
                                                                         method = lm)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 0.8) + geom_smooth(method = loess, 
                                                                                    formula = y ~ x) + facet_wrap(~drv)
#coord_flip
h <- ggplot(diamonds, aes(carat)) + geom_histogram()
h
h + coord_flip()  # 翻转坐标系
#风玫瑰图
rm(list = ls())
gc()  # 清空内存
library(ggplot2)
set.seed(42)
small <- diamonds[sample(nrow(diamonds), 1000), ]

ggplot(data = small) + geom_bar(aes(x = clarity, fill = cut)) + coord_polar() + 
  scale_fill_brewer(type = "qual", palette = "Set2", direction = -1)


p <- ggplot(mtcars,aes(x=mpg,y=wt))
p <- p+ geom_poin
t(aes(color=factor(cyl)))
p <- p + geom_point(aes(y=disp))

boysbox <- ggplot(Oxboys,aes(Occasion,heignt))

ggplot(diamonds,aes(carat))+geom_histogram(aes(y=..density..),binwidth = 0.1)


d <- ggplot(diamonds,aes(carat)) + xlim(0,3)
d + stat_bin(aes(ymax=..count..),binwidth = 0.1,geom = 'point',position = 'identity')

ggplot(faithful,aes(x=eruptions,y=waiting))+
  geom_point()+stat_smooth()
quakes
ggplot(quakes,aes(x=depth))+ geom_bar(binwidth = 50)
ggplot(quakes,aes(x=long,y=lat)) +geom_point()
ggplot(quakes,aes(x= depth))+stat_bin(binwidth = 50)
ggplot(longley,aes(x=Year,y=Employed)) + geom_point()+ stat_smooth(method='lm')
ggplot(mtcars,aes(x=hp,y=mpg))+geom_point(aes(shape=factor(cyl),colour=factor(cyl))) + 
  scale_shape_discrete(name='cylinders')+ scale_color_discrete(name='cylinders')
attach

library(plyr)
qplot(carat,depth,data=diamonds,geom="boxplot",group=round_any(carat,0.1,floor),xlim = c(0,3))
qplot(depth,data= diamonds,geom='density',xlim=c(54,70),fill=cut,alpha=I(0.2))


p <- qplot(cty,hwy,data=mpg,colour=displ) +
  scale_x_continuous("city mpg")+xlab('city mpg')+ylab('highway mpg')+ labs(x='city mpg',y='highway',color='Displacement')+
  xlab(expression(frac(miles,gallon)))
p
# ????
mpg2 <- within(mpg,{model<- reorder(model,cty)
manufacturer <-reorder(manufacturer,-cty)})
models <- qplot(cty,model,data = mpg2)
models + facet_grid(manufacturer ~ .,scales = 'free',space = 'free')+
  theme(strip.text.y = element_text(angle=0))
# ??????????
qplot(color,data=diamonds,geom = 'bar',fill=cut,position = 'dodge')
qplot(cut,data=diamonds,geom = 'bar',fill=cut)+facet_grid(.~color)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 8,colour = 'grey50')) + theme_bw()

mpg3 <- subset(mpg,manufacturer %in% c('audi','volkswagen','jeep'))
mpg3$manufacturer <- as.character(mpg3$manufacturer)
mpg3$model <- as.character(mpg3$model)
base <- ggplot(mpg3,aes(fill=model)) + geom_bar(position = 'dodge') +
  theme(legend.position = 'none')
base + aes(x=model) + facet_grid(.~manufacturer)
last_plot() + facet_grid(.~manufacturer,scales = 'free_x',space = 'free')
base + aes(x=manufacturer) 

# theme
theme_gray()
theme_bw()
theme_set(theme_gray())

# yi ye duo tu
a <- qplot(date,unemploy,data = economics,geom = 'line')
b <- qplot(uempmed,unemploy,data = economics)+geom_smooth(se=F)
c <- qplot(uempmed,unemploy,data=economics,geom = 'path')
# zi tu
library(grid)
vp1 <- viewport(width = 1,height = 1,x=0.5,y=0.5)
vp1 <- viewport()
vp2 <- viewport(width = 0.5,height = 0.5)
vp3 <- viewport(width = unit(2,'cm'),height = unit(3,'cm'))
subvp <- viewport(width = 0.4,height = 0.4,x=0.75,y=0.35)
b
print(c,vp=subvp)

csmall <- c + theme_gray(9) + labs(x=NULL,y=NULL)+theme(plot.margin = unit(rep(0,4),'lines'))
print(csmall,vp=subvp)
pdf()

# ju xing wang ge
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
vplayout <- function(x,y){
  viewport(layout.pos.row = x,layout.pos.col = y)
}
print(a,vp=vplayout(1,1:2))
print(b,vp= vplayout(2,1))
print(c,vp=vplayout(2,2))

# plyr
library(plyr)
#  every color zuixiao de
ddply(diamonds,.(color),subset,carat==min(carat))
# 每组 qian 1% da xiao 
ddply(diamonds,.(color),subset,carat > quantile(carat,0.99))
# 每个颜色组里钻石价格标准化，均值为0，方差为1
ddply(diamonds,.(color),transform,price=scale(price))
# 拟合多个模型
dense <- subset(diamonds,carat <2)
qplot(carat,price,data=dense,geom = 'smooth',colour= color,fullrange=T)
