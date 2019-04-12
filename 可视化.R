library(gcookbook)
library(ggplot2)
library(RColorBrewer) # 调色盘
library(dplyr)

# 散点图
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex))+
  geom_point(size=3) + scale_shape_manual(values = c(1,2)) # 调整点
ggplot(heightweight,aes(x=ageYear,y=heightIn,shape=sex,colour=sex,size=weightLb))+
  geom_point(alpha=.5) +scale_size_area() # 将连续型变量映射到大小或颜色,数据点面积正比于变量值
# 处理图形重叠：半透明/分箱（bin)
ggplot(diamonds,aes(x=carat,y=price))+
  geom_point()+stat_bin2d(bins = 50)+ # 设置分箱数
  scale_fill_gradient(low = 'lightblue',high = 'red',
                      breaks=c(0,1000,2000,4000,6000),limits=c(0,6000))
ggplot(diamonds,aes(x=carat,y=price))+
  geom_point()+
  stat_smooth(method = lm,se=F,color='black') # 添加回归拟合线
# 添加标签
sp <- ggplot(subset(countries,Year==2009&healthexp>2000),
             aes(x=healthexp,y=infmortality)) +geom_point()
sp + annotate('text',x=4350,y=5.4,label='canada')+
  annotate('text',x=7400,y=6.8,label='usa')
sp + geom_text(aes(label=Name),size=3,vjust=1,hjust= -1)
# 指定标签
cdat <- subset(countries,Year==2009 & healthexp > 2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c('Canada','Ireland','Iceland','Japan')
cdat$Name1[!idx] <- NA
ggplot(cdat,aes(x=healthexp,y=infmortality))+
  geom_point()+ geom_text(aes(x=healthexp+100,label=Name1),size=4,hjust=0)+
  xlim(2000,10000) # 增大x轴范围
# 绘制气泡图
p <- ggplot(cdat,aes(x=healthexp,y=infmortality,size=GDP))+
  geom_point(shape=21,color='black',fill='cornsilk') +
  scale_size_area(max_size = 15)
p

# 折线图
plot(pressure$temperature,pressure$pressure,type = 'l')
points(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure/2,col='red')
qplot(temperature,pressure,data=pressure,geom = 'line')
ggplot(pressure,aes(x=temperature,y=pressure)) + geom_line()
qplot(temperature,pressure,data=pressure,geom = c('line','point'))
# 密度曲线图
ggplot(faithful,aes(x=waiting,y=..density..))+ # .. ..减小直方图标度
  geom_histogram(fill='cornsilk',size=.2)+ geom_density()+xlim(35,105)


# 条形图
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
ggplot(BOD,aes(x=factor(Time),y=demand)) + 
  geom_bar(stat = 'identity')
# 簇状条形图：分类映射到fill，position=’dodge'
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(position = 'dodge',stat = 'identity',colour='black')+
  scale_fill_brewer(palette = 'Paired') # 对fill进行调色
upc <- subset(uspopchange,rank(Change) >40)
ggplot(upc,aes(x=reorder(Abb,Change),y=Change,fill=Region))+
  geom_bar(stat='identity')+
  scale_fill_manual(values = c('darkred','darkgreen'))+
  xlab('state')
# 正负条形图分别进行着色
csub <- subset(climate,Source == 'Berkeley' & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
csub
ggplot(csub,aes(x=Year,y=Anomaly10y ,fill=pos))+
  geom_bar(stat = 'identity',position = 'identity',color='black',size=0.25)+
  scale_fill_manual(values=c('#CCEEFF','#FFDDDD'),guide=F) # guide=F，移除条例
# 调整条形宽度和间距
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity',width=0.5,position=position_dodge(0.7)) # position = 'dodge'默认0.9
# 绘制堆积条形图
library(plyr)
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar,order=desc(Cultivar)))+
  geom_bar(stat = 'identity') # plyr包desc函数调整堆叠顺序
#  guides(fill=guide_legend(reverse=T))调整图例顺序，默认与堆积顺序相反
# 绘制百分比堆积条形图：plyr包中ddply()和transform()函数将数据标准化位100%
ce <- ddply(cabbage_exp,'Date',transform,percent_weight = Weight / sum(Weight) * 100)
ggplot(ce,aes(x=Date,y=percent_weight,fill=Cultivar))+
  geom_bar(stat = 'identity')
# 参数向量包含在同一个数据框
qplot(Time,demand,data = BOD,geom = 'bar',stat = 'identity')
ggplot(BOD,aes(Time,demand))+ geom_bar(stat = 'identity',fill='skyblue',colour='red')
# 添加标签
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity') + geom_text(aes(label=Weight),vjust=1.5) # 条形图顶端下方
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity') + geom_text(aes(label=Weight),vjust=-0.2) # 条形图顶端上方
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = 'identity')+ geom_text(aes(label=Weight),vjust=1.5,color='white')

# 箱线图
boxplot(len~supp,data = ToothGrowth)
# interaction()将分组变量组合在一起成多分组变量箱线图
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len,color=factor(dose))) + 
  geom_boxplot()
# 箱线图、直方图密度图对比
library(MASS)
ggplot(birthwt,aes(x=factor(race),y=bwt)) + 
  geom_boxplot(outlier.size = 1.5,outlier.shape = 21) # 修改异常点大小形状
ggplot(birthwt,aes(x=factor(race),y=bwt)) + 
  geom_boxplot(outlier.size = 1.5,outlier.shape = 21,notch = T) # notch槽口
# wilkinson点图
ggplot(subset(countries,Year==2009&healthexp>2000),aes(x=infmortality)) + geom_dotplot()
ggplot(heightweight,aes(x=sex,y=heightIn))+
  geom_dotplot(binaxis = 'y',binwidth = .5,stackdir = 'center') # 沿y轴堆叠 
# 并排放置
ggplot(heightweight,aes(sex,heightIn))+
  geom_boxplot(aes(x=as.numeric(sex)+.2,group=sex),width=.25)+
  geom_dotplot(aes(x=as.numeric(sex)-.2,group=sex),binaxis = 'y',binwidth = .5,stackdir = 'center')+
  scale_x_continuous(breaks = 1:nlevels(heightweight$sex),labels = levels(heightweight$sex))

# cleveland点图
tophit <- tophitters2001[1:25,]
ggplot(tophit,aes(x=avg,y=reorder(name,avg))) +
  geom_point(size=3) + 
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',linetype = 'dashed'))
ggplot(tophit,aes(x=avg,y=name))+
  geom_segment(aes(yend=name),xend=0,colour='grey')+
  geom_point(size=3,aes(color=lg))+
  scale_colour_brewer(palette = 'Set1',limits=c('NL','AL'),guide=F)+
  theme_bw()+ theme(panel.grid.major.y = element_blank(),legend.position = 'left')+ 
  facet_grid(lg~.,scales = 'free_y',space = 'free_y')
# 折线图
ggplot(BOD,aes(x=Time,y=demand))+ geom_line()+
  geom_point(size=4,shape=21,fill='white')

# 面积图
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+
  geom_area(colour='black',size=0.2,alpha=0.4)+
  scale_fill_brewer(palette = 'Blues',
                    breaks=rev(levels(uspopage$AgeGroup)))# breaks切分反转堆积顺序
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(AgeGroup)))+
  geom_area(colour=NA,size=0.2,alpha=0.4)+geom_line(position = 'stack')+
  scale_fill_brewer(palette = 'Blues') 
# 百分比堆积面积图
library(plyr)
usp_prob <- ddply(uspopage,'Year',transform,
                  Percent =Thousands/sum(Thousands)*100) # 按year分组,transform计算组内百分比，ddply将数据框重组
ggplot(usp_prob,aes(x=Year,y=Percent,fill=AgeGroup))+
  geom_area(colour='black',size=.2,alpha=.4)+
  scale_fill_brewer(palette = 'Blues',breaks=rev(levels(uspopage$AgeGroup)))

# 绘制环状图
ggplot(wind,aes(x=DirCat,fill=SpeedCat))+
  geom_histogram(binwidth = 15,origin=-7.5)+
  coord_polar()+scale_x_continuous(limits = c(0,360))
# 添加置信域
ggplot(climate,aes(x=Year,y=Anomaly10y))+
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y),alpha=.2)+
  geom_line() # unc10y表示95%置信水平下置信区间，ribbon指阴影

# 添加注释
p <- ggplot(faithful,aes(x=eruptions,y=waiting)) + geom_point()
p + annotate('text',x=3,y=48,label='group 1')
# 添加数字表达式
p + annotate('text',x=2,y=80,parse=T,label='frac(1,sqrt(2*pi)) *e')
# 添加直线
p + geom_hline(yintercept = 60)+geom_vline(xintercept = 4)
p + annotate('segment',x=5,xend = 4,y=60,yend=1)
p + annotate('segment',x=5,xend = 4,y=60,yend=1,
             arrow=arrow(ends = 'both',angle = 90,
                         length = unit(.2,'cm')))
# 添加误差线
ce <- subset(cabbage_exp,Cultivar == 'c39')
ggplot(cabbage_exp,aes(Date,Weight,fill=Cultivar))+
  geom_bar(position = 'dodge',stat = 'identity')+
  geom_errorbar(aes(ymin=Weight-se,ymax=Weight+se),
                position = position_dodge(.9),width=.2)+
  coord_flip() + 
  theme(axis.text.x = element_text(angle=45,hjust=1,family = 'Times'))

# 修改外观
p <- ggplot(heightweight,aes(x=ageYear,y=heightIn,color=sex))+geom_point()
# 主题项目
# 调用win字体
windowsFonts(bold.italic=windowsFont("Bold Italic"))
p + theme(axis.title.x = element_text(size=16,lineheight = .9,
                                      colour='red',family = 'bold.italic'))
p + theme(plot.title = element_text(size=rel(1.5),lineheight = .9,colour = 'red')) +
  ggtitle('plot')
p + annotate('text',x=15,y=53,label='some text',size=7,colour='red')
p + geom_text(aes(label=weightLb),size=4,colour='red')
# 修改主题外观
p + theme(panel.grid.major = element_line(colour = 'red',),
          panel.grid.minor = element_line(colour='blue',linetype = 'dashed'),
          panel.background = element_rect(fill = 'skyblue'),
          axis.title.x = element_text(colour='green'),
          axis.text.x = element_text(colour='orange'),
          legend.title = element_text(colour='purple'),
          legend.text = element_text(colour='darkgreen')) +
  facet_grid(sex~.) +
  theme(strip.background = element_rect(fill = 'pink'),
        strip.text.y = element_text(size=14,angle=-90,face = 'bold'))
# 修改图例位置
p <- ggplot(PlantGrowth,aes(group,weight,fill=group))+
  geom_boxplot()+ scale_fill_brewer(palette = 'Pastel2')
p + theme(legend.position = 'top')
p + theme(legend.position = c(1,0),legend.justification = c(1,0),
          legend.background = element_blank())
# 修改图里标签
p + theme(legend.position = c(1,0),legend.justification = c(1,0),
          legend.background = element_blank())+
  scale_fill_discrete(limits=c('trt1','trt2','ctrl'),
                      labels=c('treatment1','treatment2','control'))

# 分面
p <- ggplot(mpg,aes(displ,hwy)) + geom_point()
# 纵向排列
p + facet_grid(drv~.)
# 横向排列
p + facet_grid(.~cyl)
p + facet_grid(drv ~ cyl,scales = 'free')
# 分面标签
ggplot(cabbage_exp,aes(Cultivar,Weight))+geom_bar(stat = 'identity')+
  facet_grid(. ~ Date)+
  theme(strip.text = element_text(face = 'bold',size=rel(1.5)),
        strip.background =  element_rect(fill = 'skyblue',colour = 'black',size=1))
# 调色
ggplot(cabbage_exp,aes(Cultivar,Weight,fill=Cultivar))+geom_bar(stat = 'identity')+
  facet_grid(. ~ Date)+
  theme(strip.text = element_text(face = 'bold',size=rel(1.5)),
        strip.background =  element_rect(fill = 'skyblue',colour = 'black',size=1))+
  scale_fill_brewer(palette = 'Set3')
# 渐变色
p <- ggplot(heightweight,aes(ageYear,heightIn,colour=weightLb))+ geom_point()
p + scale_color_gradient(low='red',high = 'blue')
p + scale_color_gradient2(low='red',mid = 'white',high = 'blue',midpoint = 110)
p + scale_color_gradientn(colours=c('red','yellow','blue','green'))

# 相关性图
library(corrplot)
mcor <- cor(mtcars)
corrplot(mcor)
# corr：需要可视化的相关系数矩阵 method：指定可视化的方法，可以是圆形、方形、椭圆形、数值、阴影、颜色或饼图形
# type：指定展示的方式，可以是完全的、下三角或上三角
# col：指定图形展示的颜色，默认以均匀的颜色展示
# bg：指定图的背景色 title：为图形添加标题
# is.corr：是否为相关系数绘图，默认为TRUE，同样也可以实现非相关系数的可视化，只需使该参数设为FALSE即可
# diag：是否展示对角线上的结果，默认为TRUE
# outline：是否绘制圆形、方形或椭圆形的轮廓，默认为FALSE
# mar：具体设置图形的四边间距
# addgrid.col：当选择的方法为颜色或阴影时，默认的网格线颜色为白色，否则为灰色
# addCoef.col：为相关系数添加颜色，默认不添加相关系数，只有方法为number时，该参数才起作用
# addCoefasPercent：为节省绘图空间，是否将相关系数转换为百分比格式，默认为FALSE
# order：指定相关系数排序的方法，可以是原始顺序(original)、特征向量角序(AOE)、第一主成分顺序(FPC)、层次聚类顺序(hclust)和字母顺序，一般”AOE”排序结果都比”FPC”要好
# hclust.method：当order为hclust时，该参数可以是层次聚类中ward法、最大距离法等7种之一
# addrect：当order为hclust时，可以为添加相关系数图添加矩形框，默认不添加框，如果想添加框时，只需为该参数指定一个整数即可
# rect.col：指定矩形框的颜色
# rect.lwd：指定矩形框的线宽
# tl.pos：指定文本标签(变量名称)的位置，当type=full时，默认标签位置在左边和顶部(lt)，当type=lower时，默认标签在左边和对角线(ld)，当type=upper时，默认标签在顶部和对角线，d表示对角线，n表示不添加文本标签
# tl.cex：指定文本标签的大小
# tl.col：指定文本标签的颜色
# cl.pos：图例（颜色）位置，当type=upper或full时，图例在右表(r)，当type=lower时，图例在底部，不需要图例时，只需指定该参数为n
# addshade：只有当method=shade时，该参数才有用，参数值可以是negtive/positive和all，分表表示对负相关系数、正相关系数和所有相关系数添加阴影。注意：正相关系数的阴影是45度，负相关系数的阴影是135度
# shade.lwd：指定阴影的线宽
# shade.col：指定阴影线的颜色
col <- colorRampPalette(c('#BB4444','#EE9988','#FFFFFF','#77AADD','#4477AA'))
corrplot(mcor,method = 'shade',shade.col = NA,tl.col = 'black',tl.srt = 45)
corrplot(mcor,method = 'shade',shade.col = NA,tl.col = 'black',tl.srt = 45,
         col=col(200),addCoef.col = 'black',cl.pos = 'no',order = 'AOE')

# 绘制网络图
library(igraph)
gd <- graph(c(1,2, 3,3, 4,3, 3,4, 5,6, 5,6))
plot(gd)
# 无向图
gd <- graph(c(1,2,3,3,4,3,3,4,5,6,5,6),directed = F)
plot(gd,vertex.label=NA)
# fruchterman-reingold算法从数据框生成有向图
g <- graph.data.frame(madmen2,directed = T) # 生成图对象
# 移除多余空白边
par(mar=c(0,0,0,0))
plot(g,layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5)
# 从数据框生成无向图，
g <- graph.data.frame(madmen,directed = F)
par(mar=c(0,0,0,0))
plot(g,layout=layout.circle,vertex.size=8,vertex.label=V(g)$name,vertex.label.cex=.8,
     vertex.label.dist=.4,vertex.size =4)

# 3d 图
library(rgl)
data(iris)
iris$Species <- factor(iris$Species, levels = c("Versicolor", "virginica", "setosa"))
# 主成分分析
pca <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)
plot(pca, type="lines")
biplot(pca)
plot3d(pca$scores[,1:3],col=iris$Species)
text3d(pca$scores[,1:3],texts=rownames(iris))
text3d(pca$loadings[,1:3], texts=rownames(pca$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pca$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)
