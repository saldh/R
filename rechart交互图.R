# 1. 依赖关系检查、安装和加载
# 1.1 安装CRAN来源常用包
# 我要北京使用清华镜像下载超快，比官方快100倍，下载几乎不用等待，大家下载有问题可以更新自己较快的国内镜像，如中科大、英荔教育、兰大、同济，详见https://cran.r-project.org/mirrors.html
site="https://mirrors.tuna.tsinghua.edu.cn/CRAN"
# 参数解析、数据变换、绘图和开发包安装、安装依赖、ggplot主题
package_list <- c("optparse","reshape2","ggplot2","devtools","bindrcpp",
                  "ggthemes")
for(p in package_list){
  if(!suppressWarnings(suppressMessages(require(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))){
    install.packages(p, repos=site)
    suppressWarnings(suppressMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }
}

# 1.2 安装bioconductor常用包
# 参数解析、数据变换、绘图和开发包安装
package_list <- c("digest")
for(p in package_list){
  if(!suppressWarnings(suppressMessages(require(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))){
    source("https://bioconductor.org/biocLite.R")
    biocLite(p)
    suppressWarnings(suppressMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }
}

# 1.3 安装Github常用包
# 参数解析、数据变换、绘图和开发包安装
package_list <- c("kassambara/ggpubr","madlogos/recharts")
for(p in package_list){
  q=unlist(strsplit(p,split = "/"))[2]
  if(!suppressWarnings(suppressMessages(require(q, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))){
    install_github(p)
    suppressWarnings(suppressMessages(library(q, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }
}

# 2. 开始画图

# 2.1 散点图/气泡图

# 测试数据iris来自MASS
library(MASS)

# 查看测试数据，不同鸢尾个体花萼片和花瓣长度
head(iris)

# 绘制花萼宽为X轴，花辨宽为Y轴散点图
echartr(iris, x=Sepal.Width, y=Petal.Width)

# 图片为交互相图像，鼠标悬停有参考线和坐标，点可选并显示数值，
# 可切换为数据视图和局部缩放，可另存为png和html交互图

# 多个维度：series控制分组形状和着色
echartr(iris, x=Sepal.Width, y=Petal.Width, series=Species)

# 图中分组可以按图例开关，同时作标轴跟随移动

# 气泡图：weight控制气泡大小为花瓣长，type选择图表类型scatter/point/bubble类型
echartr(iris, Sepal.Width, Petal.Width, series = Species, 
        weight=Petal.Length, type='bubble')

# 散点图：点着连续数值对应颜色

# 将数据点按花瓣大小着色，类似热图
echartr(iris, Sepal.Width, Petal.Width, weight=Petal.Length) %>%
  setDataRange(calculable=TRUE, splitNumber=0, labels=c('Big','Small'),
               color=c('red', 'yellow', 'green'), valueRange=c(0, 2.5))

# 2.2 折线图

# 先改造下内置数据集：
aq <- airquality
head(aq)
aq$Date <- as.Date(paste('1973', aq$Month, aq$Day, sep='-'))
aq$Day <- as.character(aq$Day)
aq$Month <- factor(aq$Month, labels=c("May", "Jun", "Jul", "Aug", "Sep"))
head(aq)

# 绘制时间-温度变化折线图，设置标题和符号类型为空
echartr(aq, Date, Temp, type='line') %>%
  setTitle('NY Temperature May - Sep 1973') %>% setSymbols('none')

# 设置按月分组，符号为空心圆
echartr(aq, Day, Temp, Month, type='line') %>%
  setTitle('NY Temperature May - Sep 1973, by Month') %>% 
  setSymbols('emptycircle')
# 记得可以点选图例开关分组哟

# 带有时间轴，时间为月，可播放的小动图，是不是B格满满：
echartr(aq, Day, Temp, t=Month, type='line') %>%
  setTitle('NY Temperature May - Sep 1973, by Month') %>% 
  setSymbols('emptycircle')

# 堆叠面积图：type属性控制面积，subtype控制堆叠stack
echartr(aq, Day, Temp, Month, type='area', subtype='stack') %>%
  setTitle('NY Temperature May - Sep 1973, by Month') %>% 
  setSymbols('emptycircle')

# 2.3 饼图

# 基于泰坦尼克数据，重构内置数据集
# 显示数据结构，包括孩子、成人的生或死共4个表，包括1/2/3等舱和船员中性别分布
str(Titanic)
# 表格按行求和，再进行转换长表达
titanic <- data.table::melt(apply(Titanic, c(1,4), sum))
# 修改列名
names(titanic) <- c('Class', 'Survived', 'Count')
# knitr格式化表达
knitr::kable(titanic)

# 画饼图，按舱级别class分组显示数值和比例
echartr(titanic, Class, Count, type='pie') %>%
  setTitle('Titanic: N by Cabin Class')
# 右上角按扭可以切换为漏斗图

# 多个饼图：按Class分面，每面中显示存活率
echartr(titanic, Survived, Count, facet=Class, type='pie') %>%
  setTitle('Titanic: Survival Outcome by Cabin Class')

# 环图，中空饼图：按Class分面，每面中显示存活率
echartr(titanic, Survived, Count, facet=Class, type='ring') %>%
  setTitle('Titanic: Survival Outcome by Cabin Class')

# 信息图样环图：总和为100%，突出组间比较
ds <- data.frame(q=c('68% feel good', '29% feel bad', '3% have no feelings'),
                 a=c(68, 29, 3))
g <- echartr(ds, q, a, type='ring', subtype='info') %>% 
  setTheme('macarons', width=800, height=600) %>%
  setTitle('How do you feel?','ring_info', 
           pos=c('center','center', 'horizontal'))
g

# 南丁格尔玫瑰图：中空饼图，高度和比例正相关
echartr(titanic, Class, Count, facet=Survived, type='rose', subtype='radius') %>% 
  setTitle('Titanic: Survival Outcome by Cabin Class')

# 2.4 雷达图

# 筛选内置数据mtcars的某些行和列，重构内置数据集
cars = mtcars[c('Merc 450SE','Merc 450SL','Merc 450SLC'),
              c('mpg','disp','hp','qsec','wt','drat')]
cars$model <- rownames(cars)
cars <- data.table::melt(cars, id.vars='model')
names(cars) <- c('model', 'indicator', 'Parameter')
knitr::kable(cars)

# 单个雷达图：展示不同车的性能指标
echartr(cars, indicator, Parameter, model, type='radar', sub='fill') %>%
  setTitle('Merc 450SE  vs  450SL  vs  450SLC')

# 多个雷达图：按车型分面，每图展示车性能类型对应的数值
echartr(cars, indicator, Parameter, facet=model, type='radar') %>%
  setTitle('Merc 450SE  vs  450SL  vs  450SLC')

# 2.5 仪表盘图gauge plot

# 构造一个数据集：
data = data.frame(x=rep(c('KR/min', 'Kph'), 2), y=c(3.3, 56, 9.5, 88), 
                  z=c(rep('t1', 2), rep('t2', 2)))
# 表格展示数据
knitr::kable(data)

# 显示表中第一个值
echartr(data, x, y, type='gauge')

# 多个dashboard：按类型分类，可以显示两种速度类型
echartr(data, x, y, facet=x, type='gauge')

# 带时间轴：按时间轴动图
echartr(data, x, y, facet=x, t=z, type='gauge')

# 2.6 柱状混合图

# 数据筛选和变换
d <- data.table::dcast(mtcars, carb+gear~., mean, value.var='mpg')
names(d)[3] <- 'mean.mpg'
d$carb <- as.character(d$carb)
head(d)
# 绘图，按gear分组，三组分别为柱状图和线图
echartr(d, carb, "mean.mpg", gear, type=c('vbar', 'vbar', 'line')) %>% 
  setSymbols('emptycircle')
# 可以按右上角点选切换线图、柱状图、堆叠柱状图

# 3 修改图的细节

# 3.1 简单的两组散点图示例
g = echartr(mtcars, wt, mpg, factor(am, labels=c('Automatic', 'Manual')))
g

# 3.2 可以调用低级函数setSeries来修改第二组，点大小为8，并旋转30度
g %>% setSeries(series=2, symbolSize=8, symbolRotate=30)

# 3.3 给两个数据系列分别添加各自的均数标注线
g %>% addMarkLine(data=data.frame(type='average', name1='Avg'))

# 3.4 标注点markPoint
# 给第一个数据系列(‘Automatic’)标出最大值的点。
g %>% addMarkPoint(series=1, data=data.frame(type='max', name='Max'))

# 3.5 添加标题(红色)和副标题(超级链接到 https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html)。

link <- 'https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html'
g %>% setTitle('wt vs mpg', paste0('[Motor Trend](', link, ')'), 
               textStyle=list(color='red'))

# 3.6 修改图例(青柠色/绿黄色)，初始化时只选中第一系列(‘Automatic’)，可以手动选择
g %>% setLegend(selected='Automatic', textStyle=list(color='lime'))

# 3.7 修改工具箱显示语言为英文，并置于交互图右上角，垂直显示。
g %>% setToolbox(lang='en', pos=2)

# 3.8 添加缩放漫游控件(初始时不显示).
g %>% setDataZoom()

# 3.9 调整坐标轴，使x-和y-坐标交叉于零点。
g %>% setXAxis(min=0) %>% setYAxis(min=0)

# 主题Theme: 使用’dark’主题。可以选择的自带主题包括“macarons”, “infographic”, “blue”, “dark”, “gray”, “green”, “helianthus”, “macarons2”, “mint”, “red”, “roma”, “sakura”, “shine”, 和 “vintage”。
# 拖曳重算(Calculable)是Echarts特有的交互方式。在某些图(如饼图)中，效果比较好。
g %>% setTheme('dark', calculable=TRUE)

# 图标Symbols:把第1系列(‘Automatic’)的图标改为’heart’，第2系列(‘Manual’)的图标改为’star6’。
g %>% setSymbols(c('heart', 'star6'))

# 合起来Altogether: 你可以把上述步骤用%>%合起来。如果你对JavaScript很熟悉，你可以把JavaScript片段包在JS()函数中，以获得更好的效果。
g %>% setSeries(series=2, symbolSize=8, symbolRotate=30) %>% 
  addMarkLine(data=data.frame(type='average', name1='Avg')) %>%
  addMarkPoint(series=1, data=data.frame(type='max', name='Max')) %>%
  setTitle('wt vs mpg', paste0('[Motor Trend](', link, ')'), 
           textStyle=list(color='red')) %>%
  setLegend(selected='Automatic', textStyle=list(color='lime')) %>%
  setToolbox(lang='en', pos=2) %>% setDataZoom() %>% 
  setTheme('dark', calculable=TRUE) %>% setSymbols(c('heart', 'star6'))

# 4. 高B格类图

# 4.1  和弦图Chord Chart
mat <- as.data.frame(rbind(
  c(11975,  5871, 8916, 2868),
  c( 1951, 10048, 2060, 6171),
  c( 8010, 16145, 8090, 8045),
  c( 1013,   990,  940, 6907)
))
names(mat) <- c("group1", "group2", "group3", "group4")
mat$name <- names(mat)

echartr(mat, x=name, y=c(group1, group2, group3, group4), type="chord", 
        subtype='ribbon + asc + descsub + hidelab + scaletext') %>%
  setTitle("测试数据", subtitle="From d3.js", pos=5)

# 4.2 力导向布局图Force Chart

# 准备数据
grpmtx <- matrix(c(11975, 5871, 8916, 2868, 1951, 10048, 2060, 6171, 8010, 16145,
                   8090, 8045, 1013, 990, 940, 6907), byrow=TRUE, nrow=4)
grpmtx <- as.data.frame(grpmtx)
names(grpmtx) <- paste0('Group', 1:4)
grpmtx$Name <- paste0('Group', 1:4)
knitr::kable(grpmtx, align=c('lllll'))
# 点和边设置
nodes <- cbind(yuNetwork$nodes[,1], NA, yuNetwork$nodes[,2:3],
               stringsAsFactors=FALSE)
names(nodes) <- names(yuNetwork$links)
yu <- rbind(yuNetwork$links, nodes, stringsAsFactors=FALSE)
# 曲线连接
echartr(yu, c(source, target), weight, relation, type='force') %>% 
  setTitle("Yu Family of Shaoxing") %>% setTheme(palette=c(
    'tan3','green3','green2','lawngreen','olivedrab1'))

# 4.3 词云

# 获取实时百度热词，不同时间画的都不同
getBaiduHot <- function(url, top=30, HTMLencoding=NULL){
  baiduhot <- paste0(readLines(url), collapse="")
  charset <- gsub('^.+charset=([[:alnum:]-]+?)[^[:alnum:]-].+$', "\\1", 
                  baiduhot)
  if (is.null(HTMLencoding)) if (!is.null(charset)) HTMLencoding <- charset
  baiduhot <- stringr::str_conv(baiduhot, HTMLencoding)
  hotword <- gsub(".+?<a class=\"list-title\"[^>]+?>([^<>]+?)</a>.+?<span class=\"icon-(rise|fair|fall)\">(\\d+?)</span>.+?","\\1\t\\3\t\\2\t", baiduhot)
  hotword <- enc2native(gsub("^(.+?)\t{4,}.+$","\\1", hotword))
  hotword <- t(matrix(unlist(strsplit(hotword,"\t")), nrow=3))
  hotword <- as.data.frame(hotword, stringsAsFactors=FALSE)
  names(hotword) <- c("Keyword", "Freq", "Trend")
  hotword$Freq <- as.numeric(hotword$Freq)
  hotword <- hotword[order(hotword$Freq, decreasing=TRUE),]
  return(hotword[1:top,])
}
hotword <- getBaiduHot("http://top.baidu.com/buzz?b=1", HTMLencoding='GBK')
knitr::kable(hotword)

# 词云展示
echartr(hotword, Keyword, Freq, type='wordCloud') %>% 
  setTitle('Baidu Hot Word Top30 (realtime)', as.character(Sys.time()))

# 按数据系列着色Color by Series
echartr(hotword, Keyword, Freq, Trend, type='wordCloud') %>% 
  setTitle('Baidu Hot Word Top30 (realtime)', as.character(Sys.time()))

# 带时间轴With Timeline

# 获取今日和七日两个榜单的网页并转为数据框，合并。

hotword$t <- 'Realtime'
hotword1 <- getBaiduHot("http://top.baidu.com/buzz?b=341&fr=topbuzz_b1", 
                        HTMLencoding = 'GBK')
hotword1$t <- 'Today'
hotword2 <- getBaiduHot("http://top.baidu.com/buzz?b=42&c=513&fr=topbuzz_b341",
                        HTMLencoding = 'GBK')
hotword2$t <- '7-days'
hotword <- do.call('rbind', list(hotword, hotword1, hotword2))
hotword$t <- factor(hotword$t, levels=c('Realtime', 'Today', '7-days'))
# 然后作图。

g <- echartr(hotword, Keyword, Freq, t=t, type='wordCloud') %>% 
  setTitle('Baidu Hot Word Top30')
g

# 5. System information
sessionInfo()
