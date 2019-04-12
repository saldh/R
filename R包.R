#R函数包
search()
searchpaths()
install.packages('mgcv')
install.packages('D:/~',repos = NULL,type = 'source')
#加载包
require(mgcv)
library(mgcv)
#卸载包
detach('package:mgcv',unload = TRUE)#不使用
remove.packages('mgcv')#卸载
#查看R包中函数
help(package = 'base')#帮助文档
library(help = 'base')#说明书
vignette()
#查看函数
?mean
help(mean)
example(sum)
example(persp)
help.search('multivariate normal')
apropos('help')#寻找指定字符串的函数
#产看函数的源代码
fivenum#未封装函数

mean
methods(mean)#已封装函数
mean.default

plot
methods(plot)
plot.default#plot.xy , invisible
plot.stl*
getAnywhere(plot.stl)#查找带*函数
#常用领域的包
install.packages('data.table') #处理大型数据表
install.packages('ggplot2')
zoo #时间序列处理
stringr#正则表达式做批量字符串处理
reshape2#横向、总想数据变化
dplyr#处理data.frame，data.table以及数据库为基础
     #的数据，实现选择变化

install.packages('VennDiagram')
install.packages('ggplot2')
install.packages('pheatmap')
install.packages('reshape')
install.packages('devtools')