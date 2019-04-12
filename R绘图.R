#物种注释柱状图
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
library(reshape2)
data1 <- read.csv('level-4.csv')
data_m <- melt(data1, id.vars=c("index"))
data_m$index = factor(data_m$index,levels = c("AX96","AX91","AX94","AX105",
                                              "AX92","AX108","AX89","AX93","AX95","AX90"))
head(data_m)
ggplot(data = data_m,aes(x = index, y = value, fill = variable))+
       geom_bar(stat = 'identity',position = 'fill')+
  theme(axis.text.x=element_text(angle=45))
ggsave('chorain-bar.pdf')#保存
#barplot ???
data1 <- read.csv('D:/share/q2/result/level2.csv',header = T,
                  sep = ',',row.names = 1)
class(data1)

n <- dim(data1)[1]
barplot(as.matrix(data1),col = rainbow(n),beside = TRUE)
legend("topleft",row.names(data1),bty = 'n',fill = rainbow(n))
#柱状图
otu <- read.csv('D:/share/q2/result/level2.csv',header = T,
                sep = ',',row.names = 1)
#选取第一行
n <- dim(otu)[1]
#求组成比例
potu <- apply(otu, 2,function(x)x/sum(x))
#
par(mar = c(3,3,8,2))
barplot(potu,col = rainbow(dim(otu)[1]))
legend(1,1.4,row.names(otu),bty = 'n',
       fill = rainbow(n),ncol = 4,xpd = T)#加注释

setwd(" ")
cars <- c(2,3,4,5,6,11)
colors <- rainbow(length(cars))
labels <- c('a','b','c','d','e')
pie(cars,main = 'Cars',col = colors,
    labels = labels, cex = 1)
points(0,0,cex=10,pch=21,bg='white')
png("pie.png")
dev.off()

x = c(-5:5)
y = x*x
plot(x,y,type = 'o',pch = 2)

#barplot
?barplot
data <- read.csv('D:/share/q2/result/level2.csv')
d
barplot(data_m , x = variable,ylab = 'phylm',
        main = 'gut microbiota',sub = 'bases')


par(mfrow = c(1,1),col = 'black')
x = runif(50,0,2)
y = runif(50,0,2)
plot(x,y,type = 'n',ann = F,axes = F)
points(x,y)
#
axis(1，t = seq(from = 0,to = 2,bu = 0.25))
#
box()
title(xlab = 'x lab',ylab = 'y lab',main = 'main',sub = 'sub')

#VN
install.packages('VennDiagram')
install.packages('futile.logger')
library(VennDiagram)
library(futile.logger)
data <- list(A=1:10,B=3:8,c=6:9,D=2:9)
data
#
vg <- venn.diagram(data,fill = c('red',"green","blue","orange"),
                   alpha = c(0.5,0.5,0.5,0.5),cex = 2,
                   cat.fontface= 4,filename = NULL)
library(grid)
grid.draw(vg)

#pheatmap绘制热图
library('pheatmap')
data1 <- read.csv('D:/share/q2/result/level2-3.csv',
                  header = T,sep = ',',row.names = 1)
head(data1)
str(data1)
x <- as.matrix(data1[1:10,])
str(x)
#cluster_cols行聚类fontsize文字大小treeheight行列高度
#filename保存文件cellhight格子高度
#
pheatmap(log10(x+1),scale = 'column',main = 'Heatmap',
         filename = NA,cluster_rows = TRUE,
         cluster_cols = TRUE,treeheight_col = 20,
         treeheight_row = 20,cellhight = 4,cellwidth = 20,
         fontsize = 7, fontsize_col = 8,fontsize_row = 8,
         border_color = "white",
         color = colorRampPalette(c('white','yellow','red','blue'))(10))
#箱线图
adiv <- read.csv('',header = T,sep = ',')#
par(mfrow = c(2,2))
boxplot(ACE~Group,adiv)
boxplot(simpson ~Group.adiv)
boxplot(shannon ~Group.adiv)
boxplot(chao1 ~Group.adiv)
#venn diagram
data1 <- read.csv('D:/share/q2/result/level2-2.csv',header = T,sep = ',')
venn.diagram(data1,fill= index,alpha=0.5,filename = NA)
#散点图
library(ggplot2)
x <- rnorm(100,14,5)
y <- x + rnorm(100,0,1)
ggplot(data= NULL,aes(x = x, y = y)) +
  geom_point(color = 'darkred') +
  annotate("text", x = 13, y = 20, parse = T,
           label = "x[1] == x[2]")#
#散点图
attach(iris)
p <- ggplot(data= iris ,aes(x= Sepal.Length , y= Sepal.Width))
p + geom_point(aes(colours = Species)) + stat_smooth() +
  labs(title = "iris") + 
  theme_classic() + theme_bw() +
  annotate("text", x= 7, y = 4, parse = T , label = "x[1] == x[2]",
           size=6,family ='self',fontface='italic',colour= 'darkred')

#PCA分析 prcomp()
otu <- read.table('',header = T,row.names = 1)
otu.data <- otu[,1:10]#提取1到10列
otu.group <- otu$group #数据有分组
pca.out <- princomp(otu.data,cor = TRUE)#用相关性矩阵进行分析
summary(pca.out,loadings = T)#loadings显示载荷
pca_data <- predict(pca.out) #计算各个样本主成分的数据
color[which(otu.group == 'A')]='blue'
color[which]
plot(pca_data[,1],pca_data[,2],xlab = 'Comp1',
     ylab = 'Colp2',col = color,pch = 19)#pch点的大小
legend('bottomleft',c("A","B","C"),col = c("blue","green","red"),pch = 19)
install.packages('scatterplot3d')
library(scatterplot3d)
scatterplot3d(pca_data[,1],pca_data[,2],pca_data[,3],color = color,
              pch= 19, xlab = 'COMP1',ylab = 'COMP2',zlab = 'COMP3',
              cex.symbol = 1.2)
legend('topleft',c("a","b","c"),col = c("blue","green","red"),pch = 19)
