data <- read.csv('level2-3.csv',header = T)
library(reshape2)
library(ggplot2)
data_m <- melt(data, id.vars=c("index"))
head(data_m)
data_m$value <- log10(data_m$value +1)
data_m$index = factor(data_m$index,levels = c("AX96","AX91","AX94","AX105",
                                              "AX92","AX108","AX89","AX93","AX95","AX90"))
p <- ggplot(data_m, aes(x=index,y=variable)) + 
  xlab("samples") + theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(legend.key=element_blank())  + 
  theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1)) + 
  theme(legend.position="right") +  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low = "#FFFAF0", high = '#EE5C42')
p

ggsave(p, filename="chorain-heatmap2.pdf", width=15,
       height=10, units=c("cm"),colormodel="srgb")


#pheatmap
library('pheatmap')
data1 <- read.csv('D:/share/q2/result/level-4.csv',
                  header = T,sep = ',',row.names = 1)
head(data1)
str(data1)
x <- as.matrix(data1[1:30,])
str(x)
#cluster_cols行聚类fontsize文字大小treeheight行列高度
#filename保存文件cellhight格子高度
#
pheatmap(log10(x+1),main = 'Heatmap',
         filename = NA,cluster_rows = TRUE,
         cluster_cols = TRUE,treeheight_col = 20,
         treeheight_row = 20,cellhight = 4,cellwidth = 20,
         fontsize = 7, fontsize_col = 8,fontsize_row = 8,
         border_color = "white",
         color = colorRampPalette(c("#FFFAF0","#FFE4E1",'#EE5C42'))(30))

#
install.packages("gplots")
library(gplots)
class(data)
data <- read.csv('D:/share/q2/result/level2.csv')
data_m <- as.matrix(t(data))
heatmap.2(data_m,Rowv=TRUE, Colv=FALSE, scale="column", 
          trace="none", col=redgreen, xlab="index", 
          ylab="variable", margins=c(10,15))

?geom_bar

library('ggtree')
tree <- read.tree("D:/share/q5/result/rep_seqs_k5.tree")
tax <- read.table("D:/share/q5/result/rep_seqs_k5.tax",
                  row.names = 1)
# 物种注释等级标签，共七级，但细菌末分类物种太多，一般只能在门、纲、目水平比较确定
colnames(tax) = c("kingdom","phylum",'class','order')
# 按门水平建树并上色
## 给每个OTU按门分类分组，此处可以更改为其它分类级别
#如纲、目等，即phylum替换为order或class即可
groupInfo <- split(row.names(tax), tax$order) # OTU and phylum for group
groupInfo
## 将分组信息添加到树中
tree <- groupOTU(tree, groupInfo)
tree
# 画树，按组上色
ggtree(tree, aes(color=group))+  
  theme(legend.position = "right")+geom_tiplab(size=3)

# 画圈图并保存PDF
pdf(file="ggtree_circle_color.pdf", width=9, height=5)
## tiplab2保证标签自动角度，默认无图例，要显示需要+theme
ggtree(tree, layout="fan", ladderize = FALSE,
       branch.length = "none",aes(color=group))+
  geom_tiplab2(size=3)+ theme(legend.position = "right")

## 读取OTU表
otu_table = read.delim("D:/share/q5/temp/otu_table.txt", 
                       row.names= 1,  header=T, sep="\t")
otu_table
## 读取实验设计
design = read.table("D:/share/q5/metadata.tsv", 
                    header=T, row.names= 1, sep="\t")
design
## 取实验设计和OTU表中的交集:样本可能由于实验或测序量不足而舍弃掉，每次分析都要筛选数据
idx=intersect(rownames(design),colnames(otu_table))
sub_design=design[idx,]
## 按实验设计的样品顺序重排列
otu_table=otu_table[,idx]
## 将OTU表count转换为百分比
norm = t(t(otu_table)/colSums(otu_table,na=T)) * 100 # normalization to total 100
## 筛选树中OTU对应的数据
tax_per = norm[rownames(tax),]

## 保存树图于变量，align调置树OTU文字对齐，linesize设置虚线精细
p = ggtree(tree, aes(color=group))+  theme(legend.position = "right")+
  geom_tiplab( align=TRUE, linesize=.5) 
p
pdf(file="ggtree_heat_sample.pdf", width=9, height=5)
## 添加数字矩阵
## offset设置两者间距，用于解决图重叠问题；width设置热图相对树图的宽度，解决热图和树图大小关系；font.size设置热图文字大小，解决文字过大重叠；colnames_angle调整热图标签角度，解决文字重叠问题；hjust调整热图标签位置，解决文字与热图重叠问题。
gheatmap(p, tax_per, offset = .15, width=3, font.size=3,
         colnames_angle=-45, hjust=-.1)
dev.off()


