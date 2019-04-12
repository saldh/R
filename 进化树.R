install.packages('ggtree')
source("https://bioconductor.org/biocLite.R")
biocLite(c("ggtree"))
library('ggtree')
tree <- read.tree("D:/share/q5/result/tax_rep_seqs.tree")
tax <- read.table("D:/share/q5/result/tax_rep_seqs.tax",
                  row.names = 1)
tax
# 物种注释等级标签，共七级，但细菌末分类物种太多，一般只能在门、纲、目水平比较确定
colnames(tax) = c("kingdom","phylum",'class','order')
# 按门水平建树并上色
## 给每个OTU按门分类分组，此处可以更改为其它分类级别
#如纲、目等，即phylum替换为order或class即可
groupInfo <- split(row.names(tax), tax$phylum) # OTU and phylum for group
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
dev.off()

# 树+丰度热图
# 思路：矩形树右端添加每个样品的表达丰度。
## 读取OTU表
otu_table = read.delim("D:/share/q5/result/otu_table_tax.txt",  header=T, sep="\t")
## 读取实验设计
design = read.table("design.txt", header=T, row.names= 1, sep="\t")
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
p = ggtree(tree, aes(color=group))+  theme(legend.position = "right")+geom_tiplab(size=3, align=TRUE, linesize=.5)
p
pdf(file="ggtree_heat_sample.pdf", width=9, height=5)
## 添加数字矩阵
## offset设置两者间距，用于解决图重叠问题；width设置热图相对树图的宽度，解决热图和树图大小关系；font.size设置热图文字大小，解决文字过大重叠；colnames_angle调整热图标签角度，解决文字重叠问题；hjust调整热图标签位置，解决文字与热图重叠问题。
gheatmap(p, tax_per, offset = .15, width=3, font.size=3, colnames_angle=-45, hjust=-.1)
dev.off()
# 树+ 组均值热图
## 有时样本过多也无法展示和阅读，需要求各组均值展示：需要将分组信息添加至样品相对丰度表，再分类汇总
## 提取实验设计中的分组信息
sampFile = as.data.frame(sub_design$genotype,row.names = row.names(sub_design))
colnames(sampFile)[1] = "group"
## OTU表转置，让样品名为行
mat_t = t(tax_per)
## 合并分组信息至丰度矩阵，并去除样品名列
mat_t2 = merge(sampFile, mat_t, by="row.names")[,-1]
## 按组求均值
mat_mean = aggregate(mat_t2[,-1], by=mat_t2[1], FUN=mean) # mean
## 去除非数据列并转置
mat_mean_final = do.call(rbind, mat_mean)[-1,]
## 重命名列名为组名
colnames(mat_mean_final) = mat_mean$group
## 按组均值热图
pdf(file="ggtree_heat_group.pdf", width=7, height=5)
gheatmap(p, mat_mean_final, offset = .05, width=1, font.size=3, hjust=-.1)
dev.off()


install.packages('Rcpp')
install.packages('yaml')
install.packages("C:/Users/10020/Documents/R/table2itol-master/table2itol.R")
source("C:/Users/10020/Documents/R/table2itol-master/table2itol.R")
package_list = c("grid","ggplot2","gridExtra","vegan","reshape2","readODS")
for(p in package_list){
  if(!suppressWarnings(suppressMessages(require(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))){
    install.packages(p, repos=site)
    suppressWarnings(suppressMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }
}
source("http://bioconductor.org/biocLite.R")
biocLite(c("optparse", "plotrix", "readODS", "readxl", "yaml"))
source("C:/Users/10020/Documents/R/table2itol-master/table2itol.R")
## 方案1. 外圈颜色、形状分类和丰度方案
# annotation.txt OTU对应物种注释和丰度，
#-a 找不到输入列将终止运行（默认不执行）-c 将整数列转换为factor或具有小数点的数字，-t 偏离提示标签时转换ID列，-w 颜色带，区域宽度等， -D输出目录，-i OTU列名，-l OTU显示名称如种/属/科名，
table2itol.R -i TID -l NAME rep_seqs_k1.csv
create_itol_files(infiles = "D:/share/q5/result/rep_seqs_k1.csv",
                  identifier = "TID", label = "Name", na.strings = "X")
table2itol/table2itol.R -a -c double -D plan1 -i OTUID -l Genus -t %s -w 0.5 annotation.txt
# 生成注释文件中每列为单独一个文件
# 方案2. 生成丰度柱形图注释文件
Rscript table2itol/table2itol.R -a -d -c none -D plan2 -b Phylum -i OTUID -l Genus -t %s -w 0.5 annotation.txt
# 方案3. 生成热图注释文件
Rscript table2itol/table2itol.R -c keep -D plan3 -i OTUID -t %s otutab.txt
# 方案4. 将整数转化成因子生成注释文件
Rscript table2itol/table2itol.R -a -c factor -D plan4 -i OTUID -l Genus -t %s -w 0 annotation.txt
# 方案5. 自定义颜色
Rscript table2itol/table2itol.R -a -C table2itol/tests/INPUT/colours_1.yml -c double -D plan5 \
-i OTUID -l Genus -t %s -w 0.5 annotation.tx


# microbiomeViz 绘制cladogram
# 下载宏基因组数据，第一列注释信息，第二列相对丰度
download.file("https://bitbucket.org/biobakery/biobakery/raw/tip/demos/biobakery_demos/data/metaphlan2/output/SRS014459-Stool_profile.txt", 'SRS014459-Stool_profile.txt') 
knitr::kable(head(read.table('SRS014459-Stool_profile.txt')))
source("https://bioconductor.org/biocLite.R") 
biocLite("ggtree") 
devtools::install_github("lch14forever/microbiomeViz") 
library(microbiomeViz)
df <- read.table("http://bailab.genetics.ac.cn/markdown/R/microbiomeViz/merge121d_abundance_table.txt", head=TRUE, stringsAsFactors = FALSE) 
# 计算均值用于呈现结点大小 
dat <- data.frame(V1=df[,1], V2=rowMeans(df[,-1]), stringsAsFactors = FALSE)
# 用物种和丰度生成树骨架 
tr <- parseMetaphlanTSV(dat, node.size.offset=2, node.size.scale=0.8) 
p <- tree.backbone(tr, size=0.5) 
p
# 读取需要颜色标注的差异物种列表 ，本质上是两和对应
lefse_lists = data.frame(node=c('s__Haemophilus_parainfluenzae','p__Proteobacteria',
                                'f__Veillonellaceae','o__Selenomonadales', 
                                'c__Negativicutes', 's__Streptococcus_parasanguinis',
                                'p__Firmicutes','f__Streptococcaceae', 'g__Streptococcus',
                                'o__Lactobacillales', 'c__Bacilli','s__Streptococcus_mitis'),
                         color=c(rep('darkgreen',6), rep('red','6')), stringsAsFactors = FALSE ) 
# 注释树
p <- clade.anno(p, lefse_lists, alpha=0.3) 
p