# 差异分析
# 样品相关性
library(gplots)
library(RColorBrewer)
design <- read.table('mappingfile.txt',row.names = 1,header = T,comment.char ='',
                     sep = '\t')
otu_table <- read.delim('otu_table4.txt',row.names = 1,header = T,sep = '\t')
# 过滤数据并排序
idx <- rownames(design) %in% colnames(otu_table)
sub_design <- design[idx,]
count <- otu_table[,rownames(sub_design)]
# 转换原始数据为百分比
norm <- t(t(count)/colSums(count,na=T))*100
# 计算样品间相关系数
sim <- cor(norm,method = 'pearson')
# 使用热图可视化
heatmap.2(sim,Rowv =T,Colv = T,dendrogram = 'both',
          trace = 'none',col = rev(colorRampPalette(brewer.pal(11,'RdYlGn'))(256)),
          density.info = 'none')

# 差异OTU
library(limma)
library(edgeR)
design <- read.table('mappingfile.txt',row.names = 1,header = T,
                     comment.char ='',sep = '\t')
otu_table <- read.delim('otu_table4.txt',row.names = 1,
                        header = T,sep = '\t')
# 过滤数据并排序
idx <- rownames(design) %in% colnames(otu_table)
sub_design <- design[idx,]
count <- otu_table[,rownames(sub_design)]
# 创建列表
d <- DGEList(counts = count,group = sub_design$genotype)
d <- calcNormFactors(d)
# 生成实验数据矩阵
design_matrix <- model.matrix( ~ 0 + d$samples$group) # 建立分组变量
colnames(design_matrix) <- levels(sub_design$genotype)
# 计算普通离散度
d2 <- estimateGLMCommonDisp(d,design_matrix)
# 计算otu间范围内离散度
d2 <- estimateGLMTagwiseDisp(d2,design_matrix)
# 拟合广义线性模型
fit <- glmFit(d2,design_matrix)
# 设置比较组
design_compare <- makeContrasts(contrasts = 'OE-WT',levels = design_matrix)
# 组间比较，统计FC,P值
lrt <- glmLRT(fit,contrast = design_compare)
# FDR检验，控制假阳性率小于5%
fdr_lrt <- decideTestsDGE(lrt,adjust.method = 'fdr',p.value = 0.05)
# 导出计算结果
x <- lrt$table
x$sig <- fdr_lrt
enriched <- row.names(subset(x,sig==1))
depleted <- row.names(subset(x,sig==-1))
# 绘图
pair_group <- subset(sub_design, genotype %in% c('OE','WT'))
# 显著差异OTU
DE <- c(enriched,depleted)
sub_norm <- as.matrix(norm[DE,rownames(pair_group)])
# colnames(sun_norm) <- gsub('DM','KO',colnames(sub_norm),perl=T)
# 行聚类，行做系统树
heatmap.2(sub_norm,scale = 'row',Colv = F,Rowv = F,dendrogram = 'none',
          col = rev(colorRampPalette(brewer.pal(11,'RdYlGn'))(256)),
          cexCol = 1,keysize = 1,density.info = 'none',mail=NULL,trace='none')


# 曼哈顿图
# 读取数据，添加列名
taxonomy <- read.delim('rep_seqs_tax2.txt',row.names = 1,header = F,sep = '\t')
colnames(taxonomy) <- c('kingdom','phylum','class','order',
                        'family','genus','species','evalue')
# 标记差异otu类型
x$level <- as.factor(ifelse(x$sig==1,'enriched',
                            ifelse(x$sig==-1,'depleted','nosig')))
x$otu <- rownames(x)
# 转换pvalue为负对数
x$neglogp <- -log(x$PValue)
# taxonomy排序，并筛选OTU表中的存在的
library(dplyr)
taxonomy$id <- rownames(taxonomy)
taxonomy <- arrange(taxonomy,phylum,class,order,family,genus,species)
rownames(taxonomy) <- taxonomy$id
idx <- rownames(taxonomy) %in% x$otu
tax <- taxonomy[idx,]
# 手动筛选显著组
x <- x[rownames(tax),]
x$tax <- gsub('p__','',tax$phylum , perl = T)
top_phylum <- c('Bacteroidetes','Firmicutes','Planctonycetes','Proteobacteria','Verrucomicrobia')
x[!(x$tax %in% top_phylum),]$tax ='Low Abundance'
# 设置各类的level对应顺序
x$otu <- factor(x$otu,levels=x$otu) # 设置x顺序
x$level <-factor(x$level,levels = c('enriched','depleted','nosig'))
levels(x$tax) <- c(top_phylum,'Low Abundence')
# 调整y轴范围
x[x$neglogp>15,]$neglogp <- 15
# 绘制曼哈顿图
# 添加显著阈值线
FDR <- min(x$neglogp[x$level =='depleted'])
library(ggplot2)
p <- ggplot(x,aes(x=otu,y=neglogp,color=tax,size=logCPM,shape=level))+
  geom_point(alpha=0.7) + geom_hline(yintercept = FDR,linetype =5,color ='yellow')+
  scale_shape_manual(values=c(17,25,20)) +
  scale_size(breaks = c(5,10,15)) +
  labs(x='OTU',y='-loge(p)',title = 'OE-WT')+ theme_bw()+ 
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        legend.position = 'top')
p

# 韦恩图
# 统计差异OTU数量
OE_enriched <- row.names(subset(x,sig==1))
length(OE_enriched)
OE_depleted <- row.names(subset(x,sig==-1))
length(OE_depleted)
# 计算KO WT间差异OTU
com.ko_wt <- makeContrasts(contrasts = 'KO-WT',levels =design_matrix) 
lrt2 <- glmLRT(fit,contrast = com.ko_wt)
de_lrt <- decideTestsDGE(lrt2,adjust.method = 'fdr',p.value = 0.05)
y <- lrt2$table
y$sig <- de_lrt
KO_enriched <- row.names(subset(x,sig==1))
KO_depleted <- row.names(subset(x,sig==-1)) 
library(grid)
library(futile.logger)
library(VennDiagram)
color_v <- c('dodgerblue','goldenrod1','seagreen3','orchid3')
p <- venn.diagram(x= list(OE_depleted=OE_depleted,KO_depleted=KO_depleted),filename = NULL,
                  fill=color_v[1:2])
grid.draw(p)
p <- venn.diagram(x= list(OE_enriched=OE_enriched,OE_depleted=OE_depleted,
                          KO_depleted=KO_depleted,KO_enriched=KO_enriched),
                  filename = NULL, fill=color_v[1:4])

# LDA线性判别
# Installation
devtools::install_github('fawda123/ggord')
# 加载lda包
library(MASS)
library(ggord)
# 读入实验设计
design = read.table("mappingfile.txt", header=T, row.names= 1, 
                    sep="\t",check.names = F,comment.char = '') 
# 读取OTU表
otu_table = read.delim("otu_table4.txt", row.names= 1,  header=T, sep="\t")
# 转换原始数据为百分比
norm = t(t(otu_table)/colSums(otu_table,na=T)) * 100 
# 按mad值排序取前6波动最大的OTUs
mad.5 = head(norm[order(apply(norm,1,mad), decreasing=T),],n=6)
row.names(mad.5)=c("Streptophyta","Rubrivivax","Methylibium",
                   "Streptosporangiaceae","Streptomyces","Niastella")
data=as.data.frame(t(mad.5))
# 添加分组信息
data$group <- design[rownames(data),]$genotype
# 按实验基因组分组排序
ord <- lda(group ~ ., data)
# 使用ggbiplot展示lda(可选)
library('scales','plyr','grid')
library(ggbiplot)
ggbiplot(ord, obs.scale = 1, var.scale = 1,
         groups = data$group, ellipse = TRUE,var.axes = F)
# 展示LDA分析
library(ggord)
p <- ggord(ord, data$group, ellipse_pro = 0.68)
p

