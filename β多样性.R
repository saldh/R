# β多样性
library(phyloseq)
library(ggplot2)
library(qiimer)
qiimedata <- import_qiime(otufilename = 'otu_table_tax.txt',
                          mapfilename = 'mappingfile.txt',
                          treefilename = 'rep_seqs.tree',
                          refseqfilename = 'rep_seqs4.fa')
# 从导入的数据中提取 otu表格 
# otu<-otu_table(qiimedata) ，可选的 otu表格提取方法 表格提取方法 
otu <- qiimedata@otu_table@.Data 
# 计算各个 otu检测到的总序列数 
sum_of_otus<-colSums(t(otu)) 
# 获取总序列数大于 10的 
selected_otu<-names(sum_of_otus)[sum_of_otus>10] 
# 筛选总序列数大于 10的 otu的 phyloseq数据 
sub_qiimedata <- prune_taxa(selected_otu, qiimedata) 
# 计算样本间 Bray-Curtis距离矩阵， method 可选 " wunifrac ", " unifrac " ，"jaccard"等 
bray_curtis <- distance(qiimedata, method='bray') 
# 保存距离矩阵 write.table(as.matrix(bray_curtis),"bray_curtis.txt",sep = '\t',quote = FALSE,col.names = NA) 
# 基于 Bray-Curtis距离矩阵的 距离矩阵的 PCoA排序分析 
pcoa_of_bray_curtis<-ordinate(physeq=qiimedata,
                              distance = 'bray',method = "PCoA") 
# 将 PCoA排序分析结果可视化 
p<-plot_ordination(qiimedata, pcoa_of_bray_curtis, type="samples", 
                   color="Group1",shape = "Group1") 
# 对图片进行适当修饰 
# 用 scale_colour_manual(values=c())自定义颜色，可查的 自定义颜色，可查的 16进制对照表 
p<-p+ scale_colour_manual(values=c("#DC143C","#808000","#00CED1")) +
  geom_point(size=2) +ggtitle("PCoA of Bray-Curtis distance")+
  theme(text = element_text(size = 15)) 
# 基于 Bray-Curtis距离矩阵的 距离矩阵的 NMDS排序分析 nmds_of_bray_curtis<-ordinate(physeq=qiimedata,distance = 'bray',method = "NMDS") 
# 将 NMDS排序分析结果可视化 # color=“Group1”指定不同分组的点染颜色 ”指定不同分组的点染颜色 
p<-plot_ordination(qiimedata, nmds_of_bray_curtis, type="samples", color="Group1") 
# 对图片进行适当修饰， stat_ellipse()加椭圆， ggtitle()加标题 
p<-p + geom_point(size=3) +ggtitle("NMDS of Bray-Curtis distance") + 
  stat_ellipse()+theme(text = element_text(size = 15)) 
# 保存 
ggsave(plot = p,'nmds_of_bary_curtis.pdf',dpi = 300,width = 7,height = 6)

1 # pCoA 展示样品间差异
library("ggplot2") 
library('permute','lattice')
library("vegan")
design <- read.table('mappingfile.txt',header = T,row.names = 1,sep = '\t',
                     comment.char = '',check.names = F)
# 读入距离矩阵
bray_curtis <- read.table('beta/bray_curtis_otu_table_css.txt',sep = '\t',
                          header = T,check.names = F)
# 过滤数据并排序
idx <- rownames(design) %in% colnames(bray_curtis)
sub_design <- design[idx,]
# 距离矩阵设置自己并重新排序
bray_curtis <- bray_curtis[rownames(sub_design),rownames(sub_design)]
# 将距离矩阵进行主坐标分析 k是维数 eig是特征值
pcoa <- cmdscale(bray_curtis,k=3,eig = T)
# 将坐标字符串数据转换为数据框
points <- as.data.frame(pcoa$points)
colnames(points) <- c('x','y','z')
eig <- pcoa$eig
# 合并数据
points_bind <- cbind(points,sub_design[match(rownames(points),
                                        rownames(sub_design)),])
# 绘制1，2轴，ellipse添加圈图
p <- ggplot(points_bind,aes(x,y,color = genotype))+
  geom_point(alpha=0.7,size=2) + theme_bw() +
  labs(x=paste('PCoA 1(',format(100*eig[1]/sum(eig),digits = 4),'%)',sep = ' '),
       y=paste('PCoA 2(',format(100*eig[2]/sum(eig),digits = 4),'%)',sep = ' '),
       title='bray_curtis PCoA') + stat_ellipse(aes(x,y,color=genotype))
p
#绘制 1/3轴， plot PCo 1 and 3 
points_bind$siteXcompt=paste(points_bind$site,points_bind$compartment,sep = "") 
p = ggplot(points_bind, aes(x=x, y=z, color=genotype)) +
  geom_point(alpha=.7, size=2) + 
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""), 
       y=paste("PCoA 3 (", format(100 * eig[3] / sum(eig), digits=4), "%)", sep=""), 
       title="bray_curtis PCoA")
ggsave("beta_pcoa_day_bray_curtis3.pdf", p, width = 4, height = 2.5)

# 统计组间差异
design2 <- subset(sub_design,genotype %in% c('OE','KO'))
# 获取对应距离矩阵并排序
sub_distance <- bray_curtis[rownames(design2),rownames(design2)]
sub_distance <- as.dist(sub_distance,diag = F,
                        upper = F)
# 计算组间差异显著性水平
adonis_table <- adonis(sub_distance ~ genotype,data = design2,
                       permutations = 10000)
adonis_pvalue <- adonis_table$aov.tab$`Pr(>F)`[1]
adonis_pvalue

2 # PCA::ggbiplot
library(purrr,plyr,grid)
library(scales)
library(ggbiplot)
design <- read.table('mappingfile.txt',header = T,row.names = 1,sep = '\t',
                     comment.char = '')
otu_table <- read.delim('otu_table4.txt',row.names = 1,header = T,sep = '\t')
# 过滤数据并排序
idx <- rownames(design) %in% colnames(otu_table)
sub_design <- design[idx,]
count <- otu_table[,rownames(sub_design)]
# 基于OTU表计算PCA,scale.进行标准化
otu_pca <- prcomp(t(count),scale. = T)
groups <- factor(count[1,])
ggbiplot(otu_pca, obs.scale = 1,var.scale = 1,
         groups = sub_design$genotype, ellipse = T,var.axes=F)
# 主要差异菌与主成分关系
# 转换原始数据为百分比, MAD= Mean Absolute deviation平均绝对偏差
norm <- t(t(count)/colSums(count,na=T))*100
mad.5 <- norm[apply(norm,1, mad)>0.5]
# 或者按mad值排序取前6波动最大的OTUs
mad.5 <- head(norm[order(apply(norm,1,mad),decreasing = T),],n=6)
# 计算pca和菌与菌轴的相关性
otu_pca2 <- prcomp(t(mad.5))
rownames(otu_pca$rotation) <- c("Streptophyta","Rubrivivax","Methylibium",
                                "Streptosporangiaceae","Streptomyces","Niastella")
ggbiplot(otu_pca,obs.scale = 1,var.scale = 1,
         groups = sub_design$genotype, ellipse = T, var.axes = T)
# pca3d
library(rgl)
library(pca3d)
head(otu_pca)
pca3d(otu_pca,group=sub_design$genotype,components = 1:3,show.ellipses = F,
      ellipse.ci = 0.95,show.plane = F,show.axe.titles = T,show.axes = T,
      biplot.vars = 5,biplot = F,show.shadows = F,show.centroids = F,
      show.group.labels = T,show.shapes = T)
# biplot显示主要差异菌，
text3d(otu_pca,texts=rownames(sub_design$genotype))

3 # PCA::Factoextra
library(FactoMineR)
library(facto)

# RDA 环境因子分析
# 首先要安装devtools包，仅需安装一次
install.packages("devtools")
# 加载devtools包
library(devtools)
# 下载ggvegan包
devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
otu.tab <- read.csv("otutab.txt", row.names = 1, header=T, sep="\t")
env.data <- read.csv("new_meta.txt", row.names = 1, fill = T, header=T, sep="\t")
#transform data
otu <- t(otu.tab)
#data normolization (Legendre and Gallagher,2001)
##by log
env.data.log <- log1p(env.data)##
##delete NA
env <- na.omit(env.data.log)
###hellinger transform
otu.hell <- decostand(otu, "hellinger")
#DCA analysis  
sel <- decorana(otu.hell)
otu.tab.0 <- rda(otu.hell ~ 1, env) #no variables
#Axis 第一项大于四应该用CCA分析
otu.tab.1<- rda(otu.hell ~ ., env)
#我们在筛选完RDA和CCA分析后，我们需要对所有环境因子进行共线性分析，利用方差膨胀因子分析
vif.cca(otu.tab.1)
#删除掉共线性的环境因子，删掉最大的变量，直到所有的变量都小于10
otu.tab.1 <- rda(otu.hell ~ N+P+K+Ca+Mg+pH+Al+Fe+Mn+Zn+Mo, env.data.log)
vif.cca(otu.tab.1)
#进一步筛选
otu.tab.1 <- rda(otu.hell ~ N+P+K+Mg+pH+Al+Fe+Mn+Zn+Mo, env.data.log)
vif.cca(otu.tab.1)
#test again
otu.tab.1 <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env.data.log)
#方差膨胀因子分析,目前所有变量都已经小于10
vif.cca(otu.tab.1)
##用step模型检测最低AIC值
mod.u <- step(otu.tab.0, scope = formula(otu.tab.1), test = "perm")# "perm"增加P值等参数
mod.d <- step(otu.tab.0, scope = (list(lower = formula(otu.tab.0), upper = formula(otu.tab.1))))
mod.d
##本处筛选的结果，找到一个Mg环境因子适合模型构建，为了下一步画图，我们
#保留所有非共线性的环境因子
#choose variables for best model and rda analysis again#
(otu.rda.f <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env))
anova(otu.rda.f)
anova(otu.rda.f, by = "term")
anova(otu.rda.f, by = "axis")
#计算db-rda
## 用ggvegan绘图
p<- autoplot(otu.rda.f, arrows = TRUE,axes = c(1, 2), geom =  c("point", "text"), layers = c( "species","sites", "biplot", "centroids"), legend.position = "right", title = "db-RDA")
## 添加图层
p + theme_bw()+theme(panel.grid=element_blank())

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
 