# 环境因子相关性分析
library(pheatmap)
library(psych)
library(stringr)
# 读取数据，na.strings=“”设置缺失值的字符
species_table <- read.table("D://data/微生太/环境因子相关性分析/otu_table.Genus.relative.txt",
                            sep = "\t",header = T,check.names=F,stringsAsFactors=F,comment.char='')
map <- read.table("D://data/微生太/环境因子相关性分析/mapping_file.txt",sep="\t",na.strings="",header = T,
                  row.names=1,comment.char = "",check.names = F,stringsAsFactors = F)
# 提取要分析的环境因子
map <-map[,10:14]
colname_of_map<-colnames(map)
# 区分重复的属名（由于GG数据库的关系，某些不同科分类下的属，会出现同名不同类的情况）
species_table$Taxonomy[duplicated(species_table$Taxonomy)] <-paste(species_table$Taxonomy[duplicated(species_table$Taxonomy)], "_1",sep = "")
# 剔除最后一行（无效分类）
species_table<-species_table[-nrow(species_table),]
# 将行名设置为属分类名
rownames(species_table)<-species_table$Taxonomy
# 去除表格里不需要的注释信息
species_table<-species_table[,-c(1,ncol(species_table))]
# 合并数据，转置
species_table<-t(species_table)
# 重排species_table行的顺序，使其和map行名一致
sum_of_species<-colSums(species_table)
species_table<-species_table[match(rownames(map),rownames(species_table)),]
# 合并表格
merged_table<-data.frame(map,species_table,check.names = F,check.rows = T)
# 计算spearman秩相关
correlation_results<-corr.test(merged_table,method ="spearman",adjust="fdr")
# 计算pearson相关
# correlation_results<-corr.test(merged_table,method ="pearson",adjust="fdr")
# method ="spearman"指明用秩相关的方法
# adjust="fdr"校正错误发现率
# 提取相关矩阵和p值矩阵
r<-correlation_results$r
p<-correlation_results$p
# 剔除不关心的相关系数（环境因子之间，微生物之间）
r<-r[-c(1:5),-c(6:192)]
p<-p[-c(1:5),-c(6:192)]
# 选择相关系数显著次数最多的20个物种
selected_position_of_species<-head(order(colSums(t(p)<0.05),decreasing =T),20)
r<-r[selected_position_of_species,]
# 得到筛选后的相关系数，p值矩阵
p<-p[selected_position_of_species,]
# 自定义显著性标记函数
sig_label<-function(x){ifelse(x<0.001,"***",ifelse(x<0.01,"**",ifelse(x<0.05,"*","")))}
# 得到显著性标记矩阵
sig_matrix<-sig_label(p)
# 绘制相关性热图
pheatmap(r,fontsize=15,border_color = "black",
         display_numbers = sig_matrix,fontsize_row =15,fontsize_col = 15,
         fontsize_number = 22,
         #显著性标记的符号大小
         cluster_rows=T,clustering_distance_rows="correlation",
         #指明行聚类，聚类依据的距离
         cluster_cols=T,clustering_distance_cols="euclidean",
         #指明列聚类，聚类依据的距离
         clustering_method="centroid")
#聚类方法
library(purrr)
library(corrplot)
library(pheatmap)
library(ggcorrplot)
design <- read.table('mappingfile.txt',header = T,row.names = 1,sep = '\t',comment.char = '')
otu_table <- read.delim('otu_table4.txt',row.names = 1,header = T,sep = '\t')
# 过滤数据
idx <- rownames(design) %in% colnames(otu_table)
sub_design <- design[idx,]
# 标准化为百分比
count <- otu_table[,rownames(sub_design)]
norm <- t(t(count)/colSums(count,na=T))*100
# 按组合并
sampfile <- as.data.frame(sub_design$genotype,row.names = row.names(sub_design))
colnames(sampfile)[1] <- 'group'
mat <- norm
mat_t <- t(mat)
mat_t2 <- merge(sampfile,mat_t,by='row.names')
mat_t2 <- mat_t2[,-1]
# 按组求平均值
mat_mean <- aggregate(mat_t2[,-1],by=mat_t2[1],FUN =mean)
mat_mean_final <- do.call(rbind,mat_mean)[-1,]
geno <- mat_mean$group
colnames(mat_mean_final) <- geno
# 计算相关系数,保留三位小数
sim <- cor(mat_mean_final,method = 'pearson')
sim <- round(sim,3)
ggcorrplot(sim,type = 'upper',colors = c('green','yellow','red'))
sim_otutable <- cor(otu_table,method = 'pearson')
sim_otutable <- round(sim,3)
corrplot(sim_otutable,method = 'circle',order='hclust',addrect = 3 )
