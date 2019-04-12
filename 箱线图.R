profile="Name;2cell_1;2cell_2;2cell_3;4cell_1;4cell_2;4cell_3;zygote_1;zygote_2;zygote_3
A;4;6;7;3.2;5.2;5.6;2;4;3
B;6;8;9;5.2;7.2;7.6;4;6;5
C;8;10;11;7.2;9.2;9.6;6;8;7
D;10;12;13;9.2;11.2;11.6;8;10;9
E;12;14;15;11.2;13.2;13.6;10;12;11
F;14;16;17;13.2;15.2;15.6;12;14;13
G;15;17;18;14.2;16.2;16.6;13;15;14
H;16;18;19;15.2;17.2;17.6;14;16;15
I;17;19;20;16.2;18.2;18.6;15;17;16
J;18;20;21;17.2;19.2;19.6;16;18;17
L;19;21;22;18.2;20.2;20.6;17;19;18
M;20;22;23;19.2;21.2;21.6;18;20;19
N;21;23;24;20.2;22.2;22.6;19;21;20
O;22;24;25;21.2;23.2;23.6;20;22;21"

profile_text <- read.table(text=profile, header=T,  quote="",sep=";", check.names=F)
rownames(profile_text[1,1]) <- 'id'

library(ggplot2)
library(reshape2)
data_m <- melt(profile_text,id.vars = 'Name')
??reshape2
head(data_m)
# 
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")

dev.off()
# 箱线图
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot(aes(fill=factor(variable))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p

dev.off()
# 小提琴图
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_violinboxplotill=factor(variable))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
dev.off()

library(ggbeeswarm)
data_m2 <- data_m[grepl("_3", data_m$variable),]
p <- ggplot(data_m2, aes(x=variable, y=value),color=variable) + 
  geom_quasirandom(aes(colour=factor(variable))) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.key=element_blank()) +
  theme(legend.position="none")
ggsave(p, filename="jitterplot.pdf", width=14, height=8, units=c("cm"))
p

profile="Name;2cell_1;2cell_2;2cell_3;2cell_4;2cell_5;2cell_6;4cell_1;4cell_2;4cell_3;4cell_4;4cell_5;4cell_6;zygote_1;zygote_2;zygote_3;zygote_4;zygote_5;zygote_6
A;4;6;7;5;8;6;3.2;5.2;5.6;3.6;7.6;4.8;2;4;3;2;4;2.5
B;6;8;9;7;10;8;5.2;7.2;7.6;5.6;9.6;6.8;4;6;5;4;6;4.5"
profile_text <- read.table(text=profile, header=T, row.names=1, quote="",sep=";", check.names=F)
data_m = data.frame(t(profile_text['A',]))
data_m$sample = rownames(data_m)

data_m[grepl('_[123]', data_m$sample),]

group = unlist(lapply(strsplit(data_m$sample,"_"), function(x) x[1]))
data_m$group = group
data_m[grepl('_[123]', data_m$sample),]
#
sampleGroup_text="Sample;Group
zygote_1;zygote
zygote_2;zygote
zygote_3;zygote
zygote_4;zygote
zygote_5;zygote
zygote_6;zygote
2cell_1;2cell
2cell_2;2cell
2cell_3;2cell
2cell_4;2cell
2cell_5;2cell
2cell_6;2cell
4cell_1;4cell
4cell_2;4cell
4cell_3;4cell
4cell_4;4cell
4cell_5;4cell
4cell_6;4cell"

#sampleGroup = read.table(text=sampleGroup_text,sep="\t",header=1,check.names=F,row.names=1)

#data_m <- merge(data_m, sampleGroup, by="row.names")

data_m$group <- factor(data_m$group, levels=c("zygote","2cell","4cell"))

p <- ggplot(data_m, aes(x=group, y=A),color=group) + 
  geom_violin(aes(fill=factor(group))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
long_table <- "Grp;Value
GrpA;10
GrpA;11
GrpA;12
GrpB;5
GrpB;4
GrpB;3
GrpB;2
GrpC;2
GrpC;3"

long_table <- read.table(text=long_table,sep="\t",header=1,check.names=F)

p <- ggplot(data_m, aes(x=Grp, y=Value),color=Grp) + 
geom_violin(aes(fill=factor(Grp))) + 
theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
theme(legend.position="none")
p
dev.off()

# α多样性
library(ggplot2)
library(magrittr)
library(ggpubr)
alpha_data <- read.table('alpha.txt',header = T,row.names = 1,sep = '\t') 
design <- read.table('mappingfile.txt',header = T,row.names = 1,sep = '\t',
                     comment.char = '',check.names = F)
data <- cbind(alpha_data,design[match(rownames(alpha_data),rownames(design)),])
group <- design['Group1']
head(data)
p <- ggplot(data,aes(genotype,shannon,color=genotype))+
  geom_boxplot(alpha=1,outlier.size = 0,size=0.7,width=0.5,fill='transparent')+
  geom_jitter(position = position_jitter(0.2),size=1,alpha=0.7)+
  labs(x='Groups',y='shannon')
mycompare <- list(c('A','B'),c('A','C'),c('B','C'))
p <- p + stat_compare_means(comparisons = mycompare,label='p.signif',method='wilcox')

anova <- aov(shannon~data$genotype,data = data) 


# α多样性
library("ggplot2") # load related packages

# 读入实验设计和Alpha多样性值
design <- read.table('mappingfile.txt',header = T,row.names = 1,sep = '\t',
                     comment.char = '',check.names = F)
alpha = read.table("alpha.txt", header=T, row.names= 1, sep="\t")
# 以Observed OTU为例进行可视化和统计分析，其它指数将observed_otus替换为shannon, chao1, PD_whole_tree即可计算
# 合并Alpha指数与实验设计
index = cbind(alpha, design[match(rownames(alpha), rownames(design)), ]) 
# 绘图代码、预览、保存PDF
p = ggplot(index, aes(x=genotype, y=observed_otus, color=genotype))+
  geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +  
  geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+
  labs(x="Groups", y="observed_otus index")
p
ggsave(paste("alpha_observed_otus.pdf", sep=""), p, width = 5, height = 3)

# 统计组间是否显著差异
# 指定多重比较的分组对 
# 添加星号显著性标记 ,wilcoxon非参数检验 
p <- p+stat_compare_means(comparisons=data$genotype,label = "p.signif",
                          method = 'wilcox') 
# 添加显著性标记参数检验，要求α多样指服从正态分布 
#anova对指数与分组统计 
anova <- aov(shannon~Group1,data = data) plotdata<-duncan.test(anova,"Group1",console = TRUE, alpha = 0.05) plotdata<-data.frame(id=rownames(plotdata$groups),plotdata$groups) p<-p+geom_text(data = plotdata,aes(x=id,y=7,label=groups)) p #
# anova对指数与分组统计
library(agricolae)
observed_otus_stats <- aov(observed_otus ~ genotype, data = index)
plotdata <- duncan.test(observed_otus_stats,'genotype',console=T,alpha=0.5)
plotdata <- data.frame(id=rownames(plotdata$groups),plotdata$groups)
p <- p + geom_text(plotdata,aes(x=id,y=7,label=groups))
# 使用TukeyHSD对组间进行检验，效正pvalue
Tukey_HSD_observed_otus <- TukeyHSD(observed_otus_stats, ordered = FALSE, conf.level = 0.95)
# 结果中提取需要的结果
plotdata <- data.frame(id=rownames(Tukey_HSD_observed_otus$genotype),Tukey_HSD_observed_otus$genotype)
Tukey_HSD_observed_otus_table <- as.data.frame(Tukey_HSD_observed_otus$genotype)
# 预览结果
Tukey_HSD_observed_otus_table
# 保存结果到文件，按Pvaule值由小到大排序
write.table(Tukey_HSD_observed_otus_table[order(Tukey_HSD_observed_otus_table$p, decreasing=FALSE), ], 
            file="alpha_observed_otus_stats.txt",append = FALSE, 
            quote = FALSE, sep="\t",eol = "\n", na = "NA", dec = ".", 
            row.names = TRUE,col.names = TRUE)