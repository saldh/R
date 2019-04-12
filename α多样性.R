
# 安装和载入相关包和数据文件
#source("https://bioconductor.org/biocLite.R")
#biocLite(c("ggplot2","reshape2")) # 没安装过此类包的请手动运行这两行代码安装包
library("ggplot2") # load related packages
library("reshape2")
design = read.table("design.txt", header=T, row.names= 1, sep="\t") 
rare = read.table("observed_otus.csv", header=T, row.names= 1, sep=",") 

# 提取样品组信息
sampFile = as.data.frame(design$BMI,row.names = row.names(design))
sampFile
colnames(sampFile)[1] = "group"

# 转换宽表格为ggplot通用长表格格式
rare$x = rownames(rare) # 添加x轴列
rare_melt = melt(rare, id.vars=c("x")) # 转换为长表格

rare_melt$x = factor(rare_melt$x, levels=1:200) # 设置x轴顺序

# 添加分组信息
rare_melt3 = merge(sampFile,rare_melt, by.x="row.names", by.y="variable")
rare_melt3$variable=rare_melt3$Row.names

# 按样品分组，按组上色
p = ggplot(rare_melt3, aes(x = x, y = value, group = variable, color = group )) + 
  geom_line()+
  xlab("Rarefraction Percentage")+ylab("Richness (Observed OTUs)")+
  scale_x_discrete(breaks = c(1:10)*10, labels = c(1:10)*10) + theme_classic()
p
ggsave(paste("alpha_rare_samples.pdf", sep=""), p, width = 8, height = 5)


source("https://bioconductor.org/biocLite.R")
biocLite("clusterProfiler")
library("clusterProfiler")
