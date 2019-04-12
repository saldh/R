#!/usr/bin/env Rscript

# 1. 程序功能描述和主要步骤

# 程序功能：Anova组间统计和箱线图展示
# Anova Script functions: Calculate pvalue of groups by aov and TukeyHSD
# Main steps: 
# - Reads data table input.txt
# - Calculate pvalue and save in output.txt
# - Draw boxplot and save in output.pdf

# 程序使用示例
# USAGE
# Default
# anova.r     -i data_table.txt
#                        -o otuput filename prefix for output directory name 

# 参数说明
# Options
# -i/--input     输入数据表文件 input.txt
# -o/--output     输出结果文件名前缀 output_prefix, 通常会有统计表txt和矢量图pdf
options(warn = -1)

# 2. 依赖关系检查、安装和加载
# See whether these packages exist on comp. If not, install.
package_list <- c("optparse","reshape2","ggplot2","ggpubr")

for(p in package_list){
  if(!suppressWarnings(suppressMessages(require(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))){
    install.packages(p, repos="http://cran.r-project.org")
    suppressWarnings(suppressMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
  }
}
# 另两种常见R包安装方法
if (FALSE){
  # Bioconductor安装
  source("https://bioconductor.org/biocLite.R")
  biocLite(c("reshape2"))
  # Github安装
  install.packages("devtools", repo="http://cran.us.r-project.org")
  library(devtools)
  install_github("kassambara/ggpubr")
}

# 清理工作环境 clean enviroment object
rm(list=ls()) 

# 加载依赖关系 Load essential packages
library(optparse)
library(reshape2)
library(ggplot2)
library(ggpubr)

# 解析命令行
if (TRUE){
  option_list <- list(
    make_option(c("-i", "--input"), type="character", default="input.txt",
                help="Input table file to read [default %default]"),
    make_option(c("-o", "--output"), type="character", default="output",
                help="output directory or prefix [default %default]")
  )
  opts <- parse_args(OptionParser(option_list=option_list))
  
  # 显示输入输出确认是否正确
  print(paste("The input file is ", opts$input,  sep = ""))
  print(paste("The output file prefix is ", opts$output, sep = ""))
}

# 3. 读取输入文件
# 需要使用哪种方式，将其设置为TRUE，其它为FALSE即可

# 产生两组数据比较
if (TRUE){
  # 产生两种各10个正态分布数值，分别以均值为1和2，标准差为均值的0.5倍，colnames修改组名称
  set.seed(123)
  A = rnorm(10, mean = 1, sd = 0.5)
  B = rnorm(10, mean = 2, sd = 1)
  dat=as.data.frame(cbind(A,B))
  colnames(dat)=c("GroupA","GroupB")
  # 保存数据文件方便下面演示
  write.table(dat, file="opts$input", append = F, quote = F, sep="\t", eol = "\n", na = "NA", dec = ".", row.names = F, col.names = T)
}

# 从文件中读取
if (FALSE){
  dat = read.table("opts$input", header=T, row.names = NULL, sep="\t")
}

# 弹出窗口选择文件
if (FALSE){
  dat = read.table(file.choose(), header=T, row.names = NULL, sep="\t")
}

# 4. 统计与绘图
if (TRUE){
  # 将宽表格将换为长表格(变量位于同列方便操作)
  dat_melt=melt(dat, id.vars = 0)
  # anova比较各组variable的观测值value
  anova <- aov(value ~ variable, data = dat_melt)
  # 计算Tukey显著性差异检验
  Tukey_HSD <- TukeyHSD(anova, ordered = TRUE, conf.level = 0.95)
  # 提取比较结果
  Tukey_HSD_table <- as.data.frame(Tukey_HSD$variable) 
  # 展示组间差异：四列分别为均值差异，差异最小值，差异最大值和校正的P值
  Tukey_HSD_table
  # 采用ggpubr包中的ggboxplot展示组间比较箱线图，并添加Wilcoxon整体差异P值
  # 增加了jitter点，color和shape按分组variable变化，添加统计值 
  p <- ggboxplot(dat_melt, x="variable", y="value", color = "variable", 
                 add = "jitter", shape="variable") +stat_compare_means()
  p
}

# 5. 保存图表
if (TRUE){
  # 保存一个制表符，解决存在行名时，列名无法对齐的问题
  write.table("\t", file=paste(opts$output,".txt",sep=""),append = F, quote = F, eol = "", row.names = F, col.names = F)
  # 保存统计结果，有waring正常
  write.table(Tukey_HSD_table, file=paste(opts$output,".txt",sep=""), append = T, quote = F, sep="\t", eol = "\n", na = "NA", dec = ".", row.names = T, col.names = T)
  print(paste("The output table is ", opts$output, ".txt",  sep = ""))
  # 保存图片至文件，pdf方便AI修改成出版级图片
  ggsave(file=paste(opts$output,".pdf",sep=""), p, width = 5, height = 3)
  print(paste("The output figure is ", opts$output, ".pdf",  sep = ""))
}