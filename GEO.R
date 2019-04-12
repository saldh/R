# GEO数据挖掘
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GEOquery", version = "3.8")
library(GEOquery)
gset <- getGEO('GDS858',destdir='.')
# 下载数据：表达矩阵、
gset <- getGEO('GSE42872',destdir = '.',AnnotGPL = T,getGPL = T)
a <- read.table('GSE42872_series_matrix.txt.gz',sep = '\t',quote = '',
                fill = T,comment.char = '!',header = T)

# 得到表达矩阵 affy芯片
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("affy", version = "3.8")
library(affy)
