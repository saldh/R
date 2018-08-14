install.packages('ggplot2')
install.packages("vcd")
install.packages('ggthemes')
install.packages('Cairo')
install.packages('clusterProfiler')
install.packages('VennDiagram')
install.packages('pheatmap')
install.packages('reshape')
install.packages('devtools')
install.packages('bioconductor')
library(gclus)
help(package="vcd")
q()
#安装bioconductor的包
# install.packages('biocLite')
source("https://bioconductor.org/biocLite.R")
# biocLite()
biocLite('metagenomeSeq')
biocLite('BiocInstaller')
biocLite(c("RUVSeq","pcaMethods"))
biocLite(c("GenomicFeatures", "AnnotationDbi"))
library(BiocInstaller)
# 安装Github的R包
install.packages("devtools")
devtools::install_github("JustinaZ/pcaReduce")
# 手动安装, 首先下载包的源文件（压缩版就可），然后在终端运行下面的命令。
ct@ehbio:~$ R CMD INSTALL package.tar.gz
# 移除包
remove.packages("package_name")
# 查看所有安装的包
library()
# 查看特定安装包的版本
installed.packages()[c("DESeq2"), c("Package", "Version")]
Package  Version 
"DESeq2" "1.14.1" 
# 查看默认安装包的位置
.libPaths()
# 调用安装的包
library(package_name)
install.packages('ggbeeswarm')
#devtools::install_github("hms-dbmi/scde", build_vignettes = FALSE)
#install.packages(c("mvoutlier","ROCR"))
#biocLite(c("RUVSeq","pcaMethods","SC3","TSCAN","monocle","MultiAssayExperiment","SummarizedExperiment"))
#devtools::install_github("satijalab/seurat")