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
#��װbioconductor�İ�
# install.packages('biocLite')
source("https://bioconductor.org/biocLite.R")
# biocLite()
biocLite('metagenomeSeq')
biocLite('BiocInstaller')
biocLite(c("RUVSeq","pcaMethods"))
biocLite(c("GenomicFeatures", "AnnotationDbi"))
library(BiocInstaller)
# ��װGithub��R��
install.packages("devtools")
devtools::install_github("JustinaZ/pcaReduce")
# �ֶ���װ, �������ذ���Դ�ļ���ѹ����Ϳɣ���Ȼ�����ն�������������
ct@ehbio:~$ R CMD INSTALL package.tar.gz
# �Ƴ���
remove.packages("package_name")
# �鿴���а�װ�İ�
library()
# �鿴�ض���װ���İ汾
installed.packages()[c("DESeq2"), c("Package", "Version")]
Package  Version 
"DESeq2" "1.14.1" 
# �鿴Ĭ�ϰ�װ����λ��
.libPaths()
# ���ð�װ�İ�
library(package_name)
install.packages('ggbeeswarm')
#devtools::install_github("hms-dbmi/scde", build_vignettes = FALSE)
#install.packages(c("mvoutlier","ROCR"))
#biocLite(c("RUVSeq","pcaMethods","SC3","TSCAN","monocle","MultiAssayExperiment","SummarizedExperiment"))
#devtools::install_github("satijalab/seurat")