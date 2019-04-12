setwd("D:/data/qiime-data/q1-PE250/tax4fun")
install.packages('qiimer','biom')
install.packages('D://data/R/biom_0.3.12.tar.gz',
                 repos = NULL,type='source')
library('devtools')
library(biom)
install.packages('D://data/R/Tax4Fun_0.3.1.tar.gz',
                 repos = NULL,type='source')
library(qiimer)
library(Matrix)
library(Tax4Fun)
QIIMESingleData <- importQIIMEData('D://data/R/HMP_0.97_table.txt')
otu_table <-QIIMESingleData$otuTable
colSums(otu_table)
write.table('ID\t',file='otu_table_tax.txt',append = F,
            quote = F,sep = '\t',eol = ' ',na='NA',dec = '.',
            row.names = F,col.names = F)
write.table(otu_table,file='otu_table_tax.txt',append = T,
            quote = F,sep = '\t',eol = '\n',na='NA',
            dec = '.',row.names = T,col.names = T)
# 根据Tax4Fun提供的SILVA123最新数据库进行预测，要求此数据的压缩包拉于此工作目录，这个命令得出来的是KO号的各种酶的基因丰度
taxfunoutput <- Tax4Fun(QIIMESingleData,'SILVA123',fctProfiling = T,
                        refProfile = 'UProC',shortReadMode = T,
                        normCopyNo = T)
# 提取KO表，生成6508个KO相关的通路
KO_table = t(Tax4FunOutput$Tax4FunProfile)

# 所有样品标准化为1, 没有原始数据，可以用anova和Limma，无法使用edgeR和DESeq2
colSums(KO_table)

# 输出KO表，表头写个制表符，用于对齐表头
write.table("ID\t", file="KO_table.txt",append = FALSE, quote = FALSE, sep="\t",eol = "", na = "NA", dec = ".", row.names = F,col.names = F)
write.table(KO_table, file="KO_table.txt",append = T, quote = FALSE, sep="\t",eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE)

## 使用greengene注释的结果
QIIMESingleData <- importQIIMEData("../result/otu_table_tax.txt")
# 转换格式为tax4fun要求,把otuTable提取出来命名为otudf
otudf<-QIIMESingleData$otuTable 
# 去掉名字中的特殊符号
rownames(otudf)<-gsub("[a-z]__","",rownames(otudf)) 
# 重新把修改好后的otudf 命名为QIIMESingleData 中的 otuTable
QIIMESingleData$otuTable <- otudf 
Tax4FunOutput <- Tax4Fun(QIIMESingleData, "SILVA123", fctProfiling = FALSE, 
                         refProfile = "UProC", shortReadMode = TRUE, 
                         normCopyNo = TRUE) # 预测代谢物
# 根据SILVA123最新数据库进行预测，这个命令得出来的是KO号的各种酶的基因丰度
Tax4FunOutput2 <- Tax4Fun(QIIMESingleData, "SILVA123", fctProfiling = TRUE, 
                         refProfile = "UProC", shortReadMode = TRUE, 
                         normCopyNo = TRUE) 

