# 数据导入
read.table()
# file 指定读入的文件
# header 是否有列名（默认无）
# seq 指定分隔符(空格、TAB、换行符、回车符)
# quote 制定包围字符型数据的字符。默认情况下，字符串可以被 " 或 ’ 括起，并且两种情况下，引号内部的字符都作为字符串的一部分。有效的引用字符（可能没有）的设置由参数 quote 控制。默认值改为 quote = “”
# dec = “.” 指定小数点数
# colClasses 指定列的数据类型格式
# row.names 指定各行名称，也可以是数字，指定某列为行名
# col.names
# as.is = !stringsAsFactors as.is 字符向量是否转换成因子（仅仅这个功能），TRUE时保留为字符型
# na.strings = “NA” 指定什么样的字符表示值缺少
# colClasses = NA colClasses运行为输入中的每个列设置需要的类型。注意，colClasses 和 as.is 对每 列专用，而不是每个变量。因此，它对行标签列也同样适用（如果有的话）。
# nrows = -1 最大读入行数，即读入前多少行，“-1”表示都读入
# skip = 0 跳过文件的前n行（skip = n）
# check.names = TRUE # 检查变量名在R中是否有效
# fill = !blank.lines.skip 从一个电子表格中导出的文件通常会把拖尾的空字段（包括?堑姆指舴? 忽略掉。为了读取这样的文件，必须设置参数 fill = TRUE
# strip.white = FALSE 如果设定了分隔符，字符字段起始和收尾处的空白会作为字段一部分看待的。为了去掉这些空白，可以使用参数 strip.white = TRUE
# blank.lines.skip = TRUE 默认情况下，read.table 忽略空白行。这可以通过设置 blank.lines.skip = FALSE 来改变。但这个参数只有在和 fill = TRUE 共同使用时才有效。这时，可能是用空白行表明规则数据中的缺损样本。
# comment.char = “#” 默认情况下，read.table 用 # 作为注释标识字符。如果碰到该字符（除了在被引用的字符串内），该行中随后的内容将会被忽略。只含有空白和注释的行被当作空白行。如果确认数据文件中没有注释内容，用 comment.char = “” 会比较安全 。
# allowEscapes = FALSEread.table 和 scan 都有一个逻辑参数 allowEscapes。该参数默认为否，而且反斜杠是唯一被解释为逃逸引用符的字符（在前面描述的环境中）。如果该参数设为是，以C形式的逃逸规则解释，也就是控制符如 , , , , , , 八进制和十六进制如 40 和 x2A 一样描述。任何其它逃逸字符都看着是自己，包括反斜杠

# 数据处理
library(tidyverse)
library(nycflights13)
library(ggplot2)
setwd('~/data/qiime/q1-250b ')
design <- read.table('mappingfile.txt',row.names = 1,header = T,comment.char = '')
otutable <- read.table('otu_table.txt',header = T,row.names = 1)
# 排序
sub_otu <- mutate(otutable,rownames(otutable))
sub_otu <- mutate(sub_otu,rowSums(sub_otu[,1:25]))
sub_otu <- sub_otu[order(sub_otu$`rowSums(sub_otu[, 1:25])`,decreasing = T),]
rownames(sub_otu) <- sub_otu$`rownames(otutable)`
# 过滤数据 filter选择行
filter(design,genotype %in% c('KO' ,'WT'))
filter(design,genotype=='KO'| genotype=='WT')
select(design,genotype)
# 排序
arrange(design,genotype)
arrange(design,desc(genotype))
# select选择列
select(design,genotype)
select(design,genotype:species)
select(design,starts_with('gen'))
select(design,ends_with('pe'))
select(design,contains('eno'))
# matches('(.)\\1)匹配正则表达式的变量
# num_range('x',1:3)
# 添加新列
otutable <- as.data.frame(t(otutable))
sub_design <- design[rownames(design) %in% colnames(otutable),]
mutate(otutable,sub_design$genotype)
group <- select(sub_design,genotype)
cbind(otutable,group)
data <- mutate(otutable,rowSums(otutable))

# 分组摘要
design %>% 
  group_by(genotype,Description,species) %>%
  summarize(mean = mean(batch))

delays <- flights %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay,na.rm=T),n=n())
ggplot(data=delays,mapping = aes(x=n,y=delay))+
  geom_point(alpha=1/10)

# 去掉观测数量较少的分组
delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x=n,y=delay))+
  geom_point()


# 字符串
