getwd()
volcanoData <- read.table('volcano.txt', header=T, quote="", check.names=F)
head(volcanoData)
library(ggplot2)
p <- ggplot(volcanoData, aes(x=log2FoldChange, y=padj))
p <- p + geom_point()
# 前面是给p不断添加图层的过程
# 单输入一个p是真正作图
# 前面有人说，上面都输完了，怎么没出图
# 就因为差了一个p
p
#对数据坐下预处理，差异大的基因padj小，先对其求取负对数，所谓负负得正，差异大的基因就会处于图的上方了。
# 从示例数据中看到，最小的padj值为0，求取负对数为正无穷。
# 实际上padj值小到一个点对我们来讲就是个数
# 所以可以给所有小于1e-6的padj都让其等于1e-6，再小也没意义
volcanoData[volcanoData$padj<1e-6, "padj"] <- 1e-6
volcanoData$padj <- (-1)* log10(volcanoData$padj)
#数据中基因的上调倍数远高于下调倍数，使得出来的图是偏的，这次画图时调整下X轴的区间使图对称。
P <- ggplot(volcanoData , aes(x=log2FoldChange , y= padj)) +
  geom_point() +
  xlim(-4.7,4.7)
p
#加颜色
p <- ggplot(volcanoData, aes(x=log2FoldChange, y=padj)) +
  geom_point(color=significant) +
  xlim(-4.7,4.7)
p
##
fastqc<-"ID;GC_quality;Base_quality
ehbio_1_1;PASS;PASS
ehbio_1_2;PASS;PASS
ehbio_2_1;WARN;PASS
ehbio_2_2;WARN;PASS
Other_1_1;FAIL;FAIL
Other_1_2;FAIL;FAIL"
fastqc_data <- read.table(text=fastqc, sep=";", header=T)
# 就不查看了
p <- ggplot(fastqc_data, aes(x=GC_quality, y=Base_quality)) + geom_point()
p
#六个点少了只剩下了3个，重叠在一起了，而且也不知道哪个点代表什么样品。这时需要把点抖动下，用到一个包ggbeeswarm，抖动图的神器。
library(ggbeeswarm)
p <- ggplot(fastqc_data, aes(x=GC_quality, y=Base_quality)) + geom_quasirandom()
# 使用geom_text增加点的标记
# label表示标记哪一列的数值
# position_quasirandom获取点偏移后的位置
# xjust调整对齐方式; hjust是水平的对齐方式，0为左，1为右，0.5居中，0-1之间可以取任意值。vjust是垂直对齐方式，0底对齐，1为顶对齐，0.5居中，0-1之间可以取任意值。
# check_overlap检查名字在图上是否重叠
p <- p + geom_text(aes(label=ID), position=position_quasirandom(),hjust=0, check_overlap=T)
p

##假如有一个输入数据如下所示(存储于文件scatterplot.xls中)

Samp    Gene1    Gene2    Color    Size    GC_quality    Base_quality
a    1    1    grp1    10    PASS    PASS
b    2    2    grp1    10    PASS    PASS
c    1    3    grp1    10    WARN    PASS
d    3    1    grp2    15    WARN    WARN
e    2    2    grp2    15    PASS    WARN
f    3    3    grp3    5    PASS    PASS
g    2    1    grp3    5    WARN    PASS

# -f: 指定输入文件，列数不限，顺序不限; 第一行为列名字，第一列无特殊要求，必选
# -X: 指定哪一列为X轴信息，必选
# -Y: 指定哪一列为Y轴信息，必选
# -c: 指定用哪一列标记颜色，可选
# -s: 指定哪一列标记大小，一般为数字列，可选
# -S: 指定哪一列标记形状，可选
# -L: 指定哪一列用来作为文本标记
# -w, -u: 指定图的长宽
sp_scatterplot2.sh -f scatterplot.xls -X Gene1 -Y Gene2 -c Color -s Size -S GC_quality -L Samp -w 10 -u 10
#
#如果横纵轴为字符串，且有重复, 则需指定参数-J TRUE以错开重叠的点，具体如下
# -O: 指定X轴变量的顺序, 默认是字母顺序
# 其它列或其它属性的顺序也可以用相应的方式指示，具体看程序的帮助提示
# -c Gene1: 用特定基因的表达对点着色，单细胞分析图中常用
# -J TRUE: 见上
# -Z FALSE：默认使用geom_text_repel添加点的标记，及其智能，不会出现标签过多覆盖的情况
# 但对jitterplot，会有些冲突，所以在`-J TRUE`且出来的图中点的标签不符合预期时，设定
# 次参数为FALSE，使用geom_text标记点。

sp_scatterplot2.sh -f scatterplot.xls -X GC_quality -Y Base_quality -O "'WARN', 'PASS'" -c Gene1 -w 10 -u 10 -J TRUE -L Samp -Z FALSE


