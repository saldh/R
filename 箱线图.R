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

profile_text <- read.table(text=profile, header=T, row.names=1, quote="",sep=";", check.names=F)
# 在melt时保留位置信息
# melt格式是ggplot2画图最喜欢的格式
# 好好体会下这个格式，虽然多占用了不少空间，但是确实很方便

library(ggplot2)
library(reshape2)
data_m <- melt(profile_text)
head(data_m)
# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
# 图会存储在当前目录的Rplots.pdf文件中，如果用Rstudio，可以不运行dev.off()
dev.off()
# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot(aes(fill=factor(variable))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
# 图会存储在当前目录的Rplots.pdf文件中，如果用Rstudio，可以不运行dev.off()
dev.off()
# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_violin(aes(fill=factor(variable))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
# 图会存储在当前目录的Rplots.pdf文件中，如果用Rstudio，可以不运行dev.off()
dev.off()

library(ggbeeswarm)
# 为了更好的效果，只保留其中一个样品的数据
# grepl类似于Linux的grep命令，获取特定模式的字符串
data_m2 <- data_m[grepl("_3", data_m$variable),]
# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p <- ggplot(data_m2, aes(x=variable, y=value),color=variable) + 
  geom_quasirandom(aes(colour=factor(variable))) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.key=element_blank()) +
  theme(legend.position="none")
# 也可以用geom_jitter(aes(colour=factor(variable)))代替geom_quasirandom(aes(colour=factor(variable)))
# 但个人认为geom_quasirandom给出的结果更有特色
ggsave(p, filename="jitterplot.pdf", width=14, height=8, units=c("cm"))
p
#绘制单个基因 (A)的箱线图
profile="Name;2cell_1;2cell_2;2cell_3;2cell_4;2cell_5;2cell_6;4cell_1;4cell_2;4cell_3;4cell_4;4cell_5;4cell_6;zygote_1;zygote_2;zygote_3;zygote_4;zygote_5;zygote_6
A;4;6;7;5;8;6;3.2;5.2;5.6;3.6;7.6;4.8;2;4;3;2;4;2.5
B;6;8;9;7;10;8;5.2;7.2;7.6;5.6;9.6;6.8;4;6;5;4;6;4.5"
profile_text <- read.table(text=profile, header=T, row.names=1, quote="",sep=";", check.names=F)
data_m = data.frame(t(profile_text['A',]))
data_m$sample = rownames(data_m)
# 只挑选显示部分
# grepl前面已经讲过用于匹配
data_m[grepl('_[123]', data_m$sample),]
#获得样品分组信息 (这个例子比较特殊，样品的分组信息就是样品名字下划线前面的部分)
# 可以利用strsplit分割，取出其前面的字符串
# R中复杂的输出结果多数以列表的形式体现，在之前的矩阵操作教程中
# 提到过用str函数来查看复杂结果的结构，并从中获取信息
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

# 会获得相同的结果，脚本注释掉了以免重复执行引起问题。

矩阵准备好了，开始画图了 (小提琴图做例子，其它类似)
# 调整下样品出现的顺序
data_m$group <- factor(data_m$group, levels=c("zygote","2cell","4cell"))
# group和A为矩阵中两列的名字，group代表了值的属性，A代表基因A对应的表达值。
# 注意看修改了的地方
p <- ggplot(data_m, aes(x=group, y=A),color=group) + 
  geom_violin(aes(fill=factor(group))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
# 图会存储在当前目录的Rplots.pdf文件中，如果用Rstudio，可以不运行dev.off()

#长矩阵绘制箱线图
#常规矩阵绘制箱线图要求必须是个方正的矩阵输入，而有时想比较的几个组里面检测的值数目不同。比如有三个组，GrpA组检测了6个病人，GrpB组检测了10个病人，GrpC组是12个正常人的检测数据。这时就很难形成一个行位检测值，列为样品的矩阵，长表格模式就适合与这种情况。
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
#长表格形式自身就是常规矩阵melt后的格式，这种用来绘制箱线图就很简单了，就不做解释了。
