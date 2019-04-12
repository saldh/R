#生成测试数据
data <- c(1:6,6:1,6:1,1:6,(6:1)/10,(1:6)/10,(1:6)/10,(6:1)/10,1:6,6:1,6:1,1:6, 6:1,1:6,1:6,6:1)
# ncol: 指定列数
# byrow: 先按行填充数据
# ?matrix 可查看函数的使用方法
# as.data.frame的as系列是转换用的
data <- as.data.frame(matrix(data, ncol=12, byrow=T))
#增加列的名字
colnames(data) <- c("Zygote","2_cell","4_cell","8_cell","Morula","ICM","ESC","4 week PGC","7 week PGC","10 week PGC","17 week PGC", "OOcyte")
#增加行的名字
rownames(data) <- paste("Gene", 1:8, sep="_")
#只显示前6行和前4列
head(data)[,1:4]
#转换数据
library(reshape2)
library(ggplot2)
#转换前先增加一列id列，保存行名字
data$ID <- rownames(data)工作原理是把全部的非id列的数值列转为1列，命名为value；所有字符列转为variable列。
# melt：把正常矩阵转换成长表格模式的函数，
# id.vars 列用于指定哪些列为id列；这些列不会被merge，会保留为完整一列。
data_m <- melt(data, id.vars=c("ID"))
head(data_m)
#分解绘图
# data_m: 是前面费了九牛二虎之力得到的数据表
# aes: aesthetic的缩写，一般指定整体的X轴、Y轴、颜色、形状、大小等。
# 在最开始读入数据时，一般只指定x和y，其它后续指定
p <- ggplot(data_m, aes(x=variable,y=ID)) 
# 热图就是一堆方块根据其值赋予不同的颜色，所以这里使用fill=value, 用数值做填充色。
p <- p + geom_tile(aes(fill=value)) 
# ggplot2为图层绘制，一层层添加，存储在p中，在输出p的内容时才会出图。
p
# theme: 是处理图美观的一个函数，可以调整横纵轴label的选择、图例的位置等。
# 这里选择X轴标签45度。
# hjust和vjust调整标签的相对位置，具体见 <https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot>。
# 简单说，hjust是水平的对齐方式，0为左，1为右，0.5居中，0-1之间可以取任意值。vjust是垂直对齐方式，0底对齐，1为顶对齐，0.5居中，0-1之间可以取任意值。
p <- p + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1))
p
## 如果你没有使用Rstudio或其它R图形版工具，而是在远程登录的服务器上运行的交互式R，需要输入下面的语句，获得输出图形 （图形存储于R的工作目录下的Rplots.pdf文件中）。

## 如何指定输出，后面会讲到。
#dev.off()
# theme: 是处理图美观的一个函数，可以调整横纵轴label的选择、图例的位置等。
# 这里选择X轴标签45度。
# hjust和vjust调整标签的相对位置，具体见 <https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot>。
# 简单说，hjust是水平的对齐方式，0为左，1为右，0.5居中，0-1之间可以取任意值。vjust是垂直对齐方式，0底对齐，1为顶对齐，0.5居中，0-1之间可以取任意值。
p <- p + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1))
p
# 连续的数字，指定最小数值代表的颜色和最大数值赋予的颜色
# 注意fill和color的区别，fill是填充，color只针对边缘
p <- p + scale_fill_gradient(low = "white", high = "red")
p
# postion可以接受的值有 top, bottom, left, right, 和一个坐标 c(0.05,0.8) (左上角，坐标是相对于图的左下角计算的）
p <- p + theme(legend.position="top")
p <- p + xlab("samples") + theme_bw() + theme(panel.grid.major = element_blank()) + theme(legend.key=element_blank())
p
#合并命令
p <- ggplot(data_m, aes(x=variable,y=ID)) + xlab("samples") + theme_bw() + theme(panel.grid.major = element_blank()) + theme(legend.key=element_blank())  + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1)) + theme(legend.position="top") +  geom_tile(aes(fill=value)) + scale_fill_gradient(low = "white", high = "red")
p
#图形储存
# 可以跟输出文件不同的后缀，以获得不同的输出格式
# colormode支持srgb （屏幕）和cmyk （打印，部分杂志需要，看上去有点褪色的感觉）格式
ggsave(p, filename="heatmap.pdf", width=10,
       height=15, units=c("cm"),colormodel="srgb")

heatmap.2 (x,
           # dendrogram control
           Rowv = TRUE,                                 # 确定行树形图是否应重新排序以及如何排序
           Colv=if(symm)"Rowv" else TRUE,               # 确定列树形图是否应重新排序以及如何排序
           distfun = dist,                              # 用于计算行和列之间的距离（差异性）的函数
           hclustfun = hclust,                          # 当rowv或colv不是树形图时，用于计算分层聚类的函数,默认为hclust
           dendrogram = c("both","row","column","none"),    # 指示是绘制“none”、“row”、“column”还是“both”树形图的字符串。默认为both
           reorderfun = function(d, w) reorder(d, w),       # 树枝图的函数（d，w）和重排序行和列树枝图的权重。默认使用stats reorder.dendrogram
           symm = FALSE,                                # 逻辑指示是否应对称处理X；仅当X是平方矩阵时才能为真
           # data scaling
           scale = c("none","row", "column"),           # 指示字符是否应在行方向或列方向居中和缩放，或不居中和缩放
           na.rm=TRUE,
           # image plot
           revC = identical(Colv, "Rowv"),              # 逻辑指示是否应颠倒列顺序进行绘图
           add.expr,                                    # 调用Image后将计算的表达式。可用于向绘图中添加组件。
           # mapping data to colors
           breaks,                                      # 分隔x（数字向量）为不同颜色
           symbreaks=any(x < 0, na.rm=TRUE) || scale!="none",
           # colors
           col="heat.colors",
           # block sepration
           colsep,
           rowsep,
           sepcolor="white",
           sepwidth=c(0.05,0.05),
           # cell labeling
           cellnote,
           notecex=1.0,
           notecol="cyan",
           na.color=par("bg"),

           # level trace
           trace=c("column","row","both","none"),
           tracecol="cyan",
           hline=median(breaks),
           vline=median(breaks),
           linecol=tracecol,
           # Row/Column Labeling
           margins = c(5, 5),
           ColSideColors,
           RowSideColors,
           cexRow = 0.2 + 1/log10(nr),
           cexCol = 0.2 + 1/log10(nc),
           labRow = NULL,
           labCol = NULL,
           srtRow = NULL,
           srtCol = NULL,
           adjRow = c(0,NA),
           adjCol = c(NA,0),
           offsetRow = 0.5,
           offsetCol = 0.5,
           colRow = NULL,
           colCol = NULL,
           # color key + density info
           key = TRUE,
           keysize = 1.5,
           density.info=c("histogram","density","none"),
           denscol=tracecol,
           symkey = any(x < 0, na.rm=TRUE) || symbreaks,
           densadj = 0.25,
           key.title = NULL,
           key.xlab = NULL,
           key.ylab = NULL,
           key.xtickfun = NULL,
           key.ytickfun = NULL,
           key.par=list(),
           # plot labels
           main = NULL,
           xlab = NULL,
           ylab = NULL,
           # plot layout
           lmat = NULL,
           lhei = NULL,
           lwid = NULL,
           # extras
           extrafun=NULL,
           ...
           )
