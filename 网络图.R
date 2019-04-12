library(igraph)
library(psych)
otu <- read.table('otu_table.txt',header = T,row.names = 1)
# 计算OTU两两间相关系数矩阵
occor <- corr.test(otu,use = 'pairwise',method = 'spearman',adjust='fdr',
                   alpha = .05)
occor.r <- occor$r
occor.p <- occor$p
# 确定物种间相互作用关系阈值，不符合数据转换为0
occor.r[occor.p > 0.05 | abs(occor.r)< 0.6] = 0
# 构建igraph对象
igraph <- graph_from_adjacency_matrix(occor.r,mode = 'undirected',
                                      weighted = T,diag = F)
# 去掉无相关性OTU
bad.vs <- V(igraph)[degree(igraph)==0]
igraph <- delete.vertices(igraph,bad.vs)
# 将igraph加权属性赋值
igraph.weight <- E(igraph)$weight
# 作图去掉igraph权重
E(igraph)$weight <- NA
# 设定随机种子
set.seed(123)
plot(igraph,main='co-occurrence network',vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=T,margin=c(0,0,0,0))
# 按相关类型设置边颜色
sum(igraph.weight > 0)
sum(igraph.weight < 0)
E.color <- igraph.weight
E.color <- ifelse(E.color > 0,'red',ifelse(E.color < 0,'blue','grey'))
E(igraph)$color <- as.character(E.color)
# 改变edge颜色
set.seed(123)
plot(igraph,main='Co-occurence network',vertex.frame.color=NA,vertex.label=NA,
     edge.width=1,vertex.size=5,edge.lty=1,edge.curved=T,margin=c(0,0,0,0))
# 按相关性设置便宽度
# 可以设定edge的宽 度set edge width，例如将相关系数与edge width关联
E(igraph)$width = abs(igraph.weight)*4
# 改变edge宽度后出图
set.seed(123)
plot(igraph,main="Co-occurrence network",vertex.frame.color=NA,vertex.label=NA,
     vertex.size=5,edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
# 设置点的颜色和大小属性对应物种和丰度
# 添加OTU注释信息，如分类单元和丰度
# 另外可以设置vertices size, vertices color来表征更多维度的数据
# 注意otu_pro.txt文件为我随机产生的数据，因此网络图可能不会产生特定的模式或规律。
otu_pro = read.table("otu_pro.txt",head=T,row.names=1)
# set vertices size
igraph.size = otu_pro[V(igraph)$name,] # 筛选对应OTU属性
igraph.size1 = log((igraph.size$abundance)*100) # 原始数据是什么，为什么*100再取e对数
V(igraph)$size = igraph.size1
# set vertices color
igraph.col = otu_pro[V(igraph)$name,]
levels(igraph.col$phylum)
levels(igraph.col$phylum) = c("green","deeppink","deepskyblue","yellow","brown","pink","gray","cyan","peachpuff") # 直接修改levles可以连值全部对应替换
V(igraph)$color = as.character(igraph.col$phylum)
set.seed(123)
plot(igraph,main="Co-occurrence network",vertex.frame.color=NA,vertex.label=NA,
     edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
# 调整布局样式
# 改变layout,layout有很多，具体查看igraph官方帮助文档。
set.seed(123)
plot(igraph,main="Co-occurrence network",layout=layout_with_kk,vertex.frame.color=NA,vertex.label=NA,
     edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
set.seed(123)
plot(igraph,main="Co-occurrence network",layout=layout.fruchterman.reingold,vertex.frame.color=NA,vertex.label=NA,
     edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
# 按模块着色
# 模块性 modularity
fc = cluster_fast_greedy(igraph,weights =NULL)# cluster_walktrap cluster_edge_betweenness, cluster_fast_greedy, cluster_spinglass
modularity = modularity(igraph,membership(fc))
# 按照模块为节点配色
comps = membership(fc)
colbar = rainbow(max(comps))
V(igraph)$color = colbar[comps] 
set.seed(123)
plot(igraph,main="Co-occurrence network",vertex.frame.color=NA,vertex.label=NA,
     edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))
# 显示标签和点轮廓
# 最后添加删除color和label项可显示标签和点颜色边框
plot(igraph,main="Co-occurrence network",vertex.frame.color=NA,vertex.label=NA,
     edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))

常用网络属性
# network property
# 边数量 The size of the graph (number of edges)
num.edges = length(E(igraph)) # length(curve_multiple(igraph))
num.edges
# 顶点数量 Order (number of vertices) of a graph
num.vertices = length(V(igraph))# length(diversity(igraph, weights = NULL, vids = V(igraph)))
num.vertices
# 连接数(connectance) 网络中物种之间实际发生的相互作用数之和（连接数之和）占总的潜在相互作用数（连接数）的比例，可以反映网络的复杂程度
connectance = edge_density(igraph,loops=FALSE)# 同 graph.density;loops如果为TRUE,允许自身环（self loops即A--A或B--B）的存在
connectance
# 平均度(Average degree)
average.degree = mean(igraph::degree(igraph))# 或者为2M/N,其中M 和N 分别表示网络的边数和节点数。
average.degree
# 平均路径长度(Average path length)
average.path.length = average.path.length(igraph) # 同mean_distance(igraph) # mean_distance calculates the average path length in a graph
average.path.length
# 直径(Diameter)
diameter = diameter(igraph, directed = FALSE, unconnected = TRUE, weights = NULL)
diameter
# 群连通度 edge connectivity / group adhesion
edge.connectivity = edge_connectivity(igraph)
edge.connectivity
# 聚集系数(Clustering coefficient)：分局域聚类系数和全局聚集系数，是反映网络中节点的紧密关系的参数，也称为传递性。整个网络的全局聚集系数C表征了整个网络的平均的“成簇性质”。
clustering.coefficient = transitivity(igraph) 
clustering.coefficient
no.clusters = no.clusters(igraph)
no.clusters
# 介数中心性(Betweenness centralization)
centralization.betweenness = centralization.betweenness(igraph)$centralization 
centralization.betweenness
# 度中心性(Degree centralization)
centralization.degree = centralization.degree(igraph)$centralization
centralization.degree


# 微生物相关网络构建LSA

# sparcc
library(igraph)
pvals=read.table("sparcc_pvals_two_sided.txt",header=TRUE,sep="\t") 
pvals.mat=pvals[,2:ncol(pvals)]
pvals.mat[,1:length(pvals.mat)]
# 设置p value值 0 至非零
pvals.mat[pvals.mat==0]=0.000000001
# 转化为显著性
sig.mat <- -1*log10(pvals.mat)
# 去除小于1的值
sig.mat[sig.mat < 1] = 0
sig.mat <- as.matrix(sig.mat)
# 邻接矩阵转化为图表
sparcc.graph <- graph.adjacency(sig.mat,mode = 'undirected')
# 展示图标
layout <- layout.spring
plot(sparcc.graph,layout=layout, alpha = 0.3)

# network 3D
#install.packages("networkD3")
library("networkD3")

# 网络数据和节点属性数据以类似格式存入文本文件即可
# 网络文件有3列组成，第一列为
network <- "Src;Target;Value
Bioinfo;Biology;4
Bioinfo;Math;4
Bioinfo;Program;4
Bioinfo;NGS;4
Program;Linux;1
Program;Python;1
Program;R;1
NGS;RNAseq;1
NGS;ChIPseq;3
NGS;16Sseq;3
NGS;Metagenome;1
NGS;SingeCellSeq;3
NGS;DNAmethylseq;1
NGS;lncRNA;3
NGS;Exomeseq;1
NGS;TCGA;1
"

attribute <- "name;group;size
Bioinfo;Class;4
Biology;Class;4
Math;Class;4
Program;Class;4
NGS;Class;4
Linux;On;2
Python;Off;2
R;Off;2
RNAseq;Off;1
ChIPseq;On;1
16Sseq;On;1
Metagenome;On;1
SingeCellSeq;InPrepare;1
DNAmethylseq;InPrepare;1
lncRNA;InPrepare;1
Exomeseq;InPrepare;1
TCGA;InPrepare;1"

network <- read.table(text=network, sep=";", header=T, row.names=NULL, quote="", comment="")

network <- network[,1:3]
colnames(network) <- c("Src", "Target", "Value")

nodes <- unique(c(network$Src, network$Target))
factor_list <- sort(unique(c(levels(network$Src), levels(network$Target))))
num_list <- 0:(length(factor_list)-1)
levels(network$Src) <- num_list[factor_list %in% levels(network$Src)]
levels(network$Target) <- num_list[factor_list %in% levels(network$Target)]

attribute <- read.table(text=attribute, sep=";", header=T, row.names=NULL, quote="", comment="")
attribute <- attribute[match(factor_list, attribute$name),]
forceNetwork(Links = network, Nodes = attribute,
             width = 600, height=400,
             Source = "Src", Target = "Target",
             Value = "Value", NodeID = "name",
             Group = "group", opacity = 1, 
             legend = T, zoom = T, Nodesize = "size",
             bounded = T, opacityNoHover = 1, fontSize = 15)
