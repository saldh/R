# 安装与载入
install.packages("riverplot")
library(riverplot)
#  包内部演示数据
plot(riverplot.example())


# 1. 画一个6级能量流动图/桑基图
# 构造连接节点(边)的数据框，采用runif生成模拟数据
# 实验中每个节点间的连续情况是己知的
# 生成一个组1-5与组2-6对应的值数据框
edges = data.frame(N1 = paste0(rep(LETTERS[1:4], each = 4), rep(1:5, each = 16)),  
                   N2 = paste0(rep(LETTERS[1:4], 4), rep(2:6, each = 16)),  
                   Value = runif(80, min = 2, max = 5) * rep(c(1, 0.8, 0.6, 0.4, 0.3), each = 16),  
                   stringsAsFactors = F)  
# 筛选80%的记录，以免每个点都对应到4个点(可选)
edges = edges[sample(c(TRUE, FALSE), nrow(edges), replace = TRUE, prob = c(0.8, 0.2)),]  
head(edges)  

# 获得非冗余结点nodes
nodes = data.frame(ID = unique(c(edges$N1, edges$N2)), stringsAsFactors = FALSE)  
# 添加x: X为组编号，即列位置
nodes$x = as.integer(substr(nodes$ID, 2, 2))  
# Y为组类型字符，转换为ASCII编号，减65，即为A/B/C/D转换为0/1/2/3数值，即行位置
nodes$y = as.integer(sapply(substr(nodes$ID, 1, 1), charToRaw)) - 65  
# 添加行名
rownames(nodes) = nodes$ID  
head(nodes)  

# 添加颜色  
library(RColorBrewer)  
# brewer.pal生成柔合色，后面加调淡颜色  
palette = paste0(brewer.pal(4, "Set1"), "60")  

# 对每个节点生成相应的列表格式，颜色col，线条类型lty，文字颜色textcol
styles = lapply(nodes$y, function(n) {  
  list(col = palette[n+1], lty = 0, textcol = "black")  
})  
names(styles) = nodes$ID  

# 将点、单和样式合并为List，构建riverplot对象  
rp <- list(nodes = nodes, edges = edges, styles = styles)  
# 添加对你属性包括riverplot
class(rp) <- c(class(rp), "riverplot") 
# 绘制桑基图，plot_area设置绘图面积，yscale设置Y轴方向缩放
plot(rp, plot_area = 0.95, yscale=0.06)


# 绘制六个时间点四组间能量流动的桑基图

2. minard图



# 这个非常著名的图是Charles Minard在1869年所作的拿破仑东征俄国的信息图。Charles Minard是信息图表的之父，他是信息图领域的创始者。这张图描绘的是拿破仑在1812到1813年进攻俄国的情况。它的背景是一个真实的地图，西边是波兰的边境，东边是莫斯科。图上那条主线的宽度代表拿破仑军队的人数，黄色表示进攻路线，黑色表示撤退的路线： 他开始于42万人，在向莫斯科进军的过程中丧失了很多人，到达莫斯科时只剩下10万人，而最后从莫斯科活着返回的只剩下1万人。

Minard图也是桑基图的一种，采用riverplot包也可以绘制。如下：

library( riverplot )
data( minard )
nodes <- minard$nodes
edges <- minard$edges
colnames( nodes ) <- c( "ID", "x", "y" )
colnames( edges ) <- c( "N1", "N2", "Value", "direction" )

# color the edges by troop movement direction
edges$col <- c( "#e5cbaa", "black" )[ factor( edges$direction ) ]
# color edges by their color rather than by gradient between the nodes
edges$edgecol <- "col"

# generate the riverplot object
river <- makeRiver( nodes, edges )

style <- list( edgestyle= "straight", nodestyle= "invisible" )
# plot the generated object
plot( river, lty= 1, default.style= style )
# Add cities
with( minard$cities, points( Longitude, Latitude, pch= 19 ) )
with( minard$cities, text( Longitude, Latitude, Name, adj= c( 0, 0 ) ) )


# 绘制DNA双链 a DNA strand
plot.new()
par( usr= c( 0, 4, -2.5, 2.5 ) )
w <- 0.4
cols <- c( "blue", "green" )
init <- c( -0.8, -0.5 )
pos <- c( 1, -1 )
step <- 0.5
for( i in rep( rep( c( 1, 2 ), each= 2 ), 5 ) ) {
  curveseg( init[i], init[i] + step, pos[1], pos[2], width= w, col= cols[i] )
  init[i] <- init[i] + step
  pos <- pos * -1
}


#  绘制简单的分枝图
# 产生三个节点A/B/C
nodes <- c( LETTERS[1:3] )
# 产生两条边，A-C, B-C
edges <- list( A= list( C= 10 ), B= list( C= 10 ) )
# 生成点边样式数据，位置，标签，颜色样式
r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
                node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
                node_styles= list( A= list( col= "yellow" )) )
plot( r )

# 另一种写法，把位置，颜色，标签属性写在结果中
# equivalent form:
nodes <- data.frame( ID= LETTERS[1:3],
                     x= c( 1, 1, 2 ),
                     col= c( "yellow", NA, NA ),
                     labels= c( "Node A", "Node B", "Node C" ),
                     stringsAsFactors= FALSE )
r <- makeRiver( nodes, edges )
plot( r )


# 设置其它结点红色
# all nodes but "A" will be red:
r <- makeRiver( nodes, edges, default_style= list( col="red" ) )
plot( r )


# 属性的进一步赋值
# overwrite the node information from "nodes":
r <- makeRiver( nodes, edges, node_styles= list( A=list( col="red" ) ) )
plot( r )


#  绘图实战：三时间点桑基图
# Sciences封面文中采用了5个时间点的两个又桑基图。为方便演示，我们编写三个时间点的数据进行绘制。

# 三个时间点有5个结点
nodes = c("A", "B","Bnew","C","Cnew")

# 共有4条边，A-C名称重复不可用
edges <- list( A= list( B= 10 ), B= list( C= 10 ),   Bnew=list(C = 1)) # A= list( C= 5 ),

# 生成点边样式数据，位置，标签，颜色样式
r <- makeRiver( nodes, edges, node_xpos= c( 1,2,2,3,3 )
                , node_labels= c( A= "2016", B= "2017", Bnew = "BNew", C= "2018", Cnew="CNew" )
                ,node_styles= list( A= list( col= "green" ),  Bnew= list( col= "cyan" ), B= list( col= "cyan" ), C= list( col= "yellow" ) , Cnew= list( col= "yellow" ))
)
plot( r )


# 图形还需要进一步调整

# 设置5个时间点的位置和坐标
nodes = data.frame(ID = c("A", "B","Bnew","C","Cnew"), x = c(1, 2, 2, 3, 3), y = c(1, 3, 1, 2, 1), stringsAsFactors = FALSE)  
rownames(nodes)=nodes$ID
head(nodes)  

# 设置四条边的关联和属性
edges = data.frame(N1 = c("A","A","B","Bnew"),  
                   N2 = c("B","C","C","C"), 
                   Value = c(10,1,9,2),  
                   stringsAsFactors = F)  
head(edges)  

# 设置颜色等
# brewer.pal生成柔合色，后面加调淡颜色  
library(RColorBrewer)  
palette = paste0(brewer.pal(5, "Set1"), "60")  
# 对每个节点生成相应的列表格式，颜色col，线条类型lty，文字颜色textcol
styles = lapply(nodes$x, function(n) {  
  list(col = palette[n+1], lty = 0, textcol = "black")  
})  
names(styles) = nodes$ID 

# 将点、单和样式合并为List，构建riverplot对象  
rp <- list(nodes = nodes, edges = edges, styles = styles)  
# 添加对你属性包括riverplot
class(rp) <- c(class(rp), "riverplot") 
# 绘制桑基图，plot_area设置绘图面积，yscale设置Y轴方向缩放
plot(rp, plot_area = 1, yscale=0.26)