library(ggplot2)
data=read.table('volcano.txt',header = T ,row.names = 1)
threshold <- as.factor((data$log2FoldChange>1.5 | data$log2FoldChange < -1.5 & data$padj < 0.05))
r03=ggplot(data,aes(x=log2FoldChange,y=-log10(padj)))
r03 + geom_point()
#改变点的颜色
r03 + geom_point(color ="red")
r03 +geom_point(aes(color ="red"))
r03 + geom_point(aes(color =significant))
#设置坐标轴范围和标题 # xlim()，ylim()函数，labs(title=“..”,x=“..”,y=“..”)函数
r03xy = r03 +geom_point(aes(color =significant)) + xlim(-4,4) + ylim(0,30)
r03xy + labs(title="Volcanoplot",x="log2(FC)")
r03xy + labs(title="Volcanoplot",x=expression_r(log[2](FC)), y=expression_r(-log[10](FDR)))
#自定义颜色
r03xyp = r03xy + labs(title="Volcanoplot",x=expression_r(log[2](FC)), y=expression_r(-log[10](FDR)))
r03xyp + scale_color_manual(values =c("green","black", "red"))
volcano = r03xyp +scale_color_manual(values = c("#00ba38","#619cff","#f8766d"))
#添加阈值线
volcano+geom_hline(yintercept=1.3)+geom_vline(xintercept=c(-1,1))
volcano+geom_hline(yintercept=1.3,linetype=4)+geom_vline(xintercept=c(-1,1),linetype=4)
#保存图片
ggsave("volcano.png")
ggsave("volcano8.png",volcano,width=8,height=8)
