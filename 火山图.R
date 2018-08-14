setwd("C:/Users/10020/Documents")
getwd()
library(ggplot2)
library(ggthemes)
library(Cairo)

data <- read.delim("volcano.txt",header = T,sep="\t",na.strings = "")

data$threshold <- as.factor(ifelse(data$pvalues < 0.05 & abs(log2(data$foldchange)) >=1.5,ifelse(log2(data$foldchange) > 1.5 ,'Up','Down'),'Not'))

##Construct the plot object
# with legend
Cairo(file="volcan_PNG_300_lengend_dpi.png", 
      type="png",
      units="in",
      bg="white",
      width=5.5, 
      height=5, 
      pointsize=12, 
      dpi=300)
ggplot(data=data, 
       aes(x=log2(foldchange), y =-log10(pvalues), 
           colour=threshold,fill=threshold)) +
  scale_color_manual(values=c("blue", "grey","red"))+
  geom_point(alpha=0.4, size=1.2) +
  xlim(c(-4, 4)) +
  theme_bw(base_size = 12, base_family = "Times") +
  geom_vline(xintercept=c(-1.5,1.5),lty=4,col="grey",lwd=0.6)+
  geom_hline(yintercept = -log10(0.05),lty=4,col="grey",lwd=0.6)+
  theme(legend.position="right",
        panel.grid=element_blank(),
        legend.title = element_blank(),
        legend.text= element_text(face="bold", color="black",family = "Times", size=8),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="black", size=12),
        axis.text.y = element_text(face="bold",  color="black", size=12),
        axis.title.x = element_text(face="bold", color="black", size=12),
        axis.title.y = element_text(face="bold",color="black", size=12))+
  labs(x="log2 (fold change)",y="-log10 (p-value)",title="Volcano picture of DEG")
dev.off()



# without legend
Cairo(file="volcan_PNG_300_dpi.png", 
      type="png",
      units="in",
      bg="white",
      width=6, 
      height=5, 
      pointsize=12, 
      dpi=300)
ggplot(data=data, 
       aes(x=log2(foldchange), y =-log10(pvalues), 
           colour=threshold,fill=threshold)) +
  scale_color_manual(values=c("blue", "grey","red"))+
  geom_point(alpha=0.4, size=1.2) +
  xlim(c(-4, 4)) +
  theme_bw(base_size = 12, base_family = "Times") +
  geom_vline(xintercept=c(-1.5,1.5),lty=4,col="grey",lwd=0.6)+
  geom_hline(yintercept = -log10(0.05),lty=4,col="grey",lwd=0.6)+
  theme(legend.position="none",
        panel.grid=element_blank(),
        # legend.title = element_blank(),
        # legend.text= element_text(face="bold", color="black",family = "Times", size=8),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="black", size=12),
        axis.text.y = element_text(face="bold",  color="black", size=12),
        axis.title.x = element_text(face="bold", color="black", size=12),
        axis.title.y = element_text(face="bold",color="black", size=12))+
  labs(x="log2 (fold change)",y="-log10 (p-value)",title="Volcano picture of DEG")
dev.off()
