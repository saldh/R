if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("phyloseq", version = "3.8")
BiocManager::install("Biobase", version = "3.8")
BiocManager::install("BiocGenerics", version = "3.8")
BiocManager::install("biomformat", version = "3.8")
BiocManager::install("Biostrings", version = "3.8")
BiocManager::install("multtest", version = "3.8")
BiocManager::install("rhdf5", version = "3.8")
# 
library("devtools")
install_github("phyloseq/joey711")
#
packageVersion('phyloseq')
library("phyloseq"); packageVersion("phyloseq")
library(ape)
library(xlsx)
library(plyr)
library(dplyr)
library(gdata)
library(ggplot2)
library(ape)
library(extrafont)
library(scales)
library(MASS)
library(PMCMRplus)
library(psych)
library(pheatmap)
library(RColorBrewer)
library(phyloseq)
library(vegan)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(phangorn)
library(picante)
library(reshape2)
library(reshape)
loadfonts(device = "pdf", quiet = FALSE)
library(data.table)
library("ggplot2"); packageVersion("ggplot2")
#璁剧疆榛樿缁樺浘涓婚锛実gplot榛樿涓嶆槸杩欎釜
theme_set(theme_bw())
# Create a pretend OTU table that you read from a file, called otumat
otutable <- import_biom('otu_table4.biom')
treefile <- read.tree("rep_seqs.tree")
map <-import_qiime_sample_data('sub_map_uc.txt')
tax <- read.table('rep_seqs_tax.txt',header = T,row.names = 1,
                  check.names = T,sep = '\t')
tax <- tax_table(as.matrix(tax[,1:7]))
#鍔犲叆phyloseq浣撶郴
physeq <- merge_phyloseq(otutable,treefile,map,tax)
plot_bar(data, fill = "phylum")
# 鏋勫缓闅忔満鏍戝悎骞秔hyloseq
library("ape")
random_tree = rtree(ntaxa(physeq), rooted=TRUE, tip.label=taxa_names(physeq))
plot(random_tree)
#鍙噸鏂版瀯寤哄厓绱?
physeq1 = merge_phyloseq(physeq,random_tree)
physeq1
distance(physeq1,method = 'unifrac')
physeq_Ord_PCoA_bray = ordinate(physeq1, "PCoA", distance="bray")

# 浣跨敤phyloseq灞曠ず杩涘寲鏍戝拰count涓板害鐑浘
plot_tree(physeq1, color="Location", label.tips="taxa_names", ladderize="left", plot.margin=0.3)
plot_tree(physeq1, color="Depth", shape="Location", label.tips="taxa_names", ladderize="right", plot.margin=0.3)
plot_heatmap(physeq1)
plot_heatmap(physeq1, taxa.label="Phylum")

#鍩轰簬biom鏍煎紡鏁版嵁瀵煎叆
rich_dense_biom  = system.file("extdata", "rich_dense_otu_table.biom",  package="phyloseq")
rich_sparse_biom = system.file("extdata", "rich_sparse_otu_table.biom", package="phyloseq")
min_dense_biom   = system.file("extdata", "min_dense_otu_table.biom",   package="phyloseq")
min_sparse_biom  = system.file("extdata", "min_sparse_otu_table.biom",  package="phyloseq")
treefilename = system.file("extdata", "biom-tree.phy",  package="phyloseq")
refseqfilename = system.file("extdata", "biom-refseq.fasta",  package="phyloseq")

import_biom(rich_dense_biom, treefilename, refseqfilename, parseFunction=parse_taxonomy_greengenes)
myData = import_biom(rich_dense_biom, treefilename, refseqfilename, parseFunction=parse_taxonomy_greengenes)
myData
sample_data(myData)
plot_tree(myData, color="Genus", shape="BODY_SITE", size="abundance")
plot_richness(myData, x="BODY_SITE", color="Description")

plot_bar(myData, fill="Genus")
refseq(myData)

# 鍩轰簬qiime杈撳嚭鏂囦欢瀵煎叆
otufile = system.file("extdata", "GP_otu_table_rand_short.txt.gz", package="phyloseq")
mapfile = system.file("extdata", "master_map.txt", package="phyloseq")
trefile = system.file("extdata", "GP_tree_rand_short.newick.gz", package="phyloseq")
rs_file = system.file("extdata", "qiime500-refseq.fasta", package="phyloseq")
qiimedata = import_qiime(otufile, mapfile, trefile, rs_file)
qiimedata
plot_bar(qiimedata, x="SampleType", fill="Phylum")


# 浣跨敤phyloseq鐨勪緥瀛?
#浣跨敤phyloseq
data(GlobalPatterns)
data(esophagus)
data(enterotype)
data(soilrep)
?GlobalPatterns

example(enterotype, ask=FALSE)
?make_network
ig <- make_network(enterotype, "samples", max.dist=0.3)
plot_network(ig, enterotype, color="SeqTech", 
             shape="Enterotype", line_weight=0.3, label=NULL)

# phyloseq鎸夌収涓€瀹氳姹傚悎骞舵暟鎹?
##phyloseq鍚堝苟鏁版嵁
data(GlobalPatterns)
GP = GlobalPatterns
#鏁版嵁杩囨护锛屼繚鐣檆ount澶т簬0鐨凮TU
GP = prune_taxa(taxa_sums(GlobalPatterns) > 0, GlobalPatterns)
humantypes = c("Feces", "Mock", "Skin", "Tongue")
sample_data(GP)$human <- get_variable(GP, "SampleType") %in% humantypes

#鎸夌収SampleType鍚堝苟鏍峰搧
mergedGP = merge_samples(GP, "SampleType")
rowSums(otu_table(GP10)[, ocean_samples])
otu_table(mGP10)["Ocean", ]
plot_richness(GP, "human", "SampleType", title="unmerged")
sample_data(mergedGP)$SampleType = sample_names(mergedGP)
sample_data(mergedGP)$human = sample_names(mergedGP) %in% humantypes
plot_richness(mergedGP, "human", "SampleType", title="merged")
SD = merge_samples(sample_data(GP), "SampleType")
print(SD[, "SampleType"])
sample_data(mergedGP)
OTUnames10 = names(sort(taxa_sums(GP), TRUE)[1:10])
GP10  = prune_taxa(OTUnames10,  GP)
mGP10 = prune_taxa(OTUnames10, mergedGP)
ocean_samples = sample_names(subset(sample_data(GP), SampleType=="Ocean"))
print(ocean_samples)
otu_table(GP10)[, ocean_samples]

# 鏍规嵁杩涘寲鍏崇郴瀵筄TU杩涜鍚堝苟
load("example-data.RData")
plot_tree(closedps, color="Treatment", size="abundance", 
          sizebase=2, label.tips="taxa_names")

x1 = merge_taxa(closedps, taxa_names(closedps)[3:27], 2)
plot_tree(x1, color="Treatment", size="abundance", sizebase=2, label.tips="taxa_names")

鍩轰簬phyloseq鏁版嵁鍚堝苟
#鍚堝苟phyloseq瀵硅薄
data(GlobalPatterns)
tree = phy_tree(GlobalPatterns)
tax  = tax_table(GlobalPatterns)
otu  = otu_table(GlobalPatterns)
sam  = sample_data(GlobalPatterns)
otutax = phyloseq(otu, tax)
otutax
GP2 = merge_phyloseq(otutax, sam, tree)
identical(GP2, GlobalPatterns)
##鏇村姞澶嶆潅鐨勬柟寮?
otusamtree = phyloseq(otu, sam, tree)
GP3 = merge_phyloseq(otusamtree, otutax)
GP3
identical(GP3, GlobalPatterns)
GP4 = merge_phyloseq(otusamtree, tax_table(otutax))
GP4
identical(GP4, GlobalPatterns)

phyloseq绯荤粺鍐呮暟鎹煡鐪?
##璁块棶鏁版嵁鍙婂叾鏁版嵁澶勭悊
data("GlobalPatterns")
GlobalPatterns
#鏌ョ湅OTU鏁伴噺
ntaxa(GlobalPatterns)
#鏌ョ湅鏍峰搧鏁伴噺
nsamples(GlobalPatterns)
#鏍峰搧鍚嶆煡鐪嬶紝姝ゅ鏌ョ湅鍓嶄簲涓牱鍝佸悕绉?
sample_names(GlobalPatterns)[1:5]
#鏌ョ湅tax鐨勫垎绫荤瓑绾т俊鎭?
rank_names(GlobalPatterns)
#鏌ョ湅mapping鏂囦欢琛ㄥご锛堟牱鍝佷俊鎭枃浠跺垪鍚嶏級
sample_variables(GlobalPatterns)
#鏌ョ湅閮ㄥ垎OTU琛ㄦ牸鐭╅樀
otu_table(GlobalPatterns)[1:5, 1:5]
#鏌ョ湅閮ㄥ垎娉ㄩ噴锛坱ax锛夋枃浠剁煩闃?
tax_table(GlobalPatterns)[1:5, 1:4]
#杩涘寲鏍戞煡鐪嬶紝娉ㄦ剰涓嶆槸鍙鍖?
phy_tree(GlobalPatterns)
#鏌ョ湅OTU鍚嶇О锛屾澶勬煡鐪嬪墠鍗佷釜
taxa_names(GlobalPatterns)[1:10]

#鎸夌収涓板害鎻愬彇鍓嶅崄涓狾TU锛屽苟鍙鍖栬繘鍖栨爲
myTaxa = names(sort(taxa_sums(GlobalPatterns), decreasing = TRUE)[1:10])
ex1 = prune_taxa(myTaxa, GlobalPatterns)
plot(phy_tree(ex1), show.node.label = TRUE)

#鏁版嵁棰勫鐞?

#杞寲OTUcount鏁颁负鐩稿涓板害
GPr  = transform_sample_counts(GlobalPatterns, function(x) x / sum(x) )
otu_table(GPr)[1:5][1:5]
#鎻愬彇涓板害澶т簬鍗佷竾浠戒箣涓€鐨凮TU
GPfr = filter_taxa(GPr, function(x) mean(x) > 1e-5, TRUE)
#鎻愬彇鎸囧畾鍒嗙被鐨凮TU
GP.chl = subset_taxa(GlobalPatterns, Phylum=="Chlamydiae")
#鎻愬彇鎬籧ount鏁伴噺澶т簬20鐨勬牱鍝?
GP.chl = prune_samples(sample_sums(GP.chl)>=20, GP.chl)
#鍚堝苟鎸囧畾OTU锛? taxa_names(GP.chl)[1:5]涓轰竴涓狾TU
GP.chl.merged = merge_taxa(GP.chl, taxa_names(GP.chl)[1:5])
# gpsfbg = tax_glom(gpsfb, "Family")
# plot_tree(gpsfbg, color="SampleType", shape="Class", size="abundance")

transform_sample_counts(GP.chl, function(OTU) OTU/sum(OTU) )
#鍘婚櫎鑷冲皯20锛呮牱鏈腑鏈杩?3娆′互涓婄殑OTU,娉ㄦ剰缂栧啓鍑芥暟鐨勫舰寮?
GP = filter_taxa(GlobalPatterns, function(x) sum(x > 3) > (0.2*length(x)), TRUE)
#瀵规牱鍝佸垎缁勬枃浠秏apping娣诲姞涓€鍒?
sample_data(GP)$human = factor( get_variable(GP, "SampleType") %in% c("Feces", "Mock", "Skin", "Tongue") )

#娴嬪簭娣卞害杩涜缁熶竴
total = median(sample_sums(GP))
standf = function(x, t=total) round(t * (x / sum(x)))
gps = transform_sample_counts(GP, standf)
sample_sums(gps)
#鍙樺紓绯绘暟杩囨护OTU锛屽噺灏戜竴浜涙剰澶朞TU璇樊
gpsf = filter_taxa(gps, function(x) sd(x)/mean(x) > 3.0, TRUE)
#鎻愬彇鎸囧畾闂ㄧ被OTU
gpsfb = subset_taxa(gpsf, Phylum=="Bacteroidetes")
title = "plot_bar; Bacteroidetes-only"
plot_bar(gpsfb, "SampleType", "Abundance", title=title)


# 鍩轰簬寰敓鐗╃兢钀芥暟鎹窛绂荤殑璁＄畻
##鍩轰簬phyloseq鐨勫井鐢熺墿缇よ惤鑱氱被鐨勮绠?
library(phyloseq)
library("plyr"); packageVersion("plyr")
data(enterotype)
enterotype <- subset_taxa(enterotype, Genus != "-1")
#鎴戜滑杩涜鏌ョ湅鍙戠幇鏈夊緢澶氱殑璺濈绠楁硶
dist_methods <- unlist(distanceMethodList)
print(dist_methods)
#鍒犻櫎闇€瑕乼ree鐨勮窛绂荤畻娉?
# Remove them from the vector
dist_methods <- dist_methods[-(1:3)]
# 鍒犻櫎闇€瑕佺敤鎴疯嚜瀹氫箟鐨勮窛绂荤畻娉?
dist_methods["designdist"]
dist_methods = dist_methods[-which(dist_methods=="ANY")]

plist <- vector("list", length(dist_methods))
names(plist) = dist_methods
for( i in dist_methods ){
  # Calculate distance matrix
  iDist <- distance(enterotype, method=i)
  # Calculate ordination
  iMDS  <- ordinate(enterotype, "MDS", distance=iDist)
  ## Make plot
  # Don't carry over previous plot (if error, p will be blank)
  p <- NULL
  # Create plot, store as temp variable, p
  p <- plot_ordination(enterotype, iMDS, color="SeqTech", shape="Enterotype")
  # Add title to each plot
  p <- p + ggtitle(paste("MDS using distance method ", i, sep=""))
  # Save the graphic to file.
  plist[[i]] = p
}

df = ldply(plist, function(x) x$data)
names(df)[1] <- "distance"
p = ggplot(df, aes(Axis.1, Axis.2, color=SeqTech, shape=Enterotype))
p = p + geom_point(size=3, alpha=0.5)
p = p + facet_wrap(~distance, scales="free")
p = p + ggtitle("MDS on various distance metrics for Enterotype dataset")
p


df = ldply(plist, function(x) x$data)
names(df)[1] <- "distance"
p = ggplot(df, aes(Axis.1, Axis.2, color=Enterotype, shape=SeqTech))
p = p + geom_point(size=3, alpha=0.5)
p = p + facet_wrap(~distance, scales="free")
p = p + ggtitle("MDS on various distance metrics for Enterotype dataset")
p

#jsd
print(plist[["jsd"]])

print(plist[["jaccard"]])

print(plist[["bray"]])

宸紓缁熻


library("phyloseq"); packageVersion("phyloseq")
library("cluster"); packageVersion("cluster")
library("ggplot2"); packageVersion("ggplot2")
##紓
theme_set(theme_bw())
# Load data
data(enterotype)
#jsd
exord = ordinate(enterotype, method="MDS", distance="jsd")

pam1 = function(x, k){list(cluster = pam(x,k, cluster.only=TRUE))}
x = phyloseq:::scores.pcoa(exord, display="sites")
# gskmn = clusGap(x[, 1:2], FUN=kmeans, nstart=20, K.max = 6, B = 500)
gskmn = clusGap(x[, 1:2], FUN=pam1, K.max = 6, B = 50)

gap_statistic_ordination = function(ord, FUNcluster, type="sites", K.max=6, axes=c(1:2), B=500, verbose=interactive(), ...){
  require("cluster")
  #   If "pam1" was chosen, use this internally defined call to pam
  if(FUNcluster == "pam1"){
    FUNcluster = function(x,k) list(cluster = pam(x, k, cluster.only=TRUE))     
  }
  # Use the scores function to get the ordination coordinates
  x = phyloseq:::scores.pcoa(ord, display=type)
  #   If axes not explicitly defined (NULL), then use all of them
  if(is.null(axes)){axes = 1:ncol(x)}
  #   Finally, perform, and return, the gap statistic calculation using cluster::clusGap  
  clusGap(x[, axes], FUN=FUNcluster, K.max=K.max, B=B, verbose=verbose, ...)
}

plot_clusgap = function(clusgap, title="Gap Statistic calculation results"){
  require("ggplot2")
  gstab = data.frame(clusgap$Tab, k=1:nrow(clusgap$Tab))
  p = ggplot(gstab, aes(k, gap)) + geom_line() + geom_point(size=5)
  p = p + geom_errorbar(aes(ymax=gap+SE.sim, ymin=gap-SE.sim))
  p = p + ggtitle(title)
  return(p)
}

gs = gap_statistic_ordination(exord, "pam1", B=50, verbose=FALSE)
print(gs, method="Tibs2001SEmax")

plot_clusgap(gs)

plot(gs, main = "Gap statistic for the 'Enterotypes' data")
mtext("Looks like 4 clusters is best, with 3 and 5 close runners up.")