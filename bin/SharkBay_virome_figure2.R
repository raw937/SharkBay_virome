#SharkBay virome paper 
#Figure 2
#Dr. Richard Allen White III

#load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(vegan)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(reshape2)
library(lattice)
library(ecodist)
library(rgl)

#load data
tax_h <- read.delim("taxonomy_highest_metavir2_compare-others.txt")
tax_m <- read.delim("taxonomy_ssDNA-family_metavir2_compare-others_norm.txt")
tax_l <- read.delim("taxonomy_ssDNA-family_down_metavir2_compare-others_norm_100x.txt")


#load data PCA
data.t  <- read.csv("All_microbialites_raw_reads_1e5.csv", header=T, row.names=1)
data.t <- data.t[, -c(2,3,4,5)]
data.hel  <- sqrt(data.t)
data.hel.pca  <- rda(t(data.hel))
p <- length(data.hel.pca$CA$eig)
data.hel.pca.sc1 <- scores(data.hel.pca, display="wa", scaling=1, choices=c(1:p))
variance = (data.hel.pca$CA$eig / sum(data.hel.pca$CA$eig))*100

#cluster groups
source("pvclust_bcdist.R")
pathways_wide.hel.pv_fit <- pvclust(as.matrix(data.t), method.hclust="ward", method.dist="bray–curtis", n=1000)

# look at fit and decide cut height
p0 <- plot(pathways_wide.hel.pv_fit) 
pathways_wide.hel.groups <- cutree(pathways_wide.hel.pv_fit$hclust, h=1.0) # slice dendrogram for groups

#melt for ggplot2
mm <- melt(tax_h)
mm0 <- melt(tax_m)
mm1 <- melt(tax_l)

#plot A
t <- ggplot(mm, aes(x= reorder(variable, +value), y=value, fill=factor(Taxonomy))) 
t <- t + geom_bar(stat="identity")
t <- t + labs(title="", x="Refseq taxonomy", y="Relative Abundance", fill="Viral Classification")
t <- t + theme_bw() + ggtitle("A") + theme(plot.title = element_text(hjust = 0, size=50))
t

#plot B
t0 <- ggplot(mm0, aes(x= reorder(variable, +value), y=value, fill=factor(Taxonomy))) 
t0 <- t0 + geom_bar(stat="identity")
t0 <- t0 + labs(title="", x="Refseq taxonomy", y="Relative Abundance", fill="Viral Classification")
t0 <- t0 + theme_bw() + ggtitle("B") + theme(plot.title = element_text(hjust = 0, size=50))
t0

#plot C
t1 <- ggplot(mm1, aes(x=variable, y=Taxonomy, color=variable)) 
t1 <- t1 + geom_point(aes(size=log2(value)))
t1 <- t1 + labs(title="", x="Refseq taxonomy", y="Relative Abundance", colour="Site")
t1 <- t1 + theme_bw() + ggtitle("C") + theme(plot.title = element_text(hjust = 0, size=50))
t1

p1 <- qplot(data.hel.pca.sc1[,1], 
            data.hel.pca.sc1[,2], 
            label=rownames(data.hel.pca.sc1), 
            size=2, geom=c("point"), 
            xlab= paste("PC1 (", round(variance[1],2) ," % Variance)"), 
            ylab= paste("PC2 (", round(variance[2],2) ," % Variance)"), 
            color= factor(pathways_wide.hel.groups)) + 
  geom_text(hjust=-0.1, vjust=0, colour="black", size=6) + theme_bw() + xlim(-10,13) + theme(legend.position="none") 
p1 <- p1 + ggtitle("D") + theme(plot.title = element_text(hjust = 0, size=50)) 
p1


#arrange plot
grid.arrange(t, t0, t1, p1, ncol=2)


#rearrange ggplot2 with one large plot on top and two on bottom
grid.arrange(arrangeGrob(t, t0, ncol=2),p1, heights=c(1/4, 2/4), ncol=1) #plot on bottom