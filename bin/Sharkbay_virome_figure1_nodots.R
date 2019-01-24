#SharkBay virome paper 
#Figure 1
#Dr. Richard Allen White III

#load libraries
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

#load data
tax_h <- read.delim("taxonomy_highest_metavir2_edited.txt")
tax_m <- read.delim("taxonomy_ssDNA_metavir2_norm.txt")
tax_l <- read.delim("taxonomy_ssDNA-family_metavir2_norm-forBar.txt")
seed <- read.delim("SEED_annotations_mgrast_norm.txt")
gaas <- read.delim("GAAS_genome_sizes.txt")
ko_lv1 <- read.delim("KO_lvl1_annotation.txt")


#melt for ggplot2
mm <- melt(tax_h)
mm0 <- melt(tax_m)
mm1 <- melt(tax_l)
mm2 <- melt(gaas)
mm3 <- melt(seed)
mm4 <- melt(ko_lv1)


#plot A
t <- ggplot(mm, aes(x= reorder(variable, +value), y=value, fill=factor(Taxonomy))) 
t <- t + geom_bar(stat="identity")
t <- t + labs(title="", x="", y="Relative Abundance", fill="Viral Classification")
t <- t + theme_bw() + ggtitle("A") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Spectral")
t

#plot B
t0 <- ggplot(mm0, aes(x= reorder(variable, +value), y=value, fill=factor(Taxonomy))) 
t0 <- t0 + geom_bar(stat="identity")
t0 <- t0 + labs(title="", x="", y="Relative Abundance", fill="Viral Classification")
t0 <- t0 + theme_bw() + ggtitle("B") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Set3")
t0

#plot C
t1 <- ggplot(mm1, aes(x= reorder(variable, +value), y=value, fill=factor(Taxonomy)))  
t1 <- t1 + geom_bar(stat="identity",position="dodge")
t1 <- t1 + labs(title="", x="", y="Relative Abundance", fill="Taxonomy")
t1 <- t1 + coord_flip() + theme_bw()
t1 <- t1 + ggtitle("C") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Set2")
t1

#plot D
t2 <- ggplot(mm2, aes(x= reorder(variable, -value), y=value, fill=factor(Genome.Size))) 
t2 <- t2 + geom_bar(stat="identity")
t2 <- t2 + labs(title="", x="", y="Relative Abundance", fill="Genome Size (kbp)")
t2 <- t2 + theme_bw() + ggtitle("D") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Spectral")
t2

#plot E
t3 <- ggplot(mm3, aes(x= reorder(variable, +value), y=value, fill=factor(Fraction)))  
t3 <- t3 + geom_bar(stat="identity",position="dodge")
t3 <- t3 + labs(title="", x="SEED Subsystem", y="Relative Abundance", fill="Fraction")
t3 <- t3 + coord_flip() + theme_bw()
t3 <- t3 + ggtitle("E") + theme(plot.title = element_text(hjust = 0, size=50))  +
  scale_fill_brewer(palette="Set2")
t3

#plot F
t4 <- ggplot(mm4, aes(x= reorder(variable, +value), y=value, fill=factor(Fraction)))  
t4 <- t4 + geom_bar(stat="identity",position="dodge")
t4 <- t4 + labs(title="", x="KEGG - KO Level 1", y="Relative Abundance", fill="Fraction")
t4 <- t4 + coord_flip() + theme_bw()
t4 <- t4 + ggtitle("F") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Set2")
t4

#arrange plot
grid.arrange(t, t0, t1, t2, t3, t4, ncol=2)


#arrange plot
jpeg("Plot1.jpeg", width = 12, height = 15, units = 'in', res = 300)
grid.arrange(t, t0, t1, t2, t3, t4, ncol=2)
dev.off()
