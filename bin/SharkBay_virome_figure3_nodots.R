#SharkBay virome paper 
#Figure 3 
#Dr. Richard Allen White III

#load libraries
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

#load data
cas <- read.delim("CRISPR_antiviral_forBar.txt")
brex <- read.delim("BREX_antiviral_forBar.txt")
disarm <- read.delim("DISARM_antiviral_forBar.txt")

#melt for ggplot2
mm <- melt(cas)
mm0 <- melt(brex)
mm1 <- melt(disarm)


#plot A
t <- ggplot(mm, aes(x= reorder(variable, +value), y=value, fill=factor(Fraction)))  
t <- t + geom_bar(stat="identity",position="dodge")
t <- t + labs(title="", x="", y="Relative Abundance", fill="Fraction")
t <- t + coord_flip() + theme_bw()
t <- t + ggtitle("A") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Set2")
t


#plot B
t1 <- ggplot(mm0, aes(x= reorder(variable, +value), y=value, fill=factor(Fraction)))  
t1 <- t1 + geom_bar(stat="identity",position="dodge")
t1 <- t1 + labs(title="", x="", y="Relative Abundance", fill="Fraction")
t1 <- t1 + coord_flip() + theme_bw()
t1 <- t1 + ggtitle("B") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Set2")
t1

#plot C
t2 <- ggplot(mm1, aes(x= reorder(variable, +value), y=value, fill=factor(Fraction)))  
t2 <- t2 + geom_bar(stat="identity",position="dodge")
t2 <- t2 + labs(title="", x="", y="Relative Abundance", fill="Fraction")
t2 <- t2 + coord_flip() + theme_bw()
t2 <- t2 + ggtitle("C") + theme(plot.title = element_text(hjust = 0, size=50)) +
  scale_fill_brewer(palette="Set2")
t2


#arrange plot
jpeg("Plot3.jpeg", width = 6, height = 8, units = 'in', res = 300)
grid.arrange(t, t1, t2, ncol=1)
dev.off()

tiff("Plot3.tiff", width = 6, height = 8, units = 'in', res = 300)
grid.arrange(t, t1, t2, ncol=1)
dev.off()
