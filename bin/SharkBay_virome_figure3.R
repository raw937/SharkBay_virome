#SharkBay virome paper 
#Figure 3 
#Dr. Richard Allen White III

#load libraries
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

#load data
cas <- read.delim("CRISPR_antiviral_norm.txt")
brex <- read.delim("BREX_antiviral_norm.txt")
disarm <- read.delim("DISARM_antiviral_norm.txt")

#melt for ggplot2
mm <- melt(cas)
mm0 <- melt(brex)
mm1 <- melt(disarm)

#plot A
t <- ggplot(mm, aes(x=variable, y=Gene, color=variable)) 
t <- t + geom_point(aes(size=log2(value)))
t <- t + labs(title="", x="", y="CRISPR-Cas Genes", colour="Fraction")
t <- t + theme_bw() + ggtitle("A") + theme(plot.title = element_text(hjust = 0, size=50))
#t <- t + theme(axis.text.x = element_text(angle = 45, hjust = 1))
t

#plot B
t1 <- ggplot(mm0, aes(x=variable, y=Gene, color=variable)) 
t1 <- t1 + geom_point(aes(size=log2(value)))
t1 <- t1 + labs(title="", x="", y="BREX Genes", colour="Fraction")
t1 <- t1 + theme_bw() + ggtitle("B") + theme(plot.title = element_text(hjust = 0, size=50))
#t1 <- t1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
t1

#plot C
t2 <- ggplot(mm1, aes(x=variable, y=Gene, color=variable)) 
t2 <- t2 + geom_point(aes(size=log2(value)))
t2 <- t2 + labs(title="", x="", y="DISARM Genes", colour="Fraction")
t2 <- t2 + theme_bw() + ggtitle("C") + theme(plot.title = element_text(hjust = 0, size=50))
#t2 <- t2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
t2

#arrange plot
grid.arrange(t, t1, t2, ncol=1)
