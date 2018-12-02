library(corrplot)
library(Hmisc)
windowsFonts(myFont1=windowsFont("Times New Roman"))
data <- read.table("ARGs.MGEs.type.abundance.txt",header = T,sep = "\t",row.names = 1)
data <- t(data)
cor.matrix <- cor(data)
cor <- rcorr(as.matrix(data))
p <- cor$P

png(filename = "ARGs.MGEs.correlation.tiff",width = 7200,height = 7200,res = 600,type = "cairo")
corrplot(cor.matrix,method = "pie",type = "lower",tl.col = "black",p.mat = p,sig.level = c(0.05,0.01),insig = "label_sig",tl.cex = 2,cl.cex = 2,addgrid.col = "black",number.font = 2,pch.cex = 3)
dev.off()