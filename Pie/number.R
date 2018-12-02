#ARGs type
ARGs.type <- read.table("ARGs.type.txt",header = T,sep = "\t",row.names = 1)
lbls <- paste(rownames(ARGs.type)," ",ARGs.type$C0,sep = "")
windowsFonts(myFont1=windowsFont("Times New Romans"))
library(RColorBrewer)
png(filename = "ARGs.type.pie.tif",width = 7200,height = 5400,type = "cairo")
par(family = "myFont1",lwd = 10)
pie(ARGs.type$C0,labels = lbls,col = brewer.pal(8,"Set3"),clockwise = TRUE,border = rep("black",8),edges = 500,cex = 15,font = 2)
dev.off()

#ARGs mechanisms
ARGs.mechanism <- read.table("ARGs.mechanism.txt",header = T,sep = "\t",row.names = 1)
lbls <- paste(rownames(ARGs.mechanism)," ",ARGs.mechanism$C0,sep = "")
windowsFonts(myFont1=windowsFont("Times New Romans"))
library(RColorBrewer)
tiff(filename = "ARGs.mechanism.pie.tiff",width = 7600,height = 5400,type = "windows",compression = "lzw")
par(family = "myFont1",lwd = 10)
pie(ARGs.mechanism$C0,labels = lbls,col = brewer.pal(4,"Set1"),border = rep("black",8),edges = 500,cex = 15,font = 2)
dev.off()