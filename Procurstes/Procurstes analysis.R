windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体

otu <- read.csv("ARGs.abundance.txt",header = TRUE,sep = "\t",row.names = 1)
#载入第一个分析数据
otu <- t(otu)
otu <- as.matrix(otu)
#调整数据格式

env <- read.csv("genus.new.txt",header = TRUE,sep = "\t",row.names = 1)
#载入第二个分析数据
env <- t(env)
env <- as.matrix(env)
#调整数据格式

library(vegan)
#载入分析包

otu.dist <- vegdist(otu)
env.dist <- vegdist(env)
#计算距离矩阵

mds.otu <- monoMDS(otu.dist)
mds.env <- monoMDS(env.dist)
#对距离矩阵进行mds分析

pro.otu.env <- procrustes(mds.otu,mds.env)
#进行procrustes分析

protest(mds.otu,mds.env)
#手动保存结果

png(filename = "ARGs.taxa.tif",width = 5400,height = 5400,res = 700,type = "cairo")
plot(pro.otu.env,main = "Procrustes analysis",ar.col = "navy",lwd = 2,type = "p",pch = 21,bg = "navy",cex = 1.7,len = 0.2,family = "myFont1")
points(pro.otu.env,pch = 21,bg = "firebrick1",cex = 1.7,col = "firebrick1")
text(0.5,-1,labels = "Procrustes analysis:\n        M2 = 0.8902\n        p-value = 0.331\nMantel test:\n        r = -0.2793\n        p-value = 0.803",family = "myFont1",adj = 0,cex = 1.2)
#绘制procrustes图
dev.off()