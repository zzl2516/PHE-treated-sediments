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

mantel(otu.dist,env.dist)
mantel(otu.dist,env.dist,method = "spearman")
#手动保存检验结果

  


