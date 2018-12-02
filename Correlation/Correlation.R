args.16s.abundance <- read.table("ARGs.16S.abundance.txt",header = TRUE,sep = "\t",row.names = 1)
#载入绘图数据
args.16s.abundance <- t(args.16s.abundance)
args.16s.abundance <- as.data.frame(args.16s.abundance)
args.16s.abundance$ARGs <- log10(args.16s.abundance$ARGs)
#调整数据格式

windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体
cor.test(args.16s.abundance$Total,args.16s.abundance$`16S rRNA`,method = "pearson")
#对横纵坐标进行pearson相关性检验
#Pearson's product-moment correlation

#data:  args.16s.abundance$Total and args.16s.abundance$`16S rRNA`
#t = 3.2811, df = 10, p-value = 0.008273
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.2490321 0.9155861
#sample estimates:
#cor 
#0.7200275

png(filename = "ARGs.16S.correlation.tiff",width = 12000,height = 10000,res = 600,type = "cairo")
par(mar=c(6,9,2,2),family = "myFont1")
plot(`16S rRNA` ~ Total,data = args.16s.abundance,las = 1,pch = 19,col = "magenta2",cex.axis = 2.5,cex.lab = 2.5,cex = 5,xlab = "",ylab = "",family = "myFont1",font = 2)
mtext("Copy number of ARGs per g dry sediments",side = 1,line = 3.5,font = 2,cex = 3,family = "myFont1")
mtext("Copy number of 16S rRNA gene per g dry sediments",side = 2,line = 6.5,family = "myFont1",cex = 3,font = 2)
#绘制散点图
abline(lm(`16S rRNA` ~ Total,data = args.16s.abundance),lwd = 5)
#添加回归趋势线，散点图与回归线均用公式形式绘制，保证坐标的一致性
par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
text(0.9,0.05,labels = "R^2 = 0.72\np-value < 0.01",cex = 3,font = 2,family = "myFont1")
dev.off()
#添加相关性检验结果
#图像保存为12x10