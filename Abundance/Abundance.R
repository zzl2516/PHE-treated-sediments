args.mges.number <- read.table("ARGs.MGEs.abundance.txt",header = TRUE,sep = "\t",row.names = 1)
#载入绘图数据
args.16s.abundance <- read.table("ARGs.16S.abundance.txt",header = TRUE,sep = "\t",row.names = 1)
args.mges.number <- as.matrix(args.mges.number)
args.16s.abundance <- as.matrix(args.16s.abundance)
#调整数据格式
windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体
args.mges.number <- args.mges.number[,c(1,3,5,7,8,10,12,14,15,17,19,21,22,24,26)]
library(RColorBrewer)
#载入颜色包

#Time
png(filename = "ARGs.MGEs.abundance.time.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(6.3,10,4,10),family ="myFont1")
#设置绘图区
bar1 <- barplot(args.mges.number[1:8,],names.arg = c(rep("",15)),space = 0.1,col = brewer.pal(8,"Set3"),border = "black",family = "myFont1",xlim = c(0,17),axes = F,ylim = c(0,0.015))
#绘制第一张图
axis(side = 2,at = seq(0,0.015,0.003),las = 2,line = -1.55,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2,cex = 3)
#添加y轴
par(xpd = TRUE)
par(new = T)
par(mar = c(4,10,4,10),family = "myFont1")
#添加一个新的绘图区
plot(x = c(0.6,1.7,2.8,5.0,6.1,7.2,9.4,10.5,11.6,13.8,14.9,16.0),y = args.16s.abundance[1,],type = "b",col = "red",axes = F,xlim = c(0,17),xlab = "",ylab = "",family = "myFont1",ylim = c(0,12),pch = 19,bg = "red",cex = 4,lwd = 5)
#绘制第二个坐标
par(xpd = TRUE)
par(new = T)
par(mar = c(4,10,4,10),family = "myFont1")
plot(x = c(0.6,1.7,2.8,5.0,6.1,7.2,9.4,10.5,11.6,13.8,14.9,16.0),y = args.16s.abundance[2,],type = "b",col = "black",axes = F,xlim = c(0,17),xlab = "",ylab = "",family = "myFont1",ylim = c(0,12),pch = 19,bg = "black",cex = 4,lwd = 5)
#绘制第二个坐标
axis(side = 1, at = c(0.6,1.7,2.8,5.0,6.1,7.2,9.4,10.5,11.6,13.8,14.9,16.0),line = -1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = c(0.6,1.7,2.8,5.0,6.1,7.2,9.4,10.5,11.6,13.8,14.9,16.0), y = -0.42,adj = c(1,0.8),labels = colnames(args.16s.abundance),cex = 2.4, font = 2,srt = 45)
#添加x轴
axis(side = 4,at = c(0,3,6,9,12),line = -3,las = 2,family = "myFont1",cex.axis = 3,lwd = 3,lwd.ticks = 3,font = 2)
#添加第二条y轴
mtext("The relative abundance of ARGs (copies per 16S rRNA gene)",side = 2,line = 4.8,family = "myFont1",font = 2,cex = 3)
mtext("The absolute abundance of ARGs \n(logarithm-transformed copies per g dry sediment)",side = 4,line = 4,family = "myFont1",font = 2,cex = 3)
par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(-0.02,1.1,legend = rownames(args.mges.number[1:8,]),fill = brewer.pal(8,"Set3"),bty = "n",ncol = 4,text.width = 0.2,cex = 2.5,text.font = 2)
legend(0.01,0.6,legend = c("Total ARGs","16S rRNA gene"),pch = 19,col = c("red","black"),bty = "n",cex = 2.5,pt.cex = 3.3,text.font = 2,text.width = 0.2,x.intersp = 1.35,lty = 1,lwd = 5)
#添加图例
dev.off()

