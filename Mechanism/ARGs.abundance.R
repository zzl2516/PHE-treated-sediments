args.mges.number <- read.table("ARGs.abundance.txt",header = TRUE,sep = "\t",row.names = 1)
#载入绘图数据

args.mges.number <- as.matrix(args.mges.number)
#调整数据格式
windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体

library(RColorBrewer)
#载入颜色包

#Time
png(filename = "ARGs.abundance.mechanism.time.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(6.3,5,4,10),family ="myFont1")
#设置绘图区
bar <- barplot(args.mges.number,names.arg = c(rep("",13)),col = brewer.pal(8,"Set3"),border = "black",family = "myFont1",xlim = c(0,15),axes = F,ylim = c(0,0.01))
#绘制第一张图
axis(side = 2,at = seq(0,0.01,0.002),las = 2,line = -8,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
#添加y轴
par(xpd = TRUE)
par(new = T)
par(mar = c(4,5,4,10),family = "myFont1")
axis(side = 1, at = c(1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7,13.9,15.1),line = -1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = c(1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7,13.9,15.1), y = -0.0004,adj = c(1,0.8),labels = colnames(args.mges.number[,2:13]),cex = 2.4, font = 2,srt = 45)
#添加x轴
mtext("The detected number of ARGs in all samples",side = 2,line = -1,family = "myFont1",font = 2,cex = 3)
par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(0.06,1.08,legend = rownames(args.mges.number),fill = brewer.pal(9,"Set3"),bty = "n",ncol = 2,text.width = 0.3,cex = 2.5,text.font = 2)
#添加图例
segments(0.095,0.4,0.315,0.4,col = "black",lwd = 5)
segments(0.335,0.9,0.555,0.9,col = "black",lwd = 5)
segments(0.575,0.6,0.795,0.6,col = "black",lwd = 5)
segments(0.815,0.26,1.035,0.26,col = "black",lwd = 5)
text(x = 0.205,y = 0.42,labels = "Day 7",font = 2,cex = 2.5)
text(x = 0.445,y = 0.92,labels = "Day 15",font = 2,cex = 2.5)
text(x = 0.685,y = 0.62,labels = "Day 25",font = 2,cex = 2.5)
text(x = 0.925,y = 0.28,labels = "Day 40",font = 2,cex = 2.5)
dev.off()

#Treatment
args.mges.number <- args.mges.number[,c(1,seq(from = 2, to = 11,by = 3),seq(from =3, to = 12, by = 3),seq(from = 4,to = 13, by = 3))]
#修改样品顺序
png(filename = "ARGs.abundance.mechanism.treatment.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(6.3,5,4,10),family ="myFont1")
#设置绘图区
bar <- barplot(args.mges.number,names.arg = c(rep("",13)),col = brewer.pal(8,"Set3"),border = "black",family = "myFont1",xlim = c(0,15),axes = F,ylim = c(0,0.01))
#绘制第一张图
axis(side = 2,at = seq(0,0.01,0.002),las = 2,line = -8,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
#添加y轴
par(xpd = TRUE)
par(new = T)
par(mar = c(4,5,4,10),family = "myFont1")
axis(side = 1, at = c(1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7,13.9,15.1),line = -1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = c(1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7,13.9,15.1), y = -0.0004,adj = c(1,0.8),labels = colnames(args.mges.number[,2:13]),cex = 2.4, font = 2,srt = 45)
#添加x轴
mtext("The detected number of ARGs in all samples",side = 2,line = -1,family = "myFont1",font = 2,cex = 3)
par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(0.06,1.08,legend = rownames(args.mges.number),fill = brewer.pal(9,"Set3"),bty = "n",ncol = 2,text.width = 0.3,cex = 2.5,text.font = 2)
#添加图例
segments(0.1,0.22,0.39,0.22,col = "black",lwd = 5)
segments(0.415,0.9,0.72,0.9,col = "black",lwd = 5)
segments(0.74,0.6,1.035,0.6,col = "black",lwd = 5)
text(0.245,0.24,labels = "Control",font = 2,cex = 2.5)
text(0.5565,0.92,labels = "Low PHE",font = 2,cex = 2.5)
text(0.8875,0.62,labels = "High PHE",font = 2,cex = 2.5)
dev.off()
