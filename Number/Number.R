args.mges.number <- read.table("ARGs_MGEs_number.txt",header = TRUE,sep = "\t",row.names = 1)
#载入绘图数据

args.mges.number <- as.matrix(args.mges.number)
#调整数据格式
windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体

library(RColorBrewer)
#载入颜色包

#Time
png(filename = "ARGs.MGEs.number.time.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(6.3,5,4,10),family ="myFont1")
#设置绘图区
bar <- barplot(args.mges.number[1:8,],names.arg = c(rep("",13)),col = brewer.pal(8,"Set3"),border = "black",family = "myFont1",xlim = c(0,15),axes = F,ylim = c(0,40))
#绘制第一张图
axis(side = 2,at = seq(0,40,5),las = 2,line = -1.55,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
#添加y轴
par(xpd = TRUE)
par(new = T)
par(mar = c(4,5,4,10),family = "myFont1")
#添加一个新的绘图区
plot(x = bar,y = args.mges.number[9,],type = "p",col = "black",axes = F,xlim = c(0,15),xlab = "",ylab = "",family = "myFont1",ylim = c(0,7),pch = 19,bg = "black",cex = 4)
#绘制第二个坐标
par(xpd = TRUE)
par(new = T)
par(mar = c(4,5,4,10),family = "myFont1")
plot(x = bar,y = args.mges.number[9,],type = "h",col = "black",axes = F,xlim = c(0,15),xlab = "",ylab = "",family = "myFont1",ylim = c(0,7),bty = "o",lwd =5)
#绘制第二个坐标
axis(side = 1, at = bar,line = -1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = bar, y = -0.24,adj = c(1,0.8),labels = colnames(args.mges.number),cex = 2.4, font = 2,srt = 45)
#添加x轴
axis(side = 4,at = c(0:7),line = 4,las = 2,family = "myFont1",cex.axis = 3,lwd = 3,lwd.ticks = 3,font = 2)
#添加第二条y轴
mtext("The detected number of ARGs in all samples",side = 2,line = 2,family = "myFont1",font = 2,cex = 3)
mtext("The detected number of MGEs in all samples",side = 4,line = 8,family = "myFont1",font = 2,cex = 3)

par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(0,1.12,legend = rownames(args.mges.number[1:8,]),fill = brewer.pal(9,"Set3"),bty = "n",ncol = 4,text.width = 0.2,cex = 2.5,text.font = 2)
legend(0.01,1.033,legend = "MGEs",pch = 19,col = "black",bty = "n",cex = 2.5,pt.cex = 3.3,text.font = 2,text.width = 0.2,x.intersp = 1.35)
#添加图例
segments(0.095,0.72,0.315,0.72,col = "black",lwd = 5)
segments(0.335,0.6,0.555,0.6,col = "black",lwd = 5)
segments(0.575,0.47,0.795,0.47,col = "black",lwd = 5)
segments(0.815,0.43,1.035,0.43,col = "black",lwd = 5)
text(x = 0.205,y = 0.74,labels = "Day 7",font = 2,cex = 2.5)
text(x = 0.445,y = 0.62,labels = "Day 15",font = 2,cex = 2.5)
text(x = 0.685,y = 0.49,labels = "Day 25",font = 2,cex = 2.5)
text(x = 0.925,y = 0.45,labels = "Day 40",font = 2,cex = 2.5)
arrows(x0 = 0.1525,y0 = 0.85,x1 = 0.09,y1 = 0.85,lwd = 4,col = "black")
text(x = 0.28,y = 0.85,labels = "Original sediment",font = 2,cex = 2.5)
dev.off()

#Treatment
args.mges.number <- args.mges.number[,c(1,seq(from = 2, to = 11,by = 3),seq(from =3, to = 12, by = 3),seq(from = 4,to = 13, by = 3))]
#修改样品顺序
png(filename = "ARGs.MGEs.number.treatment.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(6.3,5,4,10),family ="myFont1")
#设置绘图区
bar <- barplot(args.mges.number[1:8,],names.arg = c(rep("",13)),col = brewer.pal(8,"Set3"),border = "black",family = "myFont1",xlim = c(0,15),axes = F,ylim = c(0,40))
#绘制第一张图
axis(side = 2,at = seq(0,40,5),las = 2,line = -1.55,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
#添加y轴
par(xpd = TRUE)
par(new = T)
par(mar = c(4,5,4,10),family = "myFont1")
#添加一个新的绘图区
plot(x = bar,y = args.mges.number[9,],type = "p",col = "black",axes = F,xlim = c(0,15),xlab = "",ylab = "",family = "myFont1",ylim = c(0,7),pch = 19,bg = "black",cex = 4)
#绘制第二个坐标
par(xpd = TRUE)
par(new = T)
par(mar = c(4,5,4,10),family = "myFont1")
plot(x = bar,y = args.mges.number[9,],type = "h",col = "black",axes = F,xlim = c(0,15),xlab = "",ylab = "",family = "myFont1",ylim = c(0,7),bty = "o",lwd =5)
#绘制第二个坐标
axis(side = 1, at = bar,line = -1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = bar, y = -0.24,adj = c(1,0.8),labels = colnames(args.mges.number),cex = 2.4, font = 2,srt = 45)
#添加x轴
axis(side = 4,at = c(0:7),line = 4,las = 2,family = "myFont1",cex.axis = 3,lwd = 3,lwd.ticks = 3,font = 2)
#添加第二条y轴
mtext("The detected number of ARGs in all samples",side = 2,line = 2,family = "myFont1",font = 2,cex = 3)
mtext("The detected number of MGEs in all samples",side = 4,line = 8,family = "myFont1",font = 2,cex = 3)

par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(0,1.12,legend = rownames(args.mges.number[1:8,]),fill = brewer.pal(9,"Set3"),bty = "n",ncol = 4,text.width = 0.2,cex = 2.5,text.font = 2)
legend(0.01,1.033,legend = "MGEs",pch = 19,col = "black",bty = "n",cex = 2.5,pt.cex = 3.3,text.font = 2,text.width = 0.2,x.intersp = 1.35)
#添加图例
segments(0.1,0.48,0.39,0.48,col = "black",lwd = 5)
segments(0.415,0.62,0.72,0.62,col = "black",lwd = 5)
segments(0.74,0.73,1.035,0.73,col = "black",lwd = 5)
text(0.245,0.5,labels = "Control",font = 2,cex = 2.5)
text(0.5565,0.64,labels = "Low PHE",font = 2,cex = 2.5)
text(0.8875,0.75,labels = "High PHE",font = 2,cex = 2.5)
arrows(x0 = 0.1525,y0 = 0.85,x1 = 0.09,y1 = 0.85,lwd = 4,col = "black")
text(x = 0.28,y = 0.85,labels = "Original sediment",font = 2,cex = 2.5)
dev.off()
