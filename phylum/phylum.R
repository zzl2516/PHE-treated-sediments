args.mges.number <- read.table("ALL.new.percent.phylum.new.txt",header = TRUE,sep = "\t",row.names = 1)
#载入绘图数据

args.mges.number <- as.matrix(args.mges.number)
#调整数据格式
windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体

library(RColorBrewer)
#载入颜色包

#Treatment
png(filename = "phylum.treatment.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(10,5,8,5),family ="myFont1")
#设置绘图区
bar <- barplot(args.mges.number,names.arg = c(rep("",13)),col = brewer.pal(12,"Set3"),border = "black",family = "myFont1",xlim = c(0,15),axes = F,ylim = c(0,100))
#绘制第一张图
axis(side = 2,at = seq(0,100,20),las = 2,line = -1.55,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
#添加y轴
par(xpd = TRUE)
axis(side = 1, at = bar,line = 1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = bar, y = -3,adj = c(1,0.8),labels = colnames(args.mges.number),cex = 2.4, font = 2,srt = 45)
#添加x轴
mtext("Relative abundance (%)",side = 2,line = 2,family = "myFont1",font = 2,cex = 3)

par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(0,1.2,legend = rownames(args.mges.number),fill = brewer.pal(12,"Set3"),bty = "n",ncol = 4,text.width = 0.22,cex = 2.1,text.font = 2)
#添加图例
segments(0.1,-0.15,0.39,-0.15,col = "black",lwd = 5)
segments(0.415,-0.15,0.72,-0.15,col = "black",lwd = 5)
segments(0.74,-0.15,1.035,-0.15,col = "black",lwd = 5)
text(0.245,-0.18,labels = "Control",font = 2,cex = 2.5)
text(0.5565,-0.18,labels = "Low PHE",font = 2,cex = 2.5)
text(0.8875,-0.18,labels = "High PHE",font = 2,cex = 2.5)
arrows(x0 = 0.045,y0 = -0.16,x1 = 0.045,y1 = -0.12,lwd = 4,col = "black")
text(x = 0.045,y = -0.18,labels = "Original soil",font = 2,cex = 2.5)
dev.off()

#Time
args.mges.number <- args.mges.number[,c(1,2,6,10,3,7,11,4,8,12,5,9,13)]
#修改样品顺序
png(filename = "phylum.time.tiff",width = 12000,height = 9000,res = 600,type = "cairo")
par(mar=c(10,5,8,5),family ="myFont1")
#设置绘图区
bar <- barplot(args.mges.number,names.arg = c(rep("",13)),col = brewer.pal(12,"Set3"),border = "black",family = "myFont1",xlim = c(0,15),axes = F,ylim = c(0,100))
#绘制第一张图
axis(side = 2,at = seq(0,100,20),las = 2,line = -1.55,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
#添加y轴
par(xpd = TRUE)
axis(side = 1, at = bar,line = 1,labels = FALSE,family = "myFont1",cex.axis = 2.2,lwd = 3,lwd.ticks = 3,font = 2)
text(x = bar, y = -3,adj = c(1,0.8),labels = colnames(args.mges.number),cex = 2.4, font = 2,srt = 45)
#添加x轴
mtext("Relative abundance (%)",side = 2,line = 2,family = "myFont1",font = 2,cex = 3)

par(xpd=TRUE)
par(new = T)
par(family = "myFont1")
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
legend(0,1.2,legend = rownames(args.mges.number),fill = brewer.pal(12,"Set3"),bty = "n",ncol = 4,text.width = 0.22,cex = 2.1,text.font = 2)
#添加图例
segments(0.095,-0.15,0.315,-0.15,col = "black",lwd = 5)
segments(0.335,-0.15,0.555,-0.15,col = "black",lwd = 5)
segments(0.575,-0.15,0.795,-0.15,col = "black",lwd = 5)
segments(0.815,-0.15,1.035,-0.15,col = "black",lwd = 5)
text(x = 0.205,y = -0.18,labels = "Day 7",font = 2,cex = 2.5)
text(x = 0.445,y = -0.18,labels = "Day 15",font = 2,cex = 2.5)
text(x = 0.685,y = -0.18,labels = "Day 25",font = 2,cex = 2.5)
text(x = 0.925,y = -0.18,labels = "Day 40",font = 2,cex = 2.5)
arrows(x0 = 0.045,y0 = -0.16,x1 = 0.045,y1 = -0.12,lwd = 4,col = "black")
text(x = 0.045,y = -0.18,labels = "Original soil",font = 2,cex = 2.5)
dev.off()