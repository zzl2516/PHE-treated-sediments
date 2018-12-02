args.abundance <- read.csv("ARGs.MGEs.subtype.abundance.txt",header = TRUE,sep = "\t")
#载入绘图数据

annotation <- args.abundance[,1:2]
annotation <- as.data.frame(annotation)
#定义分类参数
args.abundance <- args.abundance[,-1]
args.abundance <- args.abundance[,-1]
rownames(args.abundance) <- args.abundance[,1]
args.abundance <- args.abundance[,-1]
rownames(annotation) <- row.names(args.abundance)
args.abundance <- as.matrix(args.abundance)
args.abundance <- 6 + log10(args.abundance)
args.abundance[is.infinite(args.abundance)] <- 0
#修改数据格式

library(RColorBrewer)
#导入颜色包
Antibiotics = c(brewer.pal(9,"Set3"))
names(Antibiotics) = c("Aminoglycosides","Beta_Lactamas","FCA","MLSB","Multidrug","Sulfonamides","Tetracyclines","Vancomycin","MGEs")
Mechansims = c(brewer.pal(6,"Set1"))
names(Mechansims) = c("Antibiotic deactivate","Efflux pump","Cellular protection","Unknown","Integrase","Transposase")
ann_colors = list(Antibiotics = Antibiotics,Mechansims = Mechansims)
#定义分类参数的颜色

annotation$`ARGs types`<- as.factor(annotation$Antibiotics)
annotation$Mechansims <- as.factor(annotation$Mechansims)
annotation <- annotation[,-1]
#带解决问题，基因分类不能自定义颜色
windowsFonts(myFont1=windowsFont("Times New Roman"))
#设置绘图字体

library(pheatmap)
#载入绘图包
png(filename = "ARGs.MGEs.subtype.heatmap.time.png",width = 18000,height = 12000,res = 600,type = "cairo")
par(fig = c(0,1,0,1))
plot(rnorm,lty=0,bty='n',xaxt='n',yaxt='n',xlab='',ylab='',xlim=c(-1,1),ylim=c(-1,1))
par(xpd=TRUE)
par(fig = c(0,1,0,1),new = T)
pheatmap(args.abundance,color = colorRampPalette(c("navy","white","firebrick3"))(100),cluster_cols = FALSE,clustering_distance_rows = "euclidean",family = "myFont1",main = "",annotation_row = annotation,fontsize_row = 25,fontsize_col = 30,fontsize = 30,gaps_col = c(3,6,9))
par(fig = c(0,1,0,1))
plot(rnorm,lty=0,bty='n',xaxt='n',yaxt='n',xlab='',ylab='',xlim=c(-1,1),ylim=c(-1,1))
par(xpd=TRUE)
segments(x0 = -1.01,y0 = 1.12,x1 = -0.675,y1 = 1.12,col = "black",lwd = 5)
text(x = -0.8425,y = 1.15,labels = "Day 7",family = "myFont1",font = 2,cex = 3)
segments(x0 = -0.6625,y0 = 1.12,x1 = -0.325,y1 = 1.12,col = "black",lwd = 5)
text(x = -0.49325,y = 1.15,labels = "Day 15",family = "myFont1",font = 2,cex = 3)
segments(x0 = -0.312,y0 = 1.12,x1 = 0.023,y1 = 1.12,col = "black",lwd = 5)
text(x = -0.1445,y = 1.15,labels = "Day 25",family = "myFont1",font = 2,cex = 3)
segments(x0 = 0.0375,y0 = 1.12,x1 = 0.3725,y1 = 1.12,col = "black",lwd = 5)
text(x = 0.205,y = 1.15,labels = "Day 40",family = "myFont1",font = 2,cex = 3)
segments(x0 = 0.67,y0 = 1.1,x1 = 0.67,y1 = -0.09,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = 1.1,x1 = 0.67,y1 = 1.1,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.09,x1 = 0.67,y1 = -0.09,col = "black",lwd = 5)
text(x = 0.7,y = 0.505,labels = "Cluster I",family = "myFont1",font = 2,cex = 3,srt = -90)
segments(x0 = 0.55,y0 = -0.095,x1 = 0.55,y1 = -0.285,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.095,x1 = 0.55,y1 = -0.095,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.285,x1 = 0.55,y1 = -0.285,col = "black",lwd = 5)
text(x = 0.65,y = -0.19,labels = "Cluster II",family = "myFont1",font = 2,cex = 3)
segments(x0 = 0.67,y0 = -0.29,x1 = 0.67,y1 = -0.91,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.29,x1 = 0.67,y1 = -0.29,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.91,x1 = 0.67,y1 = -0.91,col = "black",lwd = 5)
text(x = 0.7,y = -0.6,labels = "Cluster III",family = "myFont1",font = 2,cex = 3,srt = -90)
dev.off()
#保存为18x12

args.abundance <- args.abundance[,c(1,4,7,10,2,5,8,11,3,6,9,12)]

#修改样品顺序
png(filename = "ARGs.MGEs.subtype.heatmap.treatment.png",width = 18000,height = 12000,res = 600,type = "cairo")
par(fig = c(0,1,0,1))
plot(rnorm,lty=0,bty='n',xaxt='n',yaxt='n',xlab='',ylab='',xlim=c(-1,1),ylim=c(-1,1))
par(xpd=TRUE)
par(fig = c(0,1,0,1),new = T)
pheatmap(args.abundance,color = colorRampPalette(c("navy","white","firebrick3"))(100),main = "",cluster_cols = FALSE,clustering_distance_rows = "euclidean",family = "myFont1",annotation_row = annotation,fontsize_row = 25,fontsize_col = 30,fontsize = 30,gaps_col = c(4,8))
par(fig = c(0,1,0,1))
plot(rnorm,lty=0,bty='n',xaxt='n',yaxt='n',xlab='',ylab='',xlim=c(-1,1),ylim=c(-1,1))
par(xpd=TRUE)
segments(x0 = -1.01,y0 = 1.12,x1 = -0.56,y1 = 1.12,col = "black",lwd = 5)
text(x = -0.785,y = 1.15,labels = "Control",family = "myFont1",font = 2,cex = 3)
segments(x0 = -0.545,y0 = 1.12,x1 = -0.095,y1 = 1.12,col = "black",lwd = 5)
text(x = -0.32,y = 1.15,labels = "Low PHE",family = "myFont1",font = 2,cex = 3)
segments(x0 = -0.08,y0 = 1.12,x1 = 0.37,y1 = 1.12,col = "black",lwd = 5)
text(x = 0.145,y = 1.15,labels = "High PHE",family = "myFont1",font = 2,cex = 3)
segments(x0 = 0.67,y0 = 1.1,x1 = 0.67,y1 = -0.09,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = 1.1,x1 = 0.67,y1 = 1.1,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.09,x1 = 0.67,y1 = -0.09,col = "black",lwd = 5)
text(x = 0.7,y = 0.505,labels = "Cluster I",family = "myFont1",font = 2,cex = 3,srt = -90)
segments(x0 = 0.55,y0 = -0.095,x1 = 0.55,y1 = -0.285,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.095,x1 = 0.55,y1 = -0.095,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.285,x1 = 0.55,y1 = -0.285,col = "black",lwd = 5)
text(x = 0.65,y = -0.19,labels = "Cluster II",family = "myFont1",font = 2,cex = 3)
segments(x0 = 0.67,y0 = -0.29,x1 = 0.67,y1 = -0.91,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.29,x1 = 0.67,y1 = -0.29,col = "black",lwd = 5)
segments(x0 = 0.38,y0 = -0.91,x1 = 0.67,y1 = -0.91,col = "black",lwd = 5)
text(x = 0.7,y = -0.6,labels = "Cluster III",family = "myFont1",font = 2,cex = 3,srt = -90)
dev.off()

#绘制图例
pheatmap(args.abundance,color = colorRampPalette(c("navy","white","firebrick3"))(100),cluster_cols = FALSE,clustering_distance_rows = "euclidean",family = "myFont1",annotation_row = annotation,fontsize_row = 4,legend_breaks = c(0,1,2,3,3.6),legend_labels = c("ND","1e-5","1e-4","1e-3","4e-3"),family = "myFont1")
