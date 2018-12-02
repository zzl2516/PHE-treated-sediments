library(vegan)
library(ape)
library(ggplot2)
library(grid)
library(RColorBrewer)

data <- read.csv("ARGs.abundance.txt", head=TRUE,sep="\t",row.names = 1)
groups <- read.table("group.list.txt",sep = "\t",header = F,colClasses = c("character"))
groups <- as.list(groups)
data <- t(data)
data[is.na(data)] <- 0
data <- vegdist(data)

length=length(unique(as.character(groups$V1)))
times1=length%/%8
res1=length%%8
times2=length%/%5
res2=length%%5
col1=rep(1:8,times1)
col=c(col1,1:res1)
pich1=rep(c(15,21:24,18,18),times2)
pich=c(21:25)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")
Palette <- c("#000000", "#000000", "#000000")
pcoa<- pcoa(data, correction = "none", rn = NULL)
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,groups$V2,groups$V3)
colnames(plotdata) <-c("sample","PC1","PC2","Treatment","Time")
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)

p2<-ggplot(plotdata, aes(PC1, PC2)) +
geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=6)+ 
scale_shape_manual(values=pich,breaks = c("Day 0","Day 7","Day 15","Day 25","Day 40"))+
scale_colour_manual(values=cbbPalette,breaks = c("Control","Low PHE","High PHE"))+
scale_fill_manual(values=cbbPalette,breaks = c("Control","Low PHE","High PHE"))+
labs(title="PCoA - PC1 vs PC2")+ 
xlab(paste("PC1 ( ",pc1,"%"," )",sep=""))+ 
ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
theme(text=element_text(family="Arial",size=18))+
geom_vline(aes(xintercept = 0),linetype="dotted")+
geom_hline(aes(yintercept = 0),linetype="dotted")+
theme(panel.background = element_rect(fill='white', colour='black'),
      panel.grid=element_blank(), 
      axis.title = element_text(color='black',family="Arial",size=18),
      axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
      axis.line = element_line(colour = "black"), 
      axis.title.x=element_text(colour='black', size=18),
      axis.title.y=element_text(colour='black', size=18),
      axis.text=element_text(colour='black',size=18),
      legend.title=element_text(colour = "black",size = 18,face = "bold"),
      legend.text=element_text(family="Arial", size=18),
      legend.key=element_blank(),legend.position = c(0.91,0.72),
      legend.background = element_rect(colour = "black"),legend.key.height=unit(1,"cm"))+
theme(plot.title = element_text(size=22,colour = "black",face = "bold",hjust = 0.5))

cairo_pdf("PCoA12.pdf",height=12,width=15)
p2
png(filename="PCoA12.png",res=600,height=5400,width=7200,type="cairo")
p2
dev.off()

sample1 <- c("C0","C7","","HP7","C15","LP15","HP15","C25","LP25","HP25","C40","LP40","HP40")
sample2 <- c("","","LP7",rep("",10))
p5<-ggplot(plotdata, aes(PC1, PC2)) +
geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=6)+ 
geom_text(aes(label=sample1),size=5,family="Arial",hjust=0.5,vjust=-1)+
geom_text(aes(label=sample2),size=5,family="Arial",hjust=0.5,vjust=2)+
scale_shape_manual(values=pich,breaks = c("Day 0","Day 7","Day 15","Day 25","Day 40"))+
scale_colour_manual(values=cbbPalette,breaks = c("Control","Low PHE","High PHE"))+
scale_fill_manual(values=cbbPalette,breaks = c("Control","Low PHE","High PHE"))+
labs(title="PCoA - PC1 vs PC2")+ 
xlab(paste("PC1 ( ",pc1,"%"," )",sep=""))+ 
ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
theme(text=element_text(family="Arial",size=18))+
geom_vline(aes(xintercept = 0),linetype="dotted")+
geom_hline(aes(yintercept = 0),linetype="dotted")+
theme(panel.background = element_rect(fill='white', colour='black'),
      panel.grid=element_blank(), 
      axis.title = element_text(color='black',family="Arial",size=18),
      axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
      axis.line = element_line(colour = "black"), 
      axis.title.x=element_text(colour='black', size=18),
      axis.title.y=element_text(colour='black', size=18),
      axis.text=element_text(colour='black',size=18),
      legend.title=element_text(colour = "black",size = 18,face = "bold"),
      legend.text=element_text(family="Arial", size=18),
      legend.key=element_blank(),legend.position = c(0.91,0.72),
      legend.background = element_rect(colour = "black"),legend.key.height=unit(1,"cm"))+
theme(plot.title = element_text(size=22,colour = "black",face = "bold",hjust = 0.5))

cairo_pdf("PCoA12-2.pdf",height=12,width=15)
p5
png(filename="PCoA12-2.png",res=600,height=5400,width=7200,type="cairo")
p5
dev.off()