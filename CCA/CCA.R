library(vegan)
library(ggplot2)
library(grid)


community<-read.csv("ARGs.abundance.txt", head=T, row.names=1,sep="\t")
envdata<-read.table("environment.txt", header=TRUE,row.names=1,sep="\t")
envdata <- t(envdata)
envdata <- scale(envdata,center = T,scale = T)
groups = read.table("group.list.txt", head=F,colClasses=c("character","character"),sep = "\t")
groups <- as.list(groups)
community <- t(community)
community <- as.data.frame(community)
community <- scale(community,center = F,scale = T)

length=length(unique(as.character(groups$V1)))
times1=length%/%8
res1=length%%8
times2=length%/%5
res2=length%%5
col1=rep(1:8,times1)
col=c(col1,1:res1)
pich1=rep(c(15:18,20,7:14,0:6),times2)
pich=c(22:25)
cbbPalette <- c("#56B4E9", "#009E73")
Palette <- c("#000000", "#000000")                      

#DCA
dca=decorana(veg = community)
dca1=max(dca$rproj[,1])
dca2=max(dca$rproj[,2])
dca3=max(dca$rproj[,3])
dca4=max(dca$rproj[,4])
AL=data.frame(DCA1=c(dca1),DCA2=c(dca2),DCA3=c(dca3),DCA4=c(dca4))
rownames(AL)=c("Axislength")        
write.csv(AL,file="dca.csv")

#CCA    
cca<-cca(community,envdata)
ccascore=scores(cca)
write.csv(ccascore$sites,file="cca.sample.csv")
write.csv(cca$CCA$biplot,file="cca.env.csv")
write.csv(ccascore$species,file="cca.sp.csv")
                      
CCAE =cca$CCA$biplot[,1:2]
CCASP=ccascore$species[,1:2]
CCAS1 = ccascore$sites[,1]
CCAS2 = ccascore$sites[,2]
CCAE = as.data.frame(CCAE)
CCASP = as.data.frame(CCASP)
                      

plotdata = data.frame(rownames(ccascore$sites),CCAS1,CCAS2,groups$V2,groups$V3)
colnames(plotdata)=c("sample","CCAS1","CCAS2","Treatment","Time")
plotdata$CCAS1=as.numeric(as.vector(plotdata$CCAS1))
plotdata$CCAS2=as.numeric(as.vector(plotdata$CCAS2))
pc1 =round(cca$CCA$eig[1]/sum(cca$CCA$eig)*100,2)
pc2 =round(cca$CCA$eig[2]/sum(cca$CCA$eig)*100,2)
                      
                      
#CCA plot
sample1 <- c("","Bacteroidetes","Chloroflexi","Planctomycetes","Bacteria_unclassified","","")
sample2 <- c("Proteobacteria",rep("",4),"MGEs","PHE")
plot_CCA1<-ggplot(plotdata, aes(CCAS1, CCAS2)) +
geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=6)+ 
scale_shape_manual(values=pich,breaks = c("Day 7","Day 15","Day 25","Day 40"))+
scale_colour_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
scale_fill_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
labs(title="CCA Plot") + 
xlab(paste("CCA1 ( ",pc1,"%"," )",sep="")) + 
ylab(paste("CCA2 ( ",pc2,"%"," )",sep=""))+
scale_y_continuous(limits = c(-1.4,2.1))+
geom_segment(data = CCAE,x=0,y=0, aes(x=0,y=0,xend = CCAE[,1], yend = CCAE[,2]),
colour="purple",size=1,arrow=arrow(angle=25, length=unit(0.25, "cm")))+
geom_text(data=CCAE, aes(x=CCAE[,1], y=CCAE[,2], label=sample1), family="Arial", size=6, colour="purple",hjust = (1 - 2 * sign(CCAE[ ,1])) / 3,angle = (180/pi) * atan(CCAE[ ,2]/CCAE[ ,1]))+
geom_text(data=CCAE, aes(x=CCAE[,1], y=CCAE[,2], label=sample2), family="Arial", size=6, colour="purple",hjust = 0,angle = (180/pi) * atan(CCAE[ ,2]/CCAE[ ,1]),vjust = 0.5)+
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
        legend.title=element_text(colour = "black",size = 16,face = "bold"),
        legend.text=element_text(family="Arial", size=16),
        legend.key=element_blank(),legend.position = c(0.08,0.78),
        legend.background = element_rect(colour = "black"),legend.key.height=unit(1,"cm"))+
  theme(plot.title = element_text(size=22,colour = "black",face = "bold",hjust = 0.5))       
cairo_pdf("CCA_sample_env.pdf",height=12,width=15)
plot_CCA1
png(filename="CCA_sample_env.png",res=600,height=5400,width=7200,type="cairo")
plot_CCA1
dev.off()	
                      
sample3 <- c("LP7","HP7","LP15","","LP25","HP25","LP40","HP40")
sample4 <- c(rep("",3),"HP15",rep("",4))
  plot_CCA2<-ggplot(plotdata, aes(CCAS1, CCAS2)) +
  geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=6)+ 
    geom_text(aes(label=sample3),size=5,family="Arial",hjust=0.5,vjust=-1)+
    geom_text(aes(label=sample4),size=5,family="Arial",hjust=0.5,vjust=2)+
    scale_shape_manual(values=pich,breaks = c("Day 7","Day 15","Day 25","Day 40"))+
  scale_colour_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
  scale_fill_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
  labs(title="CCA Plot") + 
  xlab(paste("CCA1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("CCA2 ( ",pc2,"%"," )",sep=""))+
  scale_y_continuous(limits = c(-1.4,2.1))+
  geom_segment(data = CCAE,x=0,y=0, aes(x=0,y=0,xend = CCAE[,1], yend = CCAE[,2]),
               colour="purple",size=1,arrow=arrow(angle=25, length=unit(0.25, "cm")))+
  geom_text(data=CCAE, aes(x=CCAE[,1], y=CCAE[,2], label=sample1), family="Arial", size=6, colour="purple",hjust = (1 - 2 * sign(CCAE[ ,1])) / 3,angle = (180/pi) * atan(CCAE[ ,2]/CCAE[ ,1]))+
  geom_text(data=CCAE, aes(x=CCAE[,1], y=CCAE[,2], label=sample2), family="Arial", size=6, colour="purple",hjust = 0,angle = (180/pi) * atan(CCAE[ ,2]/CCAE[ ,1]),vjust = 0.5)+
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
        legend.title=element_text(colour = "black",size = 16,face = "bold"),
        legend.text=element_text(family="Arial", size=16),
        legend.key=element_blank(),legend.position = c(0.08,0.78),
        legend.background = element_rect(colour = "black"),legend.key.height=unit(1,"cm"))+
  theme(plot.title = element_text(size=22,colour = "black",face = "bold",hjust = 0.5))       
cairo_pdf("CCA_sample_env2.pdf",height=12,width=15)
plot_CCA2
png(filename="CCA_sample_env2.png",res=600,height=5400,width=7200,type="cairo")
plot_CCA2
dev.off()  
                      
                      
#Permutation test       
envfit=envfit(cca,envdata,permu=2000)  
r=as.matrix(envfit$vectors$r) 
p=as.matrix(envfit$vectors$pvals)
colnames(r)="r2" 
colnames(p)="Pr(>r)" 
env=cbind(envfit$vectors$arrows,r,p) 
write.csv(as.data.frame(env),file="ccaenvfit.csv")

#RDA		
rda<-rda(community,envdata)
rdascore<-scores(rda)
write.csv(rdascore$sites,file="rda.sample.csv")
write.csv(rda$CCA$biplot,file="rda.env.csv")
write.csv(rdascore$species,file="rda.sp.csv")
RDAE =rda$CCA$biplot[,1:2]
RDASP=rdascore$species[,1:2]
RDAS1 = rdascore$sites[,1]
RDAS2 = rdascore$sites[,2]
RDAE = as.data.frame(RDAE)
RDASP = as.data.frame(RDASP)


plotdata = data.frame(rownames(ccascore$sites),RDAS1,RDAS2,groups$V2,groups$V3)
colnames(plotdata)=c("sample","RDAS1","RDAS2","Treatment","Time")
plotdata$RDAS1=as.numeric(as.vector(plotdata$RDAS1))
plotdata$RDAS2=as.numeric(as.vector(plotdata$RDAS2))
pc1 =round(rda$CCA$eig[1]/sum(rda$CCA$eig)*100,2)
pc2 =round(rda$CCA$eig[2]/sum(rda$CCA$eig)*100,2)

#RDA plot        
plot_RDA1<-ggplot(plotdata, aes(RDAS1, RDAS2)) +
  geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=6)+ 
  scale_shape_manual(values=pich,breaks = c("Day 7","Day 15","Day 25","Day 40"))+
  scale_colour_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
  scale_fill_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
  labs(title="CCA Plot") + 
  xlab(paste("CCA1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("CCA2 ( ",pc2,"%"," )",sep=""))+
  geom_segment(data = CCAE,x=0,y=0, aes(x=0,y=0,xend = CCAE[,1], yend = CCAE[,2]),
               colour="purple",size=1,arrow=arrow(angle=25, length=unit(0.25, "cm")))+
  geom_text(data=CCAE, aes(x=CCAE[,1], y=CCAE[,2], label=rownames(CCAE)), family="Arial", size=6, colour="purple",hjust = (1 - 2 * sign(CCAE[ ,1])) / 3,angle = (180/pi) * atan(CCAE[ ,2]/CCAE[ ,1]))+
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
        legend.title=element_text(colour = "black",size = 16,face = "bold"),
        legend.text=element_text(family="Arial", size=16),
        legend.key=element_blank(),legend.position = c(0.93,0.26),
        legend.background = element_rect(colour = "black"),legend.key.height=unit(1,"cm"))+
  theme(plot.title = element_text(size=22,colour = "black",face = "bold",hjust = 0.5))       
cairo_pdf("RDA_sample_env.pdf",height=12,width=15)
plot_RDA1
png(filename="RDA_sample_env.png",res=600,height=5400,width=7200,type="cairo")
plot_RDA1
dev.off()	


plot_RDA2<-ggplot(plotdata, aes(RDAS1, RDAS2)) +
  geom_text(aes(label=sample),size=5,family="Arial",hjust=0.5,vjust=-1)+
  geom_point(aes(shape=Time,fill=Treatment,colour=Treatment),size=6)+ 
  scale_shape_manual(values=pich,breaks = c("Day 7","Day 15","Day 25","Day 40"))+
  scale_colour_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
  scale_fill_manual(values=cbbPalette,breaks = c("Low PHE","High PHE"))+
  labs(title="CCA Plot") + 
  xlab(paste("CCA1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("CCA2 ( ",pc2,"%"," )",sep=""))+
  geom_segment(data = CCAE,x=0,y=0, aes(x=0,y=0,xend = CCAE[,1], yend = CCAE[,2]),
               colour="purple",size=1,arrow=arrow(angle=25, length=unit(0.25, "cm")))+
  geom_text(data=CCAE, aes(x=CCAE[,1], y=CCAE[,2], label=rownames(CCAE)), 
            family="Arial", size=6, colour="purple",
            hjust = (1 - 2 * sign(CCAE[ ,1])) / 3,angle = (180/pi) * atan(CCAE[ ,2]/CCAE[ ,1]))+
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
        legend.title=element_text(colour = "black",size = 16,face = "bold"),
        legend.text=element_text(family="Arial", size=16),
        legend.key=element_blank(),legend.position = c(0.93,0.26),
        legend.background = element_rect(colour = "black"),legend.key.height=unit(1,"cm"))+
  theme(plot.title = element_text(size=22,colour = "black",face = "bold",hjust = 0.5))                      
cairo_pdf("RDA_sample_env2.pdf",height=12,width=15)
plot_RDA2
png(filename="RDA_sample_env2.png",res=600,height=5400,width=7200,type="cairo")
plot_RDA2
dev.off()          


#Permutation test 
envfit=envfit(rda,envdata,permu=2000)
r=as.matrix(envfit$vectors$r)
p=as.matrix(envfit$vectors$pvals)
colnames(r)="r2"
colnames(p)="Pr(>r)"
env=cbind(envfit$vectors$arrows,r,p)
write.csv(as.data.frame(env),file="rdaenvfit.csv")  