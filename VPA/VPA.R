library(vegan)

args <- read.csv("ARGs.abundance.txt",header = TRUE,sep = "\t",row.names = 1)
args.env <- read.table("pcoa.txt",header = TRUE,row.names = 1,sep = "\t")
args.mges <- read.table("MGEs.txt",header = TRUE,row.names = 1,sep = "\t")
args.env <- args.env[c(2,1,4,3,6,5,8,7),]
args <- t(args)
args.mges <- t(args.mges)
args <- as.data.frame(args)
args.env <- as.data.frame(args.env)
args.mges <- as.data.frame(args.mges)


vpa <- varpart(args, ~., args.mges, data = args.env)
plot(vpa, bg = c("hotpink","skyblue"))