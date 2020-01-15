## SVS_A.thaliana_II
All scripts related with SVs Paper

## Figure 1
##### Requirements
ggplot2, reshape2
```
dat <- data.frame(Type = c("LumpyExpress-D","Delly-D","Pindel-D","LumpyExpress-DUP","Delly-DUP","Pindel-DUP","LumpyExpress-INV","Delly-INV","Pindel-INV"),Sensitivity_PB = c(0.62,0.64,0.24,0.28,0.40,0.18,0.05,0.28,0.07), Precision_PB=c("0.79","0.72","0.44",0.25,0.23,0.13,0.29,0.34,0.42), Sensitivity_Sim = c(0.96,0.95,0.44,0.96,0.97,0.87,0.95,0.97,0.93), Precision_Sim=c(0.95,0.97,0.05,0.98,0.99,0.98,1,0.5,0.98))
dat$Sensitivity_Sim = as.numeric(as.character(dat$Sensitivity_Sim))
dat$Precision_Sim = as.numeric(as.character(dat$Precision_Sim)) 
dat$Sensitivity_PB = as.numeric(as.character(dat$Sensitivity_PB))
dat$Precision_PB = as.numeric(as.character(dat$Precision_PB))
d <- melt(dat, id.vars="Type")
ggplot(d, aes(Type,value, col=variable,shape=variable)) + geom_point(size=6) + scale_x_discrete(limits=c("LumpyExpress-D","Delly-D","Pindel-D","LumpyExpress-DUP","Delly-DUP","Pindel-DUP","LumpyExpress-INV","Delly-INV","Pindel-INV"))+ylim(0,1)+ labs(title = "Performance Comparison")+theme(plot.title = element_text(hjust = 0.5,size=20), axis.title.x = element_text(size = 16) )+  labs(x = "",y = "",colour = "",shape="") + theme(legend.text = element_text(size=13))+theme(axis.text = element_text(size = 15))+scale_color_manual(values = c("#CC79A7", "#CC79A7","#56B4E9", "#56B4E9"))+scale_shape_manual(values = c(18, 11, 18,11))
```
## Figure 2
##### Requirements
rworldmap
```
newmap <- getMap(resolution = "low")
plot(newmap, asp = 1)
table <- read.table("file",header=T)
points(table$longitude, table$latitude, col = "red", cex = .3)
```
## Figure 3
##### Requirements
ape
```
mydata <- read.table("file",row.names=2, sep="\t")
names <- mydata[,1]
log.ir <- mydata[, 3:ncol(mydata)]
d <- dist(mydata, method = "euclidean")
trw <- nj(d)
trw$tip.label <- c(as.character(names))
plot(trw, "unrooted",cex=0.6,rotate=0)

plot(trw ,"unrooted",cex=0.6,rotate=280, tip.col=c("#D52EED","#763CFF","#8F30FE","#FFFF00","#67029E","#5124F1","#F1A916","#008A32","#9FD627","#1543F1","#FF4534", "#EA32E0","#FDD42E","#336DFF","#FE7C2E",
"#940107","#2E9CFD")[names],show.tip.label=F,main="EURASIA")

tiplabels(pch = c(18,18,18,19,17,18,19,18,17,18,19,17,19,18,19,19,18)[names], col = c("#D52EED","#763CFF","#8F30FE","#FFFF00","#67029E","#5124F1","#F1A916","#008A32","#9FD627","#1543F1","#FF4534", "#EA32E0","#FDD42E","#336DFF","#FE7C2E","#940107","#2E9CFD")[names], adj = 0.5, cex = 2)

legend("topleft", col = c("#D52EED","#763CFF","#8F30FE","#FFFF00","#67029E","#5124F1","#F1A916","#008A32","#9FD627","#1543F1","#FF4534", "#EA32E0","#FDD42E","#336DFF","#FE7C2E",
"#940107","#2E9CFD"),legend=c("C. Asia","C. Europe","Caucasus", "CPV","Etna-2","Germany","High Atlas","Iberia","Iberian Relict","N. Sweden","N. Middle Atlas","Qar-8a","Rif","S. Sweden","S. MiddleAtlas","Tanzania","Western Europe"),  pch=c(18,18,18,19,17,18,19,18,17,18,19,17,19,18,19,19,18), cex=1.5)
```
## Figure 4
```
dat <- data.frame(Type = c("WholeGenome","Intergenic","Genic"), NumberOfDeletions = c(124905,87627,37278),Proportions = c(0.3,0.52,0.14))

par(mfrow=c(1,2), mar=c(5, 5, 5, 5))

barplot(dat$NumberOfDeletions, main="Number of Deletions", xlab="Deletion Type", ylab="Counts", ylim=c(0,140000),names.arg = c("Whole Genome", "Intergenic", "Genic"),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=1.5)

barplot(dat$Proportions, main="Proportion of Deletions", xlab="Proportion", ylab="Deletion Type",names.arg = c("Whole Genome", "Intergenic", "Genic"),horiz=TRUE,space = c(0.5, 0),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=1.5,xlim=c(0,0.6))
```
## Figure 5
```
par(mfrow=c(2,2), mar=c(5, 5, 5, 5))
asia<-read.table("file", header=TRUE, row.names=1)
ce<-read.table("file", header=TRUE, row.names=1)
ha<-read.table("file", header=TRUE, row.names=1)
ir<-read.table("file", header=TRUE, row.names=1)


barplot(t(as.matrix(asia)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="Asia", xlab="Allele Frequency",ylim=c(0,0.4), ylab="Proportion",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=1.5)
legend("topright", col = c("black","blue","yellow","red"),legend=c("Whole Genome","Intergenic","Genic","Defense"),  pch=c(15,15,15,15), cex=1.5)
barplot(t(as.matrix(ir)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="Iberian Relicts", xlab="Allele Frequency",ylim=c(0,0.4), ylab="Proportion",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=1.5)
barplot(t(as.matrix(ce)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="Central Europe", xlab="Allele Frequency",ylim=c(0,0.4), ylab="Proportion",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=1.5)
barplot(t(as.matrix(ha)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="High Atlas", xlab="Allele Frequency",ylim=c(0,0.4), ylab="Proportion",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=1.5)
```
