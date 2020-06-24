
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
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
 
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
 
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
 
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
 
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
 
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
 
  x1 <- switch(pos,
    topleft     =x[1] + sw, 
    left        =x[1] + sw,
    bottomleft  =x[1] + sw,
    top         =(x[1] + x[2])/2,
    center      =(x[1] + x[2])/2,
    bottom      =(x[1] + x[2])/2,
    topright    =x[2] - sw,
    right       =x[2] - sw,
    bottomright =x[2] - sw)
 
  y1 <- switch(pos,
    topleft     =y[2] - sh,
    top         =y[2] - sh,
    topright    =y[2] - sh,
    left        =(y[1] + y[2])/2,
    center      =(y[1] + y[2])/2,
    right       =(y[1] + y[2])/2,
    bottomleft  =y[1] + sh,
    bottom      =y[1] + sh,
    bottomright =y[1] + sh)
 
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
 
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}


par(mfrow=c(2,2), mar=c(5, 5, 5, 5))
sma<-read.table("sma_file", header=TRUE, row.names=1)
barplot(t(as.matrix(sma)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="", xlab="",ylim=c(0,0.4), ylab="Proportion",cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=2)
fig_label("A", pos="topleft", cex=2, col="black", font=2)

ir<-read.table("ir_file", header=TRUE, row.names=1)
barplot(t(as.matrix(ir)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="", xlab="",ylim=c(0,0.4), ylab="Proportion",cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=2)
fig_label("B",pos="topleft", cex=2, col="black", font=2)

popn<-read.table("popn_file", header=TRUE, row.names=1)
barplot(t(as.matrix(popn)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="", xlab="",ylim=c(0,0.4), ylab="Proportion",cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=2)
fig_label("C",pos="topleft", cex=2, col="black", font=2)

nswe<-read.table("nswe_file", header=TRUE, row.names=1)
barplot(t(as.matrix(nswe)), beside=TRUE, col=c("black", "blue", "yellow","red"), main="", xlab="",ylim=c(0,0.4), ylab="Proportion",cex.lab=2, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,cex.names=2)
fig_label("D",pos="topleft", cex=2, col="black", font=2)


```
## Figure 5
```

setwd("./balSelStats")
tajDFiles=list.files("./", pattern="*_balSelStats_2020_compareDefenseToGenic.txt")
names=sapply(strsplit(sapply(strsplit(tajDFiles, "_"), "[[", 1), "Ids"), "[[", 1)

pdf("./balSelStats_tajD_03.pdf", height=5, width=8)
par(mfrow=c(1, 1), mar=c(6,5,2,4))


writeFile="summary_tajD-sv_2020-06-29.txt"

write("Organising tajD results", file=writeFile)
write.table(data.frame("pop", "defense_tajD", "genic_tajD", "intergenic_tajD", "pval"), 
            file=writeFile, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)

plot(y=0, x=0, ylim=c(-1.2, +1.2), bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Tajima's D"), cex=1.5), xlab=list(expression(""), cex=1.5), xlim=c(0.0, length(names)*5))
abline(h=0.0)
for (ver in 0:(5*length(names))) {
  segments((ver*5), -1.2, (ver*5), 1.2, col = "black", lwd=0.1, lty = par("lty"), xpd = FALSE)
}
exes = 1
bigX = 0
axis(2, at=c(-1, 1), labels=c("-1", "1"), tick=TRUE, las=1, cex.axis=1.2)

order_pop=c(
  4, 14, 13, 9,           # Africa
  8,                      # Madeira
  6,                      # Relicts
  5, 16, 2, 7, 3, 15, 10, # Europe
  1, 11, 12               # Asia
)
axis(1, at=seq(0, length(names)*5, 5), labels=F, tick=TRUE, las=1, cex.axis=1.2)
popNames=c("Asia", "CEur", "Ger", "Mor_HA", "INR", "IR", "IBC", "Madeira", "Mor_NMA", "NSw", "PopN", "PopY", "Mor_Rif", "Mor_SMA", "SSw", "WEur")
           
axis(1, at=seq(2.5, (length(names)*5)-2.5, 5), labels=popNames[order_pop], tick=F, las=2, cex.axis=0.8)


for (ff in 1:length(tajDFiles)) {
  f = order_pop[ff]
  
  exes = 0
  exes = exes + 2.5
  
  tajDFile <- tajDFiles[[f]]
  tajDFileStr <- strsplit(readLines(tajDFile), "\t")
  # close(IDFile)
  
  ##    Find enrichment[]
  #     For [2]: all, [3]: intergenic, [4]: genic, [5]: defense
  ###
  for (l in 1:length(tajDFileStr)) {
    if (tajDFileStr[[l]][1] == "TajD") {
      enrich_genic = as.double(tajDFileStr[[l]][[2]])
      enrich_defense = as.double(tajDFileStr[[l]][[3]])
      enrich_interg = as.double(tajDFileStr[[l]][[4]])
    }
  }
  
  
  ## Distribution bootstrap
  #
  boot = as.double(tajDFileStr[[8]])
  
  jit=30
  xxs=bigX + exes +0.04 - jit + jitter(rep(jit, length(na.omit(boot))))
  
  col=gray.colors(10, start = 0.6, end = 0.9, gamma = 2.2, 0.01)[[1]]
  
  points(x=xxs, y=na.omit(boot), bg=col, col=NULL, pch=22, cex=0.5)
  
  # Print categories
  segments((bigX + exes + 0.04 - 0.6), enrich_defense, (bigX + exes + 0.04 +0.6), enrich_defense, col = "red", lwd=5, lty = par("lty"), xpd = FALSE)
  segments((bigX + exes + 0.04 - 0.6), enrich_interg, (bigX + exes + 0.04 +0.6), enrich_interg, col = "blue3", lwd=5, lty = par("lty"), xpd = FALSE)
  segments((bigX + exes + 0.04 - 0.6), enrich_genic, (bigX + exes + 0.04 +0.6), enrich_genic, col = "yellow", lwd=5, lty = par("lty"), xpd = FALSE)
  
  pval = ecdf(-boot)(c(-enrich_defense))
  
  bigX = bigX + 5
  
  ###
  #   Write out to table
  ###
  write.table(data.frame(names[[f]], enrich_defense, enrich_genic, enrich_interg, pval), 
              file=writeFile, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
  
}

dev.off()





```
## Figure 6
```

manhattan1<-function (x, chr = "CHR", bp = "BP", p = "P", snp = "SNP", col = c("deepskyblue",
                                                                               "lightskyblue2"), chrlabs = NULL, suggestiveline = -log10(1e-05),
                      genomewideline = -log10(5e-08), highlight1 = NULL, highlight2 = NULL, logp = TRUE,
                      ...)
{
  CHR = BP = P = index = NULL
  if (!(chr %in% names(x)))
    stop(paste("Column", chr, "not found!"))
  if (!(bp %in% names(x)))
    stop(paste("Column", bp, "not found!"))
  if (!(p %in% names(x)))
    stop(paste("Column", p, "not found!"))
  if (!(snp %in% names(x)))
    warning(paste("No SNP column found. OK unless you're trying to highlight."))
  if (!is.numeric(x[[chr]]))
    stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
  if (!is.numeric(x[[bp]]))

    stop(paste(bp, "column should be numeric."))
  if (!is.numeric(x[[p]]))
    stop(paste(p, "column should be numeric."))
  d = data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[p]])
  if (!is.null(x[[snp]]))
    d = transform(d, SNP = x[[snp]])
  d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
  d <- d[order(d$CHR, d$BP), ]
  if (logp) {
    d$logp <- -log10(d$P)
  }
  else {
    d$logp <- d$P
  }
  d$pos = NA
  d$index = NA
  ind = 0
  for (i in unique(d$CHR)) {
    ind = ind + 1
    d[d$CHR == i, ]$index = ind
  }
  nchr = length(unique(d$CHR))
  if (nchr == 1) {
    options(scipen = 999)
    d$pos = d$BP/1e+06
    ticks = floor(length(d$pos))/2 + 1
    xlabel = paste("Chromosome", unique(d$CHR), "position(Mb)")
    labs = ticks
  }
  else {
    lastbase = 0
    ticks = NULL
    for (i in unique(d$index)) {
      if (i == 1) {
        d[d$index == i, ]$pos = d[d$index == i, ]$BP
      }
      else {
        lastbase = lastbase + tail(subset(d, index ==
                                            i - 1)$BP, 1)
        d[d$index == i, ]$pos = d[d$index == i, ]$BP +
          lastbase
      }
      ticks = c(ticks, (min(d[d$CHR == i, ]$pos) + max(d[d$CHR ==
                                                           i, ]$pos))/2 + 1)
    }
    xlabel = "Chromosome"
    labs <- unique(d$CHR)
  }
  xmax = ceiling(max(d$pos) * 1.03)
  xmin = floor(max(d$pos) * -0.03)
  def_args <- list(xaxt = "n", bty = "n", xaxs = "i", yaxs = "i",
                   las = 1, pch = 20, xlim = c(xmin, xmax), ylim = c(0,
                                                                     ceiling(max(d$logp))), xlab = xlabel, ylab = expression(-log[10](p)))
  dotargs <- list(...)
  do.call("plot", c(NA, dotargs, def_args[!names(def_args) %in%
                                            names(dotargs)]))
  if (!is.null(chrlabs)) {
    if (is.character(chrlabs)) {
      if (length(chrlabs) == length(labs)) {
        labs <- chrlabs
      }
      else {
        warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
      }
    }
    else {
      warning("If you're trying to specify chromosome labels, chrlabs must be a character vector")
    }
  }
  if (nchr == 1) {
    axis(1, ...)
  }
  else {
    axis(1, at = ticks, labels = labs, ...)
  }
  col = rep(col, max(d$CHR))
  if (nchr == 1) {
    with(d, points(pos, logp, pch = 20, col = col[1], ...))
  }
  else {
    icol = 1
    for (i in unique(d$index)) {
      with(d[d$index == unique(d$index)[i], ], points(pos,
                                                      logp, col = col[icol], pch = 20, ...))
      icol = icol + 1
    }
  }
  if (suggestiveline)
    abline(h = suggestiveline, col = "dodgerblue")
  if (genomewideline)
    abline(h = genomewideline, col = "red")
  if (!is.null(highlight1)) {
    if (any(!(highlight1 %in% d$SNP)))
      warning("You're trying to highlight1 SNPs that don't exist in your results.")
    d.highlight1 = d[which(d$SNP %in% highlight1), ]
    with(d.highlight1, points(pos, logp, col = "dodgerblue", pch = 20,
                              ...))
  }
  if (!is.null(highlight2)) {
    if (any(!(highlight2 %in% d$SNP)))
      warning("You're trying to highlight2 SNPs that don't exist in your results.")
    d.highlight2 = d[which(d$SNP %in% highlight2), ]
    with(d.highlight2, points(pos, logp, col = "red", pch = 19,
                              ...))
  }
}

fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
 
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
 
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
 
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
 
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
 
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
 
  x1 <- switch(pos,
    topleft     =x[1] + sw, 
    left        =x[1] + sw,
    bottomleft  =x[1] + sw,
    top         =(x[1] + x[2])/2,
    center      =(x[1] + x[2])/2,
    bottom      =(x[1] + x[2])/2,
    topright    =x[2] - sw,
    right       =x[2] - sw,
    bottomright =x[2] - sw)
 
  y1 <- switch(pos,
    topleft     =y[2] - sh,
    top         =y[2] - sh,
    topright    =y[2] - sh,
    left        =(y[1] + y[2])/2,
    center      =(y[1] + y[2])/2,
    right       =(y[1] + y[2])/2,
    bottomleft  =y[1] + sh,
    bottom      =y[1] + sh,
    bottomright =y[1] + sh)
 
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
 
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}
par(mfrow=c(2,2), mar=c(5, 5, 5, 5))
flower=read.table("gemma_assoc_file", header = T)
manhattan1(flower, chr = "chr", bp="ps", p="p_lrt", snp = "rs", genomewideline = -log10(0.05), suggestiveline = -log10(0.05/length(flower$rs)), ylim=c(0,9), main='', cex.main=0.8)
fig_label("A", pos="topleft", cex=2, col="black", font=2)

flower=read.table("gemma_assoc_file", header = T)
manhattan1(flower, chr = "chr", bp="ps", p="p_lrt", snp = "rs", genomewideline = -log10(0.05), suggestiveline = -log10(0.05/length(flower$rs)), ylim=c(0,9), main='', cex.main=0.8)
fig_label("B", pos="topleft", cex=2, col="black", font=2)

flower=read.table("gemma_assoc_file", header = T)
manhattan1(flower, chr = "chr", bp="ps", p="p_lrt", snp = "rs", genomewideline = -log10(0.05), suggestiveline = -log10(0.05/length(flower$rs)), ylim=c(0,9), main='', cex.main=0.8)

fig_label("C", pos="topleft", cex=2, col="black", font=2)

flower=read.table("gemma_assoc_file", header = T)
manhattan1(flower, chr = "chr", bp="ps", p="p_lrt", snp = "rs", genomewideline = -log10(0.05), suggestiveline = -log10(0.05/length(flower$rs)), ylim=c(0,9), main='', cex.main=0.8)
fig_label("D", pos="topleft", cex=2, col="black", font=2)
```
