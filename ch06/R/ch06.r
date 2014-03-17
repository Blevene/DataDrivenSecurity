# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/book/ch06")
# make sure the packages for this chapter
# are installed, install if necessary
pkg <- c("ggplot2", "scales", "grid", 
         "gridExtra", "gdata", "RColorBrewer", 
         "portfolio")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)  
}

# Figure 6-1 #########################################################
# requires package: ggplot2, grid
library(ggplot2)
library(grid)

label <- paste("V3JpdGluZyBhIGJvb2sgaXMgaGFyZCB3b3JrLCBidXQgb25lIHNpZ",
               "GUgcGVyayBpcyB3ZSBnZXQgdG8gaW5qZWN0IGVhc3RlciBlZ2dzIG",
               "xpa2UgdGhpcy4gIElmIHlvdSd2ZSBmb3VuZCB0aGlzLCBzZW5kIHV",
               "zIGEgbWVzc2FnZSBvbiB0d2l0dGVyIChAaHJicm1zdHIgYW5kIEBq",
               "YXlqYWNvYnMpIHNheWluZyAiSGFwcHkgRWFzdGVyIiE=", sep="")
label <- unlist(strsplit(label, NULL))
x <- rep(seq(length(label)/4), 4)
y <- rep(c(4:1), each=length(label)/4)
set.seed(1492)
# add some "jitter" to the text
x <- rnorm(length(x), mean=x, sd=0.02)
y <- rnorm(length(y), mean=y, sd=0.02)
color <- rep("gray80", length(label))
color2 <- color
color2[which(label=="X")] <- "#AA0000"
shape=rep(16, length(label))
gdota <- data.frame(x, y, shape, color, color2, label=label)
# Fig 6.1
first <- ggplot(gdota, aes(x, y, label=label))
first <- first + geom_text(size=8, color=color)
first <- first + ylim(0.5, 4.5) + xlab("") + ylab("")
first <- first + theme(
  axis.text = element_blank(),
  panel.background = element_blank(),
  panel.border =  element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = unit(c(0.5,0,0,0), "lines"))
print(first)

# Figure 6-2 #########################################################
# requires package: ggplot2, grid
# requires object: gdata (fig 6-1)
second <- ggplot(gdota, aes(x, y, label=label))
second <- second + geom_text(size=8, color=color2)
second <- second + ylim(0.5, 4.5) + xlab("") + ylab("")
second <- second + theme(
  axis.text = element_blank(),
  panel.background = element_blank(),
  panel.border =  element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = unit(c(0.5,0,0,0), "lines"))
print(second)


# Figure 6-3 #########################################################
# requires package: ggplot2, grid, gridExtra
library(gridExtra)

s.size <- 16
set.seed(1486)
x <- rnorm(s.size, mean=seq(sqrt(s.size)), sd=0.2)
y <- rnorm(s.size, mean=rep(seq(sqrt(s.size)), each=sqrt(s.size)), sd=0.2)
shape <- rep(15, s.size)
shape[sample(seq(s.size), 3)] <- 16
shape2 <- ifelse(shape==16, 4, 21)
color <- ifelse(shape==16, "red", "#9999FF")
intense <- ifelse(shape==16, "black", "gray80")
sz <- ifelse(shape==16, 12, 6)
shape3 <- ifelse(shape==16, 15, 16)
gdata <- data.frame(x, y, shape, shape2, shape3, color, intense, sz)

gsize <- 6
# set up a base ggplot here 
base <- ggplot(gdata, aes(x, y))
base <- base + xlim(0.5, 4.5) + ylim(0.5,4.5) 
base <- base + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     panel.background = element_blank(),
                     panel.border =  element_rect(fill=NA, colour = "gray50", size=0.5),
                     panel.grid = element_blank(),
                     axis.ticks = element_blank(),
                     panel.margin = unit(0, "lines"),
                     plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

# now replicate it for the individual plots
aa <- base + geom_point(size=gsize, shape=shape2, color="black")
aa <- aa + ggtitle("Shape (a)")

bb <- base + geom_point(size=gsize, shape=shape3, color="black")
bb <- bb + ggtitle("Shape (b)")

cc <- base + geom_point(size=sz)
cc <- cc + ggtitle("Size (c)")

dd <- base + geom_point(size=gsize, color=intense)
dd <- dd + ggtitle("Intensity (d)")

ee <- base + geom_point(size=gsize, color=color)
ee <- ee + ggtitle("Hue (e)")

datums <- gdata[which(shape==16), ]
ff <- base + geom_point(size=5)
ff <- ff + geom_point(data=datums, size=12, shape=21)
ff <- ff + ggtitle("Enclosure (f)")
grid.arrange(aa,bb,cc,dd,ee,ff, ncol=3, clip=T)

# Figure 6-4 #########################################################
# requires package: ggplot2, grid, gridExtra
set.seed(3)
ksize <- 15
my.sd <- 0.6
x <- c(rnorm(ksize, mean=5, sd=my.sd), rnorm(ksize, mean=4, sd=my.sd))
y <- c(rnorm(ksize, mean=4, sd=my.sd), rnorm(ksize, mean=5, sd=my.sd))
clust <- rep(c("#FF0000", "#000066"), each=ksize)
shp <- rep(22, ksize*2)
shp[sample(seq(ksize*2), ksize)] <- 21
dfi <- data.frame(x, y, clust, shp)
sz <- ifelse(shp==22, 5, 4)
temp.matrix <- matrix(c(x,y), ncol=2)
cl <- kmeans(temp.matrix, 3)
cls=factor(cl$cluster, labels=c("#8DA0CB" ,"#FC8D62", "#66C2A5"))

base <- ggplot(dfi, aes(x,y))
base <- base + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     panel.background = element_blank(),
                     panel.border =  element_rect(fill=NA, colour = "gray50", size=0.5),
                     panel.grid = element_blank(),
                     axis.ticks = element_blank(),
                     panel.margin = unit(0, "lines"),
                     plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

aa <- base + geom_point(size=4, shape=21, color="gray40", fill=as.character(cls))
aa <- aa + ggtitle("Color (a)")
bb <- base + geom_point(size=sz, shape=shp, color="gray40", fill="gray80", show_guide=F)
bb <- bb + ggtitle("Shape (b)")
cc <- base + geom_point(size=sz, shape=shp, color="gray40", fill=as.character(cls), show_guide=F)
cc <- cc + ggtitle("Shape and Color (c)")
grid.arrange(aa,bb,cc, ncol=3, clip=T)

## Figure 6.5 in not generated in R

# Figure 6-6 #########################################################
# requires packages: ggplot2, grid, gridExtra
myd <- c(24,22,20,18,16)
names(myd) <- c("A", "B", "C", "D", "E")
mycol <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
mydf <- data.frame(count=myd*.01, label=names(myd))
aa <- ggplot(mydf, aes(x = "", y = count, fill = label))
aa <- aa + geom_bar(width = 1, stat="identity", show_guide=F)
aa <- aa + scale_fill_manual(values = mycol) + xlab("") + ylab("")
aa <- aa + geom_text(aes(x=c(1.3), y=c(.12, .35, .56, .74, .92), label = label))
aa <- aa + coord_polar("y", start=0)
aa <- aa + theme(axis.text = element_blank(),
                 panel.background = element_blank(),
                 panel.border =  element_blank(),
                 panel.grid = element_blank(),
                 axis.ticks = element_blank(),
                 plot.margin = unit(c(0.5,0,0,0), "lines"))

bb <- ggplot(mydf, aes(x=label, y=count))
bb <- bb + geom_bar(fill=mycol, stat="identity")
bb <- bb + ylab("") + xlab("")
bb <- bb + scale_y_continuous(expand = c(0, 0), limits=c(0,.3), breaks=seq(0,0.2,0.05))
bb <- bb + theme(panel.background = element_blank(),
                 panel.border =  element_blank(),
                 panel.grid = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.ticks.length = unit(0.2, "cm"),
                 axis.ticks.margin = unit(0.2, "cm"),
                 panel.margin = unit(0, "lines"),
                 plot.margin = unit(c(0.5,0.5,1,0.5), "lines"))

grid.arrange(aa, bb, ncol=2, clip=T)



# Figure 6.7 was not generated in R

# Figure 6-8 #########################################################
# requires packages: RColorBrewer
library(RColorBrewer)

# Note: this does not use ggplot, but instead
# creates a blank canvas and puts text and boxes on it.
# a neat trick for truly unique approaches.
par(mar=c(0,0,0,0))
plot(NULL, xlim=c(0,120), ylim=c(50,100), yaxt="n", ann=FALSE, xaxt="n", bty="n")
text(17.5,89, "Sequantial", pos=3)
blue <- brewer.pal(5, "Blues")
rdpu <- brewer.pal(5, "RdPu")
ygb <- brewer.pal(5, "YlGnBu")
text(30,83, "Blues", pos=4)
text(30,73, "RdPu", pos=4)
text(30,63, "YlGnBu", pos=4)
for (i in seq(5)) {
  rect(i*5, 80, (i*5)+5, 86, col=blue[i], border=NA)
  rect(i*5, 70, (i*5)+5, 76, col=rdpu[i], border=NA)
  rect(i*5, 60, (i*5)+5, 66, col=ygb[i], border=NA)
}
text(57.5,89, "Diverging", pos=3)
blue <- brewer.pal(5, "RdBu")
rdpu <- brewer.pal(5, "PiYG")
ygb <- brewer.pal(5, "BrBG")
text(70,83, "RdBu", pos=4)
text(70,73, "PiYG", pos=4)
text(70,63, "BrBG", pos=4)
for (i in seq(5)) {
  rect(40+(i*5), 80, (i*5)+45, 86, col=blue[i], border=NA)
  rect(40+(i*5), 70, (i*5)+45, 76, col=rdpu[i], border=NA)
  rect(40+(i*5), 60, (i*5)+45, 66, col=ygb[i], border=NA)
}
text(97.5,89, "Qualitative", pos=3)
blue <- brewer.pal(5, "Set1")
rdpu <- brewer.pal(5, "Set2")
ygb <- brewer.pal(5, "Accent")
text(110,83, "Set1", pos=4)
text(110,73, "Set2", pos=4)
text(110,63, "Accent", pos=4)
for (i in seq(5)) {
  rect(80+(i*5), 80, (i*5)+85, 86, col=blue[i], border=NA)
  rect(80+(i*5), 70, (i*5)+85, 76, col=rdpu[i], border=NA)
  rect(80+(i*5), 60, (i*5)+85, 66, col=ygb[i], border=NA)
}
par(mar=c(5.1,4.1,4.1,2.1))

# Figure 6-9 #########################################################
# requires packages: ggplot2, gdata
library(gdata)  # for the humanReadable conversion

# set up some small functions to make labels prettier
scale.filter <- function(x) {
  x[is.na(x)] <- 0
  humanReadable(x) # shortens long labels
}
scale.filter.nb <- function(x) {
  x[is.na(x)] <- 0
  sub("B", "", humanReadable(x)) # append bytes to label
}
fw <- read.csv("data/fivemin.csv", header=T)
fullfw <- aggregate(cbind(packets, bytes) ~ hour, data=fw, FUN=sum)
gg <- ggplot(fullfw, aes(packets, bytes))
gg <- gg + geom_point(size=3, color="#000066")
gg <- gg + xlab("Packets") + ylab("Bytes") 
gg <- gg + scale_x_continuous(breaks=c(12,15,18,21,24,27)*10^6, labels=scale.filter.nb)
gg <- gg + scale_y_continuous(breaks=c(7,10,13,16,19)*10^9, labels=scale.filter)
gg <- gg + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 axis.ticks = element_blank(),
                 panel.background=element_blank())
print(gg)

# Figure 6-10 #########################################################
# requires packages: ggplot2, gdata
# require object: scale.filter.nb (6-9)
fw <- read.csv("data/fivemin.csv", header=T)
fullfw <- aggregate(cbind(packets, bytes, sessions) ~ hour, data=fw, FUN=sum)
fullfw <- fullfw[order(fullfw$hour), ] # want to be sure we're still sorted
fullfw$iter <- seq_along(fullfw$hour)

# Not a moving average
gcolor=rep("gray97", nrow(fullfw))
gcolor[seq(1,nrow(fullfw), by=12)] <- "gray80"
pcolor=rep("gray60", nrow(fullfw))
pcolor[seq(1,nrow(fullfw), by=6)] <- "#CC0000"

mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")

gg <- ggplot(fullfw, aes(iter, sessions))
gg <- gg + geom_bar(stat="identity", width=0.1, color=gcolor)
gg <- gg + geom_point(size=2, color=pcolor)
gg <- gg + ylab("Sessions") + xlab("Time")
gg <- gg + scale_x_continuous(labels=mylabel, 
                              breaks=seq(1,nrow(fullfw), by=12))
gg <- gg + scale_y_continuous(labels=scale.filter.nb)
gg <- gg + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank())
print(gg)


# Figure 6-11 #########################################################
# requires packages: ggplot2, gdata, gridExtra
# require object: scale.filter (6-9)
fw <- read.csv("data/fivemin.csv", header=T)
better <- data.frame(hour=unique(fw$hour), realx=seq_along(unique(fw$hour)))
allfw <- merge(fw, better, allx=T)
mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
myb <- seq(1, length(unique(allfw$realx)), by=12)

aa <- ggplot(allfw, aes(realx, bytes, group=type, color=type))
aa <- aa + geom_smooth(stat="identity", fill="white")
aa <- aa + theme_bw() + scale_y_log10(labels=scale.filter)
aa <- aa + scale_x_continuous(breaks=myb, label=mylabel)
aa <- aa + xlab("Time") + ylab("Bytes")
aa <- aa + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 legend.position = "bottom")
bb <- ggplot(allfw, aes(realx, bytes, group=type, color=type))
bb <- bb + geom_point(size=1.5) #(stat="identity", fill="white")
bb <- bb + theme_bw() + scale_y_log10(labels=scale.filter)
bb <- bb + scale_x_continuous(breaks=myb, label=mylabel)
bb <- bb + xlab("Time") + ylab("Bytes")
bb <- bb + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 legend.position = "bottom")
grid.arrange(bb, aa, ncol=2, clip=T)

# Figure 6-12 #########################################################
# requires packages: ggplot2, gdata, gridExtra
wk <- c(71, 61, 24, 50, 33, 22,  2,  2,  1,  2,  2,  1)
dev <- c("Workstation", "Server", "Network", "Printer")
sev <- c("High", "Med", "Low")
dfi <- data.frame(x=rep(dev, each=3), y=wk, sev=rep(rev(sev), 4))
dfi$x <- factor(dfi$x, levels=dev, ordered=T)
dfi$sev <- factor(dfi$sev, levels=sev, ordered=T)
# "Blues" palette from colorbrewer
color.pal <- c("#DEEBF7", "#9ECAE1", "#3182BD")

aa <- ggplot(dfi, aes(x, y))
aa <- aa + geom_bar(stat="identity", fill=color.pal[2], show_guide=F)
aa <- aa + xlab("Device Type") + ylab("Vulnerabilities")
aa <- aa + ggtitle("Vertical Bar Chart")
aa <- aa + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 plot.margin = unit(c(0.5,0.4,2,0), "cm"))

bb <- ggplot(dfi, aes(x, y, fill=sev))
bb <- bb + geom_bar(stat="identity") #  show_guide=F)
bb <- bb + xlab("Device Type") + ylab("Vulnerabilities")
bb <- bb + scale_fill_manual(values=rev(color.pal))
bb <- bb + ggtitle("Stacked Bar Chart")
bb <- bb + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 legend.position = "bottom")

cc <- ggplot(dfi, aes(x, y, fill=sev))
cc <- cc + geom_bar(stat="identity", position=position_dodge()) #  show_guide=F)
cc <- cc + xlab("Device Type") + ylab("Vulnerabilities")
cc <- cc + scale_fill_manual(values=rev(color.pal))
cc <- cc + ggtitle("Grouped Bar Chart")
cc <- cc + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 legend.position = "bottom")

grid.arrange(aa, bb ,cc, ncol=3, clip=T)

# Figure 6-13 #########################################################
# requires packages: ggplot2, grid, gridExtra, scales
# require object: scale.filter (6-9), scale.filter.nb (6-9)
library(scales)

fw <- read.csv("data/fivemin.csv", header=T)
wk <- fw[which(fw$type=="Network"), ]  # not log
myred <- brewer.pal(3, "Set2")[3]

aa <- ggplot(wk, aes(sessions, bytes, size=packets, color=type, fill=type))
aa <- aa + xlab("Sessions") + ylab("Bytes")
aa <- aa + geom_point(alpha=1, shape=21, fill=myred, color="gray80", guide=F)
aa <- aa + theme_bw()
aa <- aa + scale_size_continuous(name="Packet Count", breaks=c(1,10), range = c(1, 20), trans=log10_trans())
aa <- aa + ggtitle("alpha = 1")
aa <- aa + scale_x_continuous(labels=scale.filter.nb)
aa <- aa + scale_y_continuous(labels=scale.filter, limits=c(0, max(wk$bytes)*1.1)) 
aa <- aa + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 legend.position="none")

bb <- ggplot(wk, aes(sessions, bytes, size=packets, color=type, fill=type))
bb <- bb + xlab("Sessions") + ylab("Bytes")
bb <- bb + geom_point(alpha=1/3, shape=21, fill=myred, color="gray80", guide=F)
bb <- bb + theme_bw()
bb <- bb + scale_size_continuous(name="Packet Count", range = c(1, 20), trans=log10_trans())
bb <- bb + ggtitle("alpha = 1/3")
bb <- bb + scale_x_continuous(labels=scale.filter.nb)
bb <- bb + scale_y_continuous(labels=scale.filter, limits=c(0, max(wk$bytes)*1.1)) 
bb <- bb + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank(),
                 legend.position="none")

grid.arrange(aa, bb, ncol=2, clip=T)

# Figure 6-14 #########################################################
# requires packages: portfolio  (for treemap)
library(portfolio)

mydata <- read.csv("data/ipmap2.csv", header=T)
  mydata$mean <- mydata$sessions/mydata$count
  # this adds stuff, I removed it with inkscape
  map.market(id=mydata$label, area=sqrt(mydata$count)^1.3, group=mydata$type, 
             color=sqrt(mydata$mean), main="remove all but treemap", 
             lab= c("group"=F, "id"=T))

# Figure 6-15 #########################################################
# requires packages: ggplot2, gdata, gridExtra
# require object: scale.filter (6-9), scale.filter.nb (6-9)
fw <- read.csv("data/fivemin.csv", header=T)
fullfw <- aggregate(cbind(packets, bytes, sessions) ~ hour, data=fw, FUN=sum)
aa <- ggplot(fullfw, aes(x=sessions))
aa <- aa + geom_histogram(binwidth=12000, colour="black", fill="#8DA0CB") 
aa <- aa + scale_x_continuous(labels=scale.filter.nb)
aa <- aa + xlab("Number of Sessions") + ylab("Count")
aa <- aa + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank())
print(aa)
  
bb <- ggplot(fullfw, aes(x=sessions))
bb <- bb + geom_histogram(aes(y=..density..), binwidth=12000, colour="#80808080", fill="#8DA0CB66", alpha=1/3)
bb <- bb + geom_density(alpha=1/2, fill="#8DA0CB")  # Overlay with transparent density plot
bb <- bb + xlab("Number of Sessions") + ylab("Density")
bb <- bb + scale_x_continuous(labels=scale.filter.nb)
bb <- bb + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks = element_blank(),
                 panel.background=element_blank())
print(bb)

grid.arrange(aa, bb, ncol=2, clip=T)

# Figure 6.16 and 6.17 were created in R, but not for the book

# Figure 6-18 #########################################################
my.fw <- read.csv("data/3weeks.csv", header=T)
my.fw2 <- aggregate(bytes ~ dh.seq, data=my.fw, FUN=mean)
aa.breaks <- 144+seq(1,nrow(my.fw), by=nrow(my.fw)/21)
aa.lab <- rep(c("S", "M", "T", "W", "T", "F", "S"), 3)
bb.breaks <- nrow(my.fw2)/42 + seq(1, nrow(my.fw2), by=nrow(my.fw2)/21)

aa <- ggplot(my.fw, aes(x=seq, y=bytes))
aa <- aa + geom_line(color="steelblue")
aa <- aa + scale_y_continuous(labels=scale.filter, 
                              limits=c(0, max(my.fw$bytes)))
aa <- aa + ggtitle("Basic Line Plot")
aa <- aa + scale_x_continuous(labels=aa.lab, breaks=aa.breaks)
aa <- aa + xlab("Day") + ylab("Bytes") + theme_bw()
print(aa)  
bb <- ggplot(my.fw2, aes(x=dh.seq, y=bytes))
bb <- bb + geom_line(color="steelblue")
bb <- bb + scale_y_continuous(labels=scale.filter, 
                              limits=c(0, max(my.fw$bytes)))
bb <- bb + ggtitle("One hour averages")
bb <- bb + scale_x_continuous(labels=aa.lab, breaks=bb.breaks)
bb <- bb + xlab("Day") + ylab("Bytes") + theme_bw()
  
cc <- ggplot(my.fw, aes(x=seq, y=bytes))
cc <- cc + geom_point(alpha=2/3, size=1, color="steelblue") 
cc <- cc + ggtitle("Using Points")
cc <- cc + scale_y_continuous(labels=scale.filter, 
                   limits=c(0, max(my.fw$bytes)))
cc <- cc + scale_x_continuous(labels=aa.lab, breaks=aa.breaks)
cc <- cc + xlab("Day") + ylab("Bytes") + theme_bw()

grid.arrange(aa, bb, cc, ncol=1, clip=T)

# Listing 6-1 #########################################################
# random walk
set.seed(1)
# set up nine directions
dirs <- matrix(c(rep(seq(-1, 1), 3), 
                 rep(seq(-1, 1), each=3)), ncol=2, byrow=T)
# start in the center
cpos <- matrix(c(0, 0), ncol=2)
# set full screen
par(mar=c(0,0,0,0))
# take 200 steps
for(i in seq(200)) { 
  plot(cpos, type="p", col="gray80", xlim=c(-20, 20), ylim=c(-20,20),
       yaxt="n", ann=FALSE, xaxt="n", bty="n")
  cpos <- rbind(cpos, cpos[nrow(cpos), ] + dirs[sample(1:9, 1), ])
  points(cpos[nrow(cpos), 1], cpos[nrow(cpos), 2],
         type="p", pch=16, col="red")
  Sys.sleep(0.1)
}
# reset screen back to default
par(mar=c(5.1,4.1,4.1,2.1))
