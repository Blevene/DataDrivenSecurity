
# Listing 7-1 #########################################################
# this is a JSON example in the book

# Listing 7-2 #########################################################
# see python code

# Listing 7-3 #########################################################
# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/book/ch07")
# make sure the packages for this chapter
# are installed, install if necessary
pkg <- c("devtools", "ggplot2", "scales", "rjson")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

# Listing 7-4 #########################################################
library(rjson)
# fromJSON accepts a filename to read from
jsonfile <- "data/vcdb/F58E9169-AC07-400E-AB0E-DB784C6CAE59.json"
incident <- fromJSON(file=jsonfile)
# print the hacking variety
print(incident$action$hacking$variety)
## [1] "SQLi"

# Listing 7-5 #########################################################
# if we haven't installed devtools yet, let's install it
library(devtools)
# now let's install the verisr package
install_github("verisr", "jayjacobs")
# and load up the library
library(verisr)

# Listing 7-6 #########################################################
# requires package : verisr
# set this to where VCDB incidents are stored
jsondir <- 'data/vcdb/'
# create a veris instance with the vcdb data
vcdb <- json2veris(jsondir)

# Listing 7-7 #########################################################
# requires package : verisr
# requires object: vcdb (7-6)
# we can get some high level overview of the incidents by running
summary(vcdb)
## 1643 incidents in this object.
##
## Actor:
## external internal  partner  unknown
##      955      535      100       85
##
## Action:
##    error  hacking  malware   misuse physical   social
##      398      416       42      216      508       31
##
## Asset:
## Kiosk/Term  Media  Network   Person   Server  Unknown   User Dev
##         17    534        8       33      656       80        429
##
## Attribute:
##  confidentiality     availability  confidentiality     integrity
##                2              614             1604           165
#plot(vcdb)

# Listing 7-8 #########################################################
# requires package : verisr
# requires object: vcdb (7-6)
actors <- getenum(vcdb, "actor")
# actors is a data frame 
print(actors)
##       enum   x
## 1 external 955
## 2 internal 535
## 3  partner 100
## 4  unknown  85

# Listing 7-9 #########################################################
# requires package : verisr
# requires object: vcdb (7-6)
actors <- getenum(vcdb, "actor", add.n=TRUE, add.freq=TRUE)
print(actors)
##       enum   x    n  freq
## 1 external 955 1643 0.581
## 2 internal 535 1643 0.326
## 3  partner 100 1643 0.061
## 4  unknown  85 1643 0.052

# Listing 7-10 #########################################################
# requires package : verisr, ggplot2
# requires object: vcdb (7-6)
library(ggplot2)
# take in the vcdb object and the field to plot
verisplot <- function(vcdb, field) {
  # get the data.frame for the field with frequency
  localdf <- getenum(vcdb, field, add.freq=T)
  # now let's take first 5 fields in the data frame.
  localdf <- localdf[c(1:5), ]
  # add a label to the data.frame
  localdf$lab <- paste(round(localdf$freq*100, 0), "%", sep="")
  # now we can create a ggplot2 instance
  gg <- ggplot(localdf, aes(x=enum, y=freq, label=lab))
  gg <- gg + geom_bar(stat="identity", fill="steelblue")
  # add in text, adjusted to the end of the bar
  gg <- gg + geom_text(hjust=-0.1, size=3)
  # flip the axes and add in a title
  gg <- gg + coord_flip() + ggtitle(field)
  # remove axes labels and add bw theme
  gg <- gg + xlab("") + ylab("") + theme_bw()
  # fix the y scale to remove padding and fit our label (add 7%) 
  gg <- gg + scale_y_continuous(expand=c(0,0), 
                                limits=c(0, max(localdf$freq)*1.1))
  # make it slightly prettier than the default
  gg <- gg + theme(panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks = element_blank())
}

# try the following (code not in book)
print(verisplot(vcdb, "action"))
print(verisplot(vcdb, "actor.external.variety"))
print(verisplot(vcdb, "action.physical.variety"))
print(verisplot(vcdb, "action.hacking.vector"))
print(verisplot(vcdb, "attribute.confidentiality.data.variety"))
print(verisplot(vcdb, "asset.assets"))

# Listing 7-11#########################################################
# requires package : verisr, ggplot2
# requires object: vcdb (7-6)
# get a data.frame comparing the actions to the assets
# this will add zero's in missing squares and include a frequency
a2 <- getenum(vcdb, enum="action", primary="asset.assets", add.freq=T)
# trim unknown asset and environment action for space
a2 <- a2[which(a2$enum!="environmental" & a2$primary!="Unknown"), ]
# so we should create a "slim" version without zeros to color it
slim.a2 <- a2[which(a2$x!=0), ]
# could sort these by converting to factors (we did in Fig 7-6)

# now make a nice plot  
gg <- ggplot(a2, aes(x=enum, y=primary, fill=freq))
gg <- gg + geom_tile(fill="white", color="gray80")
gg <- gg + geom_tile(data=slim.a2, color="gray80")
gg <- gg + scale_fill_gradient(low = "#F0F6FF", 
                               high = "#4682B4", guide=F)
gg <- gg + xlab("") + ylab("") + theme_bw()
gg <- gg + scale_x_discrete(expand=c(0,0))
gg <- gg + scale_y_discrete(expand=c(0,0))
gg <- gg + theme(axis.ticks = element_blank())
# and view it
print(gg)



# Figure 7-1 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
# get the count of incidents with an actor
actors <- getenumlist(vcdb, "actor")
actfields <- c("external", "internal", "partner")
actors.n <- sum(sapply(actors, function(x) {
  any(actfields %in% x)
}))
# now let's grab all three sections for motives.
motive <- getenum(vcdb, "actor.external.motive")
motive <- rbind(motive, getenum(vcdb, "actor.internal.motive"))
motive <- rbind(motive, getenum(vcdb, "actor.partner.motive"))
# combine them 
allmotive <- aggregate(x ~ enum, data=motive, FUN=sum)
# have to re-order the primary enum.
allmotive <- allmotive[with(allmotive, order(x)), ]
allmotive$enum <- factor(allmotive$enum, levels=allmotive$enum, ordered=T)
# and add in the frequency with actor count above
allmotive$freq <- allmotive$x/actors.n
## remove unknown as a proportion, focusing on "known" here.
finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(finalmotive, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(allmotive$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)


# Figure 7-2 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)

# should have verisr loaded and vcdb object created

actions <- getenumlist(vcdb, "action")
actfields <- c("malware", "hacking", "social", "error", "misuse", "physical") # sorry env
actions.n <- sum(sapply(actions, function(x) {
  any(actfields %in% x)
}))
# falling in love with "do.call"
action <- do.call(rbind, lapply(actfields, function(a) {
  rawaction <- getenum(vcdb, paste('action.', a, '.variety', sep=''))
  rawaction$enum <- paste(rawaction$enum, ' (', substr(a, 1, 3) ,')', sep='')
  rawaction
}))
allaction <- action[with(action, order(-x)), ]
# gotta do a top 5
topaction <- allaction[which(allaction$enum %in% allaction$enum[1:10]), ]
topaction$enum <- factor(topaction$enum, levels=rev(topaction$enum), ordered=T)
topaction$freq <- topaction$x/actions.n
## remove unknown as a proportion
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown" | allmotive$enum=="NA"), ]
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(topaction, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(topaction$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)


# Figure 7-3 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
# this is easy compared to the rest!
asset <- getenum(vcdb, "asset.assets", add.freq=T)

# flip it
asset <- asset[(with(asset, order(x))), ]
asset$enum <- factor(asset$enum, levels=asset$enum, ordered=T)

gg <- ggplot(asset, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(asset$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)

# Figure 7-4 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
## this is a bit of a hack, as verisr does not do industry well (yet).
data("industry2")  # part of verisr package
rawi <- getenum(vcdb, "victim.industry2", add.n=T)
industrytext <- merge(rawi, industry2, by.x="enum", by.y="code", all.x=T)
aggind <- aggregate(x ~ title + n, data=industrytext, FUN=sum)
# flip it
industry <- aggind[(with(aggind, order(x))), ]
industry <- industry[c((nrow(industry)-4):nrow(industry)), ]
# hack until I fix industry2 data set with short title
industry$title <- as.character(industry$title)
#industry$title[which(industry$title=="Professional, Scientific, and Technical Services")] <- "Professional Services"
industry$title[which(industry$title=="Health Care and Social Assistance")] <- "Health Care"
industry$title <- factor(industry$title, levels=industry$title, ordered=T)
industry$freq <- industry$x/industry$n
gg <- ggplot(industry, aes(x=title, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(industry$freq)+.05)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)


# Figure 7-5 #########################################################
# requires package : verisr, ggplot2, scales, gridExtra, grid
# requires object: vcdb (7-6), verisplot (7-10)
library(grid)
library(gridExtra)
aa <- verisplot(vcdb, "action")
bb <- verisplot(vcdb, "actor.external.variety")
cc <- verisplot(vcdb, "action.physical.variety")
dd <- verisplot(vcdb, "action.hacking.vector")
ee <- verisplot(vcdb, "attribute.confidentiality.data.variety")
ff <- verisplot(vcdb, "asset.assets")
grid.arrange(aa, bb, cc, dd, ee, ff, ncol=2, clip=T)

