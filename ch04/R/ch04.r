# Listing 4-0
# This is for the R code in the chapter
# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/book/ch04")
# make sure the packages for this chapter
# are installed, install if necessary
pkg <- c("bitops", "ggplot2", "mapproj", "stringr", "maps",
         "grid", "gridExtra", "RColorBrewer", "igraph",
         "colorspace", "scales", "stringr", "reshape2")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)  
}

# Listing 4-1
# requires packages: bitops
library(bitops) # load the bitops functions

# Define functions for converting IP addresses to/from integers
# take an IP address string in dotted octets (e.g. "192.168.0.1")
# and convert it to a 32-bit long integer (e.g. 3232235521)
ip2long <- function(ip) {
  # convert string into vector of characters
  ips <- unlist(strsplit(ip, '.', fixed=TRUE))
  # set up a function to bit-shift, then "OR" the octets
  octet <- function(x,y) bitOr(bitShiftL(x, 8), y)
  # Reduce applys a function cumulatively left to right
  Reduce(octet, as.integer(ips))
}

# take an 32-bit integer IP address (e.g. 3232235521)
# and convert it to a (e.g. "192.168.0.1").
long2ip <- function(longip) {
  # set up reversing bit manipulation
  octet <- function(nbits) bitAnd(bitShiftR(longip, nbits), 0xFF)
  # Map applys a function to each element of the argument
  # paste converts arguments to character and concatenates them
  paste(Map(octet, c(24,16,8,0)), sep="", collapse=".")
}

long2ip(ip2long("192.168.0.0"))
## [1] "192.168.0.0"
long2ip(ip2long("192.168.100.6"))
## [1] "192.168.100.6"

# Listing 4-2
# requires packages: bitops
# requires all objects from 4-1
# Define function to test for IP CIDR membership
# take an IP address (string) and a CIDR (string) and
# return whether the given IP address is in the CIDR range
ip.is.in.cidr <- function(ip, cidr) {
  long.ip <- ip2long(ip)
  cidr.parts <- unlist(strsplit(cidr, "/"))
  cidr.range <- ip2long(cidr.parts[1])
  cidr.mask <- bitShiftL(bitFlip(0), (32-as.integer(cidr.parts[2])))
  return(bitAnd(long.ip, cidr.mask) == bitAnd(cidr.range, cidr.mask))
}

ip.is.in.cidr("10.0.1.15","10.0.1.3/24")
## TRUE
ip.is.in.cidr("10.0.1.15","10.0.2.255/24")
## FALSE

# Listing 4-3
# R code to extract longitude/latitude pairs from AlienVault data
# read in the AlienVault reputation data
avRep <- "data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP", "Reliability", "Risk", "Type",
                     "Country", "Locale", "Coords", "x")

# create a vector of lat/long data by splitting on ","
av.coords.vec <- unlist(strsplit(as.character(av.df$Coords), ","))
# convert the vector in a 2-column matrix
av.coords.mat <- matrix(av.coords.vec, ncol=2, byrow=TRUE)
# project into a data frame
av.coords.df <- as.data.frame(av.coords.mat)
# name the columns 
colnames(av.coords.df) <- c("lat","long")
# convert the characters to numeric values
av.coords.df$long <- as.double(as.character(av.coords.df$long))
av.coords.df$lat <- as.double(as.character(av.coords.df$lat))


# Listing 4-4
# requires packages: ggplot2, maps, RColorBrewer, scales
# requires object: av.coords.df (4-3)
# generates Figure 4-2
# R code to extract longitude/latitude pairs from AlienVault data
# need plotting and mapping functions plus colors
library(ggplot2)
library(maps)
library(RColorBrewer)
library(scales)

# extract a color palette from the RColorBrewer package
set2 <- brewer.pal(8,"Set2")

# extract the polygon information for the world map, minus Antarctica
world <- map_data('world')
world <- subset(world, region != "Antarctica")

# plot the map with the points marking lat/lon of the geocoded entries
# plotting ~200K takes a bit of time
# Chapter 5 examples explain mapping in greater detail
gg <- ggplot()
gg <- gg + geom_polygon(data=world, aes(long, lat, group=group), 
                        fill="white")
gg <- gg + geom_point(data=av.coords.df, aes(x=long, y=lat),  
                      color=set2[2], size=1, alpha=0.1)
gg <- gg + labs(x="", y="")
gg <- gg + theme(panel.background=element_rect(fill=alpha(set2[3],0.2), 
                                              colour='white'))
gg


# Listing 4-5
# requires packages: stringr
# requires object: av.df (4-3)
# R code to incporporate IANA IPv4 allocations
# retrieve IANA prefix list
library(stringr)

ianaURL <- "http://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv"
ianaData <- "data/ipv4-address-space.csv"
if (file.access(ianaData)) {
  download.file(ianaURL, ianaData) 
}

# read in the IANA table
iana <- read.csv(ianaData)

# clean up the iana prefix since it uses the old/BSD-
# number formatting (i.e. allows leading zeroes and
# we do not need to know the CIDR component.
iana$Prefix <- sub("^(00|0)", "", iana$Prefix, perl=TRUE)
iana$Prefix <- sub("/8$", "", iana$Prefix, perl=TRUE)

# define function to strip 'n' characters from a string
# (character vector) and return the shortened string.
# note that this function is 'vectorized' (you can pass it a single
# string or a vector of them)
rstrip <- function(x, n){
  substr(x, 1, nchar(x)-n)
}

# extract just the prefix from the AlienVault list
av.IP.prefix <- rstrip(str_extract(as.character(av.df$IP),
                                  "^([0-9]+)\\."), 1)

# there are faster ways than 'sapply()' but we wanted you to 
# see the general "apply" pattern in action as you will use it
# quite a bit throughout your work in R
av.df$Designation <- sapply(av.IP.prefix, function(ip) {
  iana[iana$Prefix == ip, ]$Designation
})

# summarize, order & review the findings
summary(factor(av.df$Designation))
##       Administered by AFRINIC         Administered by APNIC 
##                           322                          2615 
##          Administered by ARIN      Administered by RIPE NCC 
##                         17974                          5893 
##                       AFRINIC                         APNIC 
##                          1896                         93776 
##                          ARIN        AT&T Bell Laboratories 
##                         42358                            24 
## Digital Equipment Corporation       Hewlett-Packard Company 
##                             1                             3 
##                        LACNIC  Level 3 Communications, Inc. 
##                         18914                            31 
##                  PSINet, Inc.                      RIPE NCC 
##                            30                         74789 


# Listing 4-6
# requires packages: ggplot2, maps, RColorBrewer
# requires object: av.coords.df (4-3), iana (4-5)
# Code to extract IANA block assignments & compare w/AlienVault groups
# create a new data frame from the iana designation factors
iana.df <- data.frame(table(iana$Designation))
colnames(iana.df) <- c("Registry", "IANA.Block.Count")

# make a data frame of the counts of the av iana
# designation factor
tmp.df <- data.frame(table(factor(av.df$Designation)))
colnames(tmp.df) <- c("Registry", "AlienVault.IANA.Count")

# merge (join) the data frames on the "reg" column
combined.df <- merge(iana.df, tmp.df)
print(combined.df[with(combined.df, order(-IANA.Block.Count)),],
      row.names=FALSE)

##                       Registry IANA.Block.Count AlienVault.IANA.Count
##                          APNIC               45                 93776
##           Administered by ARIN               44                 17974
##                           ARIN               36                 42358
##                       RIPE NCC               35                 74789
##                         LACNIC                9                 18914
##          Administered by APNIC                6                  2615
##       Administered by RIPE NCC                4                  5893
##                        AFRINIC                4                  1896
##        Administered by AFRINIC                2                   322
##   Level 3 Communications, Inc.                2                    31
##         AT&T Bell Laboratories                1                    24
##  Digital Equipment Corporation                1                     1
##        Hewlett-Packard Company                1                     3
##                   PSINet, Inc.                1                    30

# Listing 4-7
# requires packages: reshape, grid, gridExtra, ggplot2, RColorBrewer
# requires object: combined.df (4-6), set2 (4-4)
# generates Figure 4-3
# plot charts from IANA data
# flatten the data frame by making one entry per "count" type
# versus having the counts in individual columns
# need the 'melt()' function from the reshape package
# to transform the data frame shape
library(reshape2) 
library(grid)
library(gridExtra)

# normalize the IANA and AV values to % so bar chart scales
# match and make it easier to compare
combined.df$IANA.pct <- 100 * (combined.df$IANA.Block.Count / 
                                 sum(combined.df$IANA.Block.Count))
combined.df$AV.pct <- 100 * (combined.df$AlienVault.IANA.Count / 
                               sum(combined.df$AlienVault.IANA.Count))

combined.df$IANA.vs.AV.pct <- combined.df$IANA.pct - combined.df$AV.pct

melted.df <- melt(combined.df)
# plot the new melted data frame values
gg1 <- ggplot(data=melted.df[melted.df$variable=="IANA.pct",], 
              aes(x=reorder(Registry, -value), y=value))
# set min/max for axis so scale is same for both charts
gg1 <- gg1 + ylim(0,40)
gg1 <- gg1 +  geom_bar(stat="identity", fill=set2[3]) # using bars

# make a better label for the y axis
gg1 <- gg1 + labs(x="Registry", y="%", title="IANA %") 
# make bar chart horizontal
gg1 <- gg1 + coord_flip()
# rotate the x-axis labels and remove the legend
gg1 <- gg1 + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                   panel.background = element_blank(),
                   legend.position = "none")

gg2 <- ggplot(data=melted.df[melted.df$variable=="AV.pct",], 
              aes(x=reorder(Registry,-value), y=value))
gg2 <- gg2 + geom_bar(stat="identity", fill=set2[4]) # using bars
gg2 <- gg2 + ylim(0,40)
gg2 <- gg2 + labs(x="Registry", y="%", title="AlienVault IANA %") 
gg2 <- gg2 + coord_flip()
gg2 <- gg2 + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                   panel.background = element_blank(),
                   legend.position = "none")

# grid.arrange makes it possible to do very precise placement of 
# multiple ggplot objects
grid.arrange(gg1, gg2, ncol=1, nrow=2)


# Listing 4-8
# requires packages: ggplot2
# requires object: combined.df (4-7), set2 (4-4)
gg <- ggplot(data=combined.df, 
             aes(x=reorder(Registry, -IANA.Block.Count), y=AV.pct ))
gg <- gg + geom_bar(stat="identity", fill=set2[2])
gg <- gg + labs(x="Registry", y="Count",
                title="AlienVault/IANA sorted by IANA (low-to-high") 
gg <- gg + coord_flip()
gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                 panel.background = element_blank(),
                 legend.position = "none")
gg


######################################################################
# BEGIN CODE FOR FIGURE 4-5 (Scatterplots)
# THIS CODE GENERATES THE FIGURE BUT IS NOT DISPLAYED IN THE BOOK
######################################################################
mfrow <- par()$mfrow
mar <- par()$mar
oma <- par()$oma

par(mfrow=c(2, 3), mar=c(0, 0, 2, 0), oma=c(1, 1, 1, 1))
set.seed(2)
x <- runif(500, min=0, max=pi)

y <- rnorm(500, mean=x, sd=0.2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Designation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=-x, sd=0.3)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=sin(x), sd=0.2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=x, sd=2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=-x, sd=1)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- runif(500, min=0, max=pi)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

par(mfrow=mfrow, mar=mar, oma=oma)
######################################################################
# END CODE FOR FIGURE 4-5
######################################################################

# Listing 4-9
# requires packages: ggplot2, scales
# requires object: combined.df (4-7), set2 (4-4)
# generates figure 4-6
library(scales)
gg <- ggplot(data=combined.df)
gg <- gg + geom_point(aes(x=IANA.Block.Count, 
                          y=AlienVault.IANA.Count),
                      color=set2[2], size=4)
gg <- gg + labs(x="IANA Block Count", y="AlienVault IANA Count",
                title="IANA ~ AlienVault")
gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                 legend.position = "none")
gg <- gg + theme(panel.background=element_rect(fill=alpha(set2[3],0.2),
                                               colour='white'))
gg

cor(combined.df$IANA.Block.Count,
    combined.df$AlienVault.IANA.Count, method="spearman")

# Listing 4-10
anscombe[,c(1,5,2,6,3,7,4,8)]
##    x1    y1 x2   y2 x3    y3 x4    y4
## 1  10  8.04 10 9.14 10  7.46  8  6.58
## 2   8  6.95  8 8.14  8  6.77  8  5.76
## 3  13  7.58 13 8.74 13 12.74  8  7.71
## 4   9  8.81  9 8.77  9  7.11  8  8.84
## 5  11  8.33 11 9.26 11  7.81  8  8.47
## 6  14  9.96 14 8.10 14  8.84  8  7.04
## 7   6  7.24  6 6.13  6  6.08  8  5.25
## 8   4  4.26  4 3.10  4  5.39 19 12.50
## 9  12 10.84 12 9.13 12  8.15  8  5.56
## 10  7  4.82  7 7.26  7  6.42  8  7.91
## 11  5  5.68  5 4.74  5  5.73  8  6.89

sapply(anscombe,mean)
##       x1       x2       x3       x4       y1       y2       y3 
## 9.000000 9.000000 9.000000 9.000000 7.500909 7.500909 7.500000 
##       y4 
## 7.500909
sapply(anscombe,sd)
##       x1       x2       x3       x4       y1       y2       y3 
## 3.316625 3.316625 3.316625 3.316625 2.031568 2.031657 2.030424 
##       y4 
## 2.030579 
sapply(anscombe,var)
##      x1        x2        x3        x4        y1        y2 
## 11.000000 11.000000 11.000000 11.000000  4.127269  4.127629 
##       y3        y4 
## 4.122620  4.123249 
for (i in 1:4) cat(cor(anscombe[,i], anscombe[,i+4]), "\n")
## 0.8164205 
## 0.8162365 
## 0.8162867 
## 0.8165214 


# Listing 4-11
# Retrieve and read ZeuS blocklist data into R
zeusURL <- "https://zeustracker.abuse.ch/blocklist.php?download=ipblocklist"
zeusData <- "data/zeus.csv"
if (file.access(zeusData)) {
  # need to change download method for universal "https" compatibility
  download.file(zeusURL, zeusData, method="curl") 
}
# read in the ZeuS table; skip junk; no header; assign colnames
zeus <- read.table(zeusData, skip=5, header=FALSE, col.names=c("IP"))


# Listing 4-12
# requires packages: igraph, plyr, RColorBrewer, colorspace
# requires object: set2 (4-4)
library(igraph)
library(plyr)
library(colorspace)

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

BulkOrigin <- function(ip.list,host="v4.whois.cymru.com",port=43) {
 
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query 
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) = c("AS","IP","BGP.Prefix","CC",
                      "Registry","Allocated","AS.Name")
  
  return(response)
  
}

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

BulkPeer <- function(ip.list,host="v4-peer.whois.cymru.com",port=43) {
   
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  response <- laply(response,function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })  
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) <- c("Peer.AS","IP","BGP.Prefix","CC",
                       "Registry","Allocated","Peer.AS.Name")
  return(response)
  
}

# HELPER FUNCTION MENTIONED IN THE BOOK
# BUT NOT IN THE PRINTED LISTINGS

BulkOriginASN <- function(asn.list,host="v4.whois.cymru.com",port=43) {
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(asn.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  
  response <- response[2:length(response)]
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) <- c("AS","CC","Registry","Allocated","AS.Name")
  
  return(response)
  
}

# load the zeus botnet data used to perform the
# remainder of the analyses in the chapter
zeus <- read.table("data/zeus-book.csv", skip=5, header=FALSE, 
                   col.names=c("IP"))
ips <- as.character(zeus$IP) 
# get BGP origin data & peer data; 
origin <- BulkOrigin(ips)
g <- graph.empty() # start graphing
# Make IP vertices; IP endpoints are red
g <- g + vertices(ips, size=5, color=set2[4], group=1)
# Make BGP vertices
g <- g + vertices(origin$CC, size=5, color=set2[2], group=2)
# for each IP address, get the origin AS CC and return 
# them as a pair to create the IP->CC edge list
ip.cc.edges <- lapply(ips, function(x) {
  iCC <- origin[origin$IP==x, ]$CC
  lapply(iCC, function(y){
    c(x, y)
  })
})
g <- g + edges(unlist(ip.cc.edges)) # build CC->IP edges
# simplify the graph by combining commmon edges
g <- simplify(g, edge.attr.comb=list(weight="sum"))
# delete any standalone vertices (lone wolf ASNs). In "graph" terms
# delete any vertex with a degree of 0
g <- delete.vertices(g, which(degree(g) < 1))
E(g)$arrow.size <- 0 # we hate arrows
# blank out all the IP addresses to focus on ASNs 
V(g)[grep("\\.", V(g)$name)]$name <- ""

# Listing 4-13
# Visualizing the ZeuS blocklist country cluster graph
# requires packages: igraph, plyr
# requires all objects from Listing 4-11
# this is a great layout for moderately sized networks. you can
# tweak the "n=10000" if this runs too slowly for you. The more
# iterations, the cleaner the graph will look
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
# plot the graph
par(bg = 'white', mfrow=c(1,1))
plot(g, margin=0, layout=L, vertex.label.dist=0.5, 
     vertex.label.cex=0.75, 
     vertex.label.color="black",
     vertex.label.family="sans", 
     vertex.label.font=2,
     main="ZeuS botnet nodes clustered by country")

# Listing 4-14
# require package: igraph (4-11)
# requires object: V() (4-11), g (4-11)
# read in country code to name translation table
zeus.cc <- grep("[A-Z]", V(g)$name, value=TRUE)
zeus.cc <- zeus.cc[order(zeus.cc)]
# read in the country codes data frame
cc.df <- read.csv("data/countrycode_data.csv")
# display cc & name for just the ones from our data set
print(head(cc.df[cc.df$iso2c %in% zeus.cc, c(7,1)], n=10),
      row.names=FALSE)
## iso2c   country.name
##    AR      ARGENTINA
##    AU      AUSTRALIA
##    AT        AUSTRIA
##    AZ     AZERBAIJAN
##    BG       BULGARIA
##    CA         CANADA
##    CL          CHILE
##    CN          CHINA
##    CZ CZECH REPUBLIC
##    DE        GERMANY


# Listing 4-15
# requires objects: BulkOrigin() & BulkPeer() from book's web site &
#   set2 (4-4)
# create connected network of ZeuS IPs, ASNs, and ASN peers
# generates Figure 4-8
# require package: igraph, RColorBrewer
g <- graph.empty()
g <- g + vertices(ips, size=3, color=set2[4], group=1)
origin <- BulkOrigin(ips)
peers <- BulkPeer(ips)
# add ASN origin & peer vertices
g <- g + vertices(unique(c(peers$Peer.AS, origin$AS)),
                  size=3, color=set2[2], group=2)
# build IP->BGP edge list
ip.edges <- lapply(ips, function(x) {
  iAS <- origin[origin$IP==x, ]$AS
  lapply(iAS,function(y){
    c(x, y)
  })
})

bgp.edges <- lapply(
  grep("NA",unique(origin$BGP.Prefix),value=TRUE,invert=TRUE),
  function(x) {
    startAS <- unique(origin[origin$BGP.Prefix==x,]$AS)
    lapply(startAS,function(z) {
      pAS <- peers[peers$BGP.Prefix==x,]$Peer.AS
      lapply(pAS,function(y) {
        c(z,y)
      })
    })
  })
g <- g + edges(unlist(ip.edges))
g <- g + edges(unlist(bgp.edges))
g <- delete.vertices(g, which(degree(g) < 1))
g <- simplify(g, edge.attr.comb=list(weight="sum"))
E(g)$arrow.size <- 0
V(g)[grep("\\.", V(g)$name)]$name = ""
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
par(bg = 'white')
plot(g, margin=0, layout=L, vertex.label.dist=0.5, 
     vertex.label=NA,
     main="ZeuS botnet ASN+Peer Network")

# ADDITIONAL HELPER FUNCTION MENTIONED IN THE BOOK BUT NOT PRINTED

graph.cc <- function(ips, alien.vault.df, show.plot=TRUE) {

  options("warn" = -1)
  
  # Lookup ASN info for the incoming IP list which will
  # have country of origin info that's fairly accurate
  origin <- BulkOrigin(ips)
  
  # filter out IP and Type from the alienvault DB only for our ip list
  ips.types <- alien.vault.df[alien.vault.df$IP %in% ips,c(1,2,4)]
  
  # get a tabular summary of the types and counts
  ftab <- table(factor(ips.types[ips.types$IP %in% ips,]$Type))
  
  # build a color table from the tabular summary
  # myColors <- rainbow_hcl(length(names(ftab)),c=60,l=70,start=20)
  myColors <- set2
  col.df <- data.frame(Type=names(ftab),Color=myColors)
  
  # begin graph creation
  g <- graph.empty()
  
  # add our ip list as the starting vertices
  g <- g + vertices(ips,size=3,group=1)
  
  # i don't the df is necessary anymore...will test later
  ips.df <- data.frame(ips)
  colnames(ips.df) <- c("IP")
  
  # get the current list of vertex names...i think i can remove this too
  v.names <- V(g)$name
  
  # assign colors to the vertices based on the type
  V(g)$color <- as.character(col.df[col.df$Type %in% ips.types[ips.types$IP %in% v.names,]$Type,]$Color)
  
  # add country vertices
  g <- g + vertices(
    unique(origin$CC),
    size=3,color="black",group=2)
  
  # build country->ip edges
  ip.cc.edges <- lapply(ips,function(x) {
    iCC <- origin[origin$IP==x,]$CC
    lapply(iCC,function(y){
      c(x,y)
    })
  })
  
  # add edges
  g <- g + edges(unlist(ip.cc.edges))
  
  # simplify (though it's almost not necessary given the low
  # complexity of the graph)
  g <- simplify(g, edge.attr.comb=list(weight="sum"))
  
  # remove lone wolf vertices
  g <- delete.vertices(g, which(degree(g) < 1))
  
  # arrows: ugh
  E(g)$arrow.size <- 0
  
  # we only want to see country labels, not IPs
  V(g)[grep("[0-9]",V(g)$name)]$name <- ""
  
  # 10000 makes pretty graphs and takes a pretty long time
  L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
  
  # for when I add community options
  c <- walktrap.community(g, steps=10)
  v <- evcent(g)$vector
  
  if (show.plot) {
    def.par <- par(no.readonly = TRUE)
    par(bg = 'white')
    layout(matrix(c(1,2,3,1), 1, 2, byrow = TRUE), widths=c(5,1))
    plot(g,margin=0,layout=L,vertex.label.dist=0.6, 
         vertex.label.cex=0.75, 
         vertex.label.color="black",
         vertex.label.family="sans", 
         vertex.label.font=2)
    par(mar=c(5,0,2,2))
    barplot(ftab,horiz=TRUE,las=1,cex.names=0.75,cex.axis=0.75,
            col=as.character(col.df[col.df$Type %in% unlist(labels(ftab)),]$Color))
    par(def.par)
  }

  options("warn" = 0)

  return(g)
}

# ADDITIONAL HELPER FUNCTION MENTIONED IN THE BOOK BUT NOT PRINTED

graph.asn <- function(ips,alien.vault.df,add.peers=FALSE,show.plot=TRUE,show.labels=FALSE) {
  
  options("warn" = -1)
  
  # Lookup ASN info for the incoming IP list
  origin <- BulkOrigin(ips)
  
  if (add.peers) { # and peers if specified
    peers <- BulkPeer(ips)
  }
  
  # filter out IP and Type from the alienvault DB only for our ip list
  ips.types <- alien.vault.df[alien.vault.df$IP %in% ips,c(1,2,4)]
  
  # get a tabular summary of the types and counts
  ftab <- table(factor(ips.types[ips.types$IP %in% ips,]$Type))
  
  # build a color table from the tabular summary
  #myColors <- rainbow_hcl(length(names(ftab)),c=60,l=70,start=20)
  myColors <- set2
  col.df <- data.frame(Type=names(ftab),Color=myColors)
  
  # begin graph creation
  g <- graph.empty()
  
  # add our ip list as the starting vertices
  g <- g + vertices(ips,size=3,group=1)
  
  # i don't think the df is necessary anymore...will test later
  ips.df <- data.frame(ips)
  colnames(ips.df) <- c("IP")
  
  # get the current list of vertex names...i think i can remove this too
  v.names <- V(g)$name
  
  # assign colors to the vertices based on the type
  V(g)$color <- as.character(col.df[col.df$Type %in% ips.types[ips.types$IP %in% v.names,]$Type,]$Color)
  
  # add BGP->IP vertices and - if requested - add peer ASN vertices
  if (add.peers) { 
    g <- g + vertices(
      unique(c(peers$Peer.AS, origin$AS)),
      size=4,color="black",group=2)
  } else {
    g <- g + vertices(
      unique(origin$AS),
      size=4,color="black", group=2)
  }
  
  # Make IP/BGP edges
  ip.edges <- lapply(ips,function(x) {
    iAS <- origin[origin$IP==x,]$AS
    lapply(iAS,function(y){
      c(x,y)
    })
  })
  
  if (add.peers) { # same for peers if specified
    bgp.edges <- lapply(
      grep("NA",unique(origin$BGP.Prefix),value=TRUE,invert=TRUE),
      function(x) {
        startAS <- unique(origin[origin$BGP.Prefix==x,]$AS)
        lapply(startAS,function(z) {
          pAS <- peers[peers$BGP.Prefix==x,]$Peer.AS
          lapply(pAS,function(y) {
            c(z,y)
          })
        })
      })
  }
  
  # build ASN->IP edges
  g <- g + edges(unlist(ip.edges))
  
  if (add.peers) { # same for peers if specified
    g <- g + edges(unlist(bgp.edges))
  }
  
  # simplify the structure (prbly needed since it's already
  # well organized w/o dupes
  g <- simplify(g, edge.attr.comb=list(weight="sum"))
  
  # delete any standalone vertices (lone wolf ASNs)
  g <- delete.vertices(g, which(degree(g) < 1))
  
  # arrows: ugh
  E(g)$arrow.size <- 0
  
  # if we do show labels, we only want to see the ASNs
  V(g)[grep("\\.",V(g)$name)]$name <- ""
  
  # 10000 makes it pretty...and pretty slow
  L <- layout.fruchterman.reingold(g, niter=10000, 
                                   area=30*vcount(g)^2)
  
  # shld make an options parameter and if-block this
  c <- walktrap.community(g, steps=10)
  v <- evcent(g)$vector
  
  if (show.plot) {
    def.par <- par(no.readonly = TRUE)
    par(bg = 'white')
    layout(matrix(c(1,2,3,1), 1, 2, byrow = TRUE),
           widths=c(5,1))
    if (show.labels) {
      plot(g,
           margin=0,
           layout=L,
           vertex.label.dist=0.6, 
           vertex.label.cex=0.75, 
           vertex.label.color="black",
           vertex.label.family="sans", 
           vertex.label.font=2)
    } else {
      plot(g,margin=0,vertex.label=NA,layout=L)      
    }
    par(mar=c(5,0,2,2))
    barplot(ftab,horiz=TRUE,las=1,cex.names=0.75,cex.axis=0.75,
            col=as.character(
              col.df[col.df$Type %in% unlist(labels(ftab)),]$Color))
    par(def.par)
  }
  
  options("warn" = 0)
  return(g)
}

# Listing 4-16
# requires objects: BulkOrigin() & BulkPeer(), graph.cc(), graph.asn()
#   from book's web site & set2 (4-4)
# Working with Real Data
# create connected network of ZeuS IPs, ASNs, and ASN peers
# generates Figure 4-9
# require package: igraph, RColorBrewer
avRep <- "data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP", "Reliability", "Risk", "Type",
                     "Country", "Locale", "Coords", "x")

# read in list of destination IP addresses siphoned from firewall logs
dest.ips <- read.csv("data/dest.ips", col.names= c("IP"))

# take a look at the reliability of the IP address entries
# (you could also plot a histogram)
table(av.df[av.df$IP %in% dest.ips$IP, ]$Reliability)
##  1   2   3   4   5   6   7   8   9  10 
## 16 828 831 170   1 266  92   2  23  24 

# extract only the "bad" ones, designated by presence in alienvault
# database with a reliability greater than 6 since there seems to 
# be a trailing off at that point
ips <- as.character(av.df[(av.df$IP %in% dest.ips$IP) & 
                            (av.df$Reliability > 6), ]$IP)
# graph it
g.cc <- graph.cc(ips, av.df)





