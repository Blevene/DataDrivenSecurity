# Listing 3-0
# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/book/ch03")
# make sure the packages for this chapter
# are installed, install if necessary
pkg <- c("ggplot2", "lattice")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)  
}
	
# Listing 3-2
# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes. NOTE: we are using a specific version of the data
# in these examples, so we are pulling it from an alternate 
# book-specific location.
avURL <- 
  "http://datadrivensecurity.info/book/ch03/data/reputation.data"

# use relative path for the downloaded data
avRep <- "data/reputation.data"

# using an if{}-wrapped test with download.file() vs read.xxx()
# directly avoids having to re-download a 16MB file every time 
# we run the script
if (file.access(avRep)) {
  download.file(avURL, avRep) 
}

# Listing 3-4
# read in the IP reputation db into a data frame
# this data file has no header, so set header=FALSE
av <- read.csv(avRep,sep="#", header=FALSE)

# assign more readable column names since we didn’t pick
# any up from the header
colnames(av) <- c("IP", "Reliability", "Risk", "Type",
                  "Country", "Locale", "Coords", "x")

str(av) # get an overview of the data frame 
## 'data.frame': 258626 obs. of  8 variables:
## $ IP : Factor w/ 258626 levels "1.0.232.167",..: 154069 154065
##    154066 171110 64223 197880 154052 154051 154050 56741 ...
## $ Reliability: int 4 4 4 6 4 4 4 4 4 6 ...
## $ Risk : int 2 2 2 3 5 2 2 2 2 3 ...
## $ Type : Factor w/ 34 levels "APT;Malware Domain",..: 25 25 25 31 25
##    25 25 25 25 31 ...
## $ Country : Factor w/ 153 levels "","A1","A2","AE",..: 34 34 34 143
##    141 143 34 34 34 1 ...
## $ Locale : Factor w/ 2573 levels "","Aachen","Aarhus",..: 2506 2506
##    2506 1 1374 2342 2506 2506 2506 1 ...
## $ Coords : Factor w/ 3140 levels "-0.139500007033,98.1859970093",..:
##    489 489 489 1426 2676 1384 489 489 489 489 ...
## $ x : Factor w/ 34 levels "11","11;12","11;2",..: 1 1 1 7 1 1 1 1 1
##    7 ...

head(av) # take a quick look at the first few rows of data
##               IP Reliability Risk          Type Country     Locale
## 1 222.76.212.189           4    2 Scanning Host      CN     Xiamen
## 2 222.76.212.185           4    2 Scanning Host      CN     Xiamen
## 3 222.76.212.186           4    2 Scanning Host      CN     Xiamen
## 4    5.34.246.67           6    3      Spamming      US           
## 5  178.94.97.176           4    5 Scanning Host      UA     Merefa
## 6    66.2.49.232           4    2 Scanning Host      US Union City
##                         Coords  x
## 1   24.4797992706,118.08190155 11
## 2   24.4797992706,118.08190155 11
## 3   24.4797992706,118.08190155 11
## 4                   38.0,-97.0 12
## 5  49.8230018616,36.0507011414 11
## 6 37.5962982178,-122.065696716 11


# Listing 3-7
# require object: av (3-4)
summary(av$Reliability)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.000   2.000   2.000   2.798   4.000  10.000 

summary(av$Risk)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.000   2.000   2.000   2.221   2.000   7.000 

# Listing 3-9
# require object: av (3-4)
table(av$Reliability)
## 1      2      3      4      5      6      7      8      9 
## 5612 149117  10892  87040      7   4758    297     21    686 
## 10 
## 196 

table(av$Risk)
## 1      2      3      4      5      6      7 
## 39 213852  33719   9588   1328     90     10

# summary sorts by the counts by default
# maxsum sets how many factors to display
summary(av$Type, maxsum=10)
##                Scanning Host               Malware Domain 
##                       234180                         9274 
##                   Malware IP               Malicious Host 
##                         6470                         3770 
##                     Spamming                          C&C 
##                         3487                          610 
## Scanning Host;Malicious Host    Malware Domain;Malware IP 
##                          215                          173 
## Malicious Host;Scanning Host                      (Other) 
##                          163                          284 

summary(av$Country, maxsum=40)
##      CN      US      TR              DE      NL      RU      GB 
##   68583   50387   13958   10055    9953    7931    6346    6293 
##      IN      FR      TW      BR      UA      RO      KR      CA 
##    5480    5449    4399    3811    3443    3274    3101    3051 
##      AR      MX      TH      IT      HK      ES      CL      AE 
##    3046    3039    2572    2448    2361    1929    1896    1827 
##      JP      HU      PL      VE      EG      ID      RS      PK 
##    1811    1636    1610    1589    1452    1378    1323    1309 
##      VN      LV      NO      CZ      BG      SG      IR (Other) 
##    1203    1056     958     928     871     868     866   15136 

# Listing 3-11
# require object: av (3-4)
# We need to load the ggplot2 library to make the graphs
# See corresponding output in Figure 3-2
# NOTE: Graphing the data shows there are a number of entries without
#       a corresponding country code, hence the blank entry 
library(ggplot2)

# Bar graph of counts (sorted) by Country (top 20)
# get the top 20 countries' names
country.top20 <- names(summary(av$Country))[1:20]
# give ggplot a subset of our data (the top 20 countries) 
# map the x value to a sorted count of country
gg <- ggplot(data=subset(av,Country %in% country.top20), 
             aes(x=reorder(Country, Country, length)))
# tell ggplot we want a bar chart
gg <- gg + geom_bar(fill="#000099")
# ensure we have decent labels
gg <- gg + labs(title="Country Counts", x="Country", y="Count")
# rotate the chart to make this one more readable
gg <- gg + coord_flip()
# remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
# display the image
print(gg)


# Listing 3-12
# requires packages: ggplot2
# require object: av (3-4)
# See corresponding output in Figure 3-3
# Bar graph of counts by Risk
gg <- ggplot(data=av, aes(x=Risk))
gg <- gg + geom_bar(fill="#000099")
# force an X scale to be just the limits of the data
# and to be discrete vs continuous
gg <- gg + scale_x_discrete(limits=seq(max(av$Risk)))
gg <- gg + labs(title="'Risk' Counts", x="Risk Score", y="Count")
# remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)
                          
# Listing 3-13
# requires packages: ggplot2
# require object: av (3-4)
# See corresponding output in Figure 3-4
# Bar graph of counts by Reliability
gg <- ggplot(data=av, aes(x=Reliability))
gg <- gg + geom_bar(fill="#000099")
gg <- gg + scale_x_discrete(limits=seq(max(av$Reliability)))
gg <- gg + labs(title="'Reliabiity' Counts", x="Reliability Score",
                y="Count")
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)


# Listing 3-17
# require object: av (3-4)
country10 <- summary(av$Country, maxsum=10)
# now convert to a percentage by dividing by number of rows 
country.perc10 <- country10/nrow(av)
# and print it
print(country.perc10)
##         CN         US         TR                    DE         NL 
## 0.26518215 0.19482573 0.05396983 0.03887854 0.03848414 0.03066590 
##         RU         GB         IN    (Other) 
## 0.02453736 0.02433243 0.02118890 0.30793501 

# Listing 3-19
# require object: av (3-4)
# See corresponding output in Figure 3-8
# compute contingency table for Risk/Reliability factors which 
# produces a matrix of counts of rows that have attributes at
# each (x, y) location
rr.tab <- xtabs(~Risk+Reliability, data=av)
print(ftable(rr.tab)) # print table

# virtually identical output to pandas (below)

# graphical view
# need to use levelplot function from lattice package
library(lattice)
# cast the table into a data frame
rr.df = data.frame(table(av$Risk, av$Reliability))
# set the column names since table uses "Var1" and "Var2"
colnames(rr.df) <- c("Risk", "Reliability", "Freq")
# now create a level plot with readable labels
levelplot(Freq~Risk*Reliability, data=rr.df, main="Risk ~ Reliabilty", 
          ylab="Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


# Listing 3-21
# require object: av (3-4), lattice (3-19)
# See corresponding output in Figure 3-10
# generate random samples for risk & reliability and re-run xtab
# starting PRNG from reproducable point
set.seed(1492) # as it leads to discovery
# generate 260,000 random samples
rel=sample(1:7, 260000, replace=T)
rsk=sample(1:10, 260000, replace=T)
# cast table into data frame
tmp.df = data.frame(table(factor(rsk), factor(rel)))
colnames(tmp.df) <- c("Risk", "Reliability", "Freq")
levelplot(Freq~Reliability*Risk, data=tmp.df, main="Risk ~ Reliabilty", 
          ylab = "Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


# Listing 3-22
# require object: av (3-4), lattice (3-19)
# See corresponding output in Figure 3-11
# Create a new varible called "simpletype" 
# replacing mutiple categories with label of "Multiples"
av$simpletype <- as.character(av$Type)
# Group all nodes with mutiple categories into a new category
av$simpletype[grep(';', av$simpletype)] <- "Multiples"
# Turn it into a factor again
av$simpletype <- factor(av$simpletype)

rrt.df = data.frame(table(av$Risk, av$Reliability, av$simpletype))
colnames(rrt.df) <- c("Risk", "Reliability", "simpletype", "Freq")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20))


# Listing 3-24
# from the existing rrt.df, filter out 'Scanning Host'
rrt.df <- subset(rrt.df, simpletype != "Scanning Host")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20))

# Listing 3-26
# require object: av (3-4), lattice (3-19), rrt.df (3-24)
# See corresponding output in Figure 3-15
rrt.df = subset(rrt.df, 
                !(simpletype %in% c("Malware distribution",
                                    "Malware Domain")))
sprintf("Count: %d; Percent: %2.1f%%",
        sum(rrt.df$Freq),
        100*sum(rrt.df$Freq)/nrow(av))
## [1] Count: 15171; Percent: 5.9%

levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20)) 


## END OF CHAPTER 3 R CODE


