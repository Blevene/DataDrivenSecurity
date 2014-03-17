# Listing 9-0 #########################################################
# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/book/ch09")
# make sure the packages for this chapter
# are installed, install if necessary
pkg <- c("ggplot2", "RColorBrewer")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)  
}

# Listing 9-1 #########################################################
memproc <- read.csv("data/memproc.csv", header=T)
summary(memproc)
##          host          proc              mem               state
##  crisnd0004:  1   Min.   :-3.1517   Min.   :-3.5939   Infected: 53
##  crisnd0062:  1   1st Qu.:-1.2056   1st Qu.:-1.4202   Normal  :194
##  crisnd0194:  1   Median :-0.4484   Median :-0.6212              
##  crisnd0203:  1   Mean   :-0.4287   Mean   :-0.5181              
##  crisnd0241:  1   3rd Qu.: 0.3689   3rd Qu.: 0.2413              
##  crisnd0269:  1   Max.   : 3.1428   Max.   : 3.2184              
##  (Other)   :241

# Listing 9-2 #########################################################
# requires package : ggplot2
# requires object: memproc (9-1)
library(ggplot2)
gg <- ggplot(memproc, aes(proc, mem, color=state))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point(size=3) + theme_bw()
print(gg)

# Listing 9-3 #########################################################
# requires package : ggplot2
# requires object: memproc (9-1)
# make this repeatable
set.seed(1492)
# count how many in the overall sample
n <- nrow(memproc)
# set the test.size to be 1/3rd
test.size <- as.integer(n/3)
# randomly sample the rows for test set
testset <- sample(n, test.size)
# now split the data into test and train
test <- memproc[testset, ]
train <- memproc[-testset, ]

# Listing 9-4 #########################################################
# requires object: train (9-3)
# pull out proc and mem columns for infected then normal
# then use colMeans() to means of the columns
inf <- colMeans(train[train$state=="Infected", c("proc", "mem")])
nrm <- colMeans(train[train$state=="Normal", c("proc", "mem")])
print(inf)
##     proc      mem 
## 1.152025 1.201779 
print(nrm)
##       proc        mem 
## -0.8701412 -0.9386983 

# Listing 9-5 #########################################################
# requires object: inf (9-4), nrm (9-4)
predict.malware <- function(data) {
  # get 'proc' and 'mem' as numeric values
  proc <- as.numeric(data[['proc']])
  mem <- as.numeric(data[['mem']])
  # set up infected comparison
  inf.a <- inf['proc'] - proc
  inf.b <- inf['mem'] - mem
  # pythagorean distance c = sqrt(a^2 + b^2)
  inf.dist <- sqrt(inf.a^2 + inf.b^2)
  # repeat for normal systems
  nrm.a <- nrm['proc'] - proc
  nrm.b <- nrm['mem'] - mem
  nrm.dist <- sqrt(nrm.a^2 + nrm.b^2)
  # assign a label of the closest (smallest)
  ifelse(inf.dist<nrm.dist,"Infected", "Normal")
}
# could test with these if you uncomment them
# predict.malware(inf['proc'], inf['mem'])
# expect "Infected" 
# predict.malware(nrm['proc'], nrm['mem'])
# expect "Normal"

# Listing 9-6 #########################################################
# requires object: test (9-3), predict.malware (9-5)
prediction <- apply(test, 1, predict.malware)

# Listing 9-7 #########################################################
# requires object: test (9-3), prediction (9-6)
sum(test$state==prediction)/nrow(test)
## [1] 0.8780488


# Figure 9-2 #########################################################
# requires package : ggplot2
# requires object: test (9-3), prediction (9-6), inf (9-4), nrm (9-4)
slope <- -1*(1/((inf['mem']-nrm['mem'])/(inf['proc']-nrm['proc'])))
intercept <- mean(c(inf['mem'], nrm['mem'])) - (slope*mean(c(inf['proc'], nrm['proc'])))

result <- cbind(test, predict=prediction)
result$Accurate <- ifelse(result$state==result$predict, "Yes", "No")
result$Accurate <- factor(result$Accurate, levels=c("Yes", "No"), ordered=T)

gg <- ggplot(result, aes(proc, mem, color=state, size=Accurate, shape=Accurate))
gg <- gg + scale_shape_manual(values=c(16, 8))
gg <- gg + scale_size_manual(values=c(3, 6))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point() + theme_bw()
gg <- gg + geom_abline(intercept = intercept, slope = slope, color="gray80")
print(gg)

# Figure 9-3 #########################################################
# requires package : ggplot2
set.seed(1)
x <- runif(200, min=-10, max=10)
y <- 1.377*(x^3) + 0.92*(x^2) + .3*x + rnorm(200, sd=250) + 1572
x <- x + 10
smooth <- ggplot(data.frame(x,y), aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se=F) + 
  theme_bw()
print(smooth)

# Figure 9-4 #########################################################
# requires package : ggplot2
memproc <- read.csv("data/memproc.csv", header=T)
memproc$infected <- ifelse(memproc$state=="Infected", 1, 0)
set.seed(1492)
n <- nrow(memproc)
test.size <- as.integer(n/3)
testset <- sample(n, test.size)
test <- memproc[testset, ]
train <- memproc[-testset, ]

glm.out = glm(infected ~ proc + mem, data=test, family=binomial(logit))
summary(glm.out)
modelog <- predict.glm(glm.out, test, type="response")
gg <- ggplot(data.frame(x=modelog, y=ifelse(test$infected>0.5, "Yes", "No")), aes(x, y)) +
  geom_point(size=3, fill="steelblue", color="black", shape=4) + 
  ylab("Known Infected Host") +
  xlab("Estimated Probability of Infected Host") + theme_bw()
print(gg)

# Figure 9-5 #########################################################
set.seed(1) # repeatable
x <- c(rnorm(200), rnorm(400)+2, rnorm(400)-2)
y <- c(rnorm(200), rnorm(200)+2, rnorm(200)-2, rnorm(200)+2, rnorm(200)-2)
randata <- data.frame(x=x, y=y)
out <- list()
for(i in c(3,4,5,6)) {
  km <- kmeans(randata, i)
  centers <- data.frame(x=km$centers[ ,1], y=km$centers[ ,2], cluster=1)
  randata$cluster <- factor(km$cluster)
  gg <- ggplot(randata, aes(x, y, color=cluster)) + geom_point(size=2)
  gg <- gg + geom_point(data=centers, aes(x, y), shape=8, color="black", size=4)
  gg <- gg + scale_x_continuous(expand=c(0,0.1))
  gg <- gg + scale_y_continuous(expand=c(0,0.1))
  gg <- gg + ggtitle(paste("k-means with", i, "clusters"))
  gg <- gg + theme(panel.grid = element_blank(),
                   panel.background = element_rect(colour = "black", fill=NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   legend.position = "none",
                   axis.ticks = element_blank())
  out[[i-2]] <- gg
}
print(out[[1]])
print(out[[2]])
print(out[[3]])
print(out[[4]])


# Listing 9-8 #########################################################
# requires package : verisr (7-5)
# requires VCDB data from chapter 7 (see comments)
library(verisr)
# if you have grabbed the incidents from the VCDB repository at 
# https://github.com/vz-risk/VCDB you can set the directory to that
# Otherwise, this should reference the data from chapter 7
jsondir <- '../ch07/data/vcdb/'
# create a veris instance with the vcdb data
vcdb <- json2veris(jsondir)
# finally, you can convert veris object into a numeric matrix
vmat <- veris2matrix(vcdb)
# you may look at the size of
# the matrix with the dim() command
dim(vmat)
## [1] 1643  264

# Listing 9-9 #########################################################
# requires package : verisr (7-5), 
# requires object : vmat (9-8)
# now pull the column names and extract industries
vmat.names <- colnames(vmat)
industry <- vmat.names[grep('victim.industry', vmat.names)]
# "fold" the matrix on industries
# this pulls all the incidents for the industry
# and compresses so the proportions of the features are represented.
imat <- foldmatrix(vmat, industry, min=10, clean=T)
dim(imat)
## [1]  17 251

# Listing 9-10 #########################################################
# requires object : imat (9-9), vmat (9-8), 
# convert the distance matrix
idist <- dist(imat, method='canberra')
# run it through classical MDS
cmd <- cmdscale(idist)
# and take a look at the first few rows returned
head(cmd)
##                           [,1]       [,2]
## victim.industry2.32 -75.080869 -50.662403
## victim.industry2.33 -29.457487  -2.942502
## victim.industry2.42 -24.727909  21.751872
## victim.industry2.44   3.692422   7.840992
## victim.industry2.45 -18.855236  93.787627
## victim.industry2.48 -54.382350  23.166301


# Listing 9-11 #########################################################
# requires package : verisr (7-5), 
# requires object : cmd (9-10), vmat (9-8), 
# get the size of bubbles
ind.counts <- colSums(vmat[ , rownames(cmd)])
# extract the industry label
ind.label <- sapply(rownames(cmd), function(x) { 
  tail(unlist(strsplit(x, "[.]")), 1) 
})
# load up industry data, included with verisr package
data(industry2)
# create a new list of short tet
txt.label <- industry2$short[which(industry2$code %in% ind.label)]

# Listing 9-12 #########################################################
# requires package : ggplot2
# requires object : cmd (9-10), ind.counts, txt.label (9-11) 
library(ggplot2)
indf <- data.frame(x=cmd[ ,1], y=cmd[, 2], label=txt.label, size=ind.counts)
gg <- ggplot(indf, aes(x, y, label=label, size=size))
gg <- gg + scale_size(trans="log2", range=c(10,30), guide=F)
gg <- gg + geom_point(fill="lightsteelblue", color="white", shape=21)
gg <- gg + xlim(range(indf$x)*1.1) # expand x scale
gg <- gg + geom_text(size=4)
gg <- gg + theme(panel.grid = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank())
print(gg)

# Listing 9-13 #########################################################
# requires object : imat (9-9), txt.label (9-11)
# go back and relabel imat
rownames(imat) <- txt.label
# rerun idist
idist <- dist(imat, 'canberra')
# hclust couldn't be easier
hc <- hclust(idist) # , method="complete")
plot(hc)

# Listing 9-14 #########################################################
# requires package : ggplot2
# requires object : indf (9-12), hc (9-13)
# we can now cut off the heirarchical clustering at some level
# and use those levels to color the MDS plot
indf$cluster <- as.factor(cutree(hc, 6))
gg <- ggplot(indf, aes(x, y, label=label, size=size, fill=cluster))
gg <- gg + scale_size(trans="log2", range=c(10,30), guide=F)
gg <- gg + geom_point(color="gray80", shape=21)
gg <- gg + scale_fill_brewer(palette="Set2")
gg <- gg + xlim(range(indf$x)*1.06) # expand x scale
gg <- gg + geom_text(size=4)
gg <- gg + theme(panel.grid = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 legend.position="none",
                 axis.ticks = element_blank())

print(gg)