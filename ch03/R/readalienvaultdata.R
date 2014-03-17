# read in the AV IP reputation db into a data frame
# since the file has no headers, we set header=FALSE
av <- read.csv(avRep,sep='#', header=FALSE)

#assign column names, since the default ones aren't descriptive
colnames(av) <- c("IP", "Reliability", "Risk", "Type", "Country", "Locale", "Coords", "x")

#overview of dataframe
#str(av)

#check out the first few lines
#head(av)

#evaluate range of Risk
#range(av$Risk)

#evaluate range of Reliability
#range(av$Reliability)

#mean of Reliability
#mean(av$Reliability)

# min, max, 1st quartile, median, mean, 3rd quartile, max of Reliability
# summary(av$Reliability)

# standard deviation of Reliability
# sd(av$Reliability)

# summary sorts counts by default, maxsum sets how many to display
# summary(av$Country, maxsum=40)

# summary of 'Type'
# summary(av$Type, maxsum=10)

#determine which countries contribute the most badness
#country10 <- summary(av$Country, maxsum=10)
#convert to a percentage
#country.perc10 <- country10/nrow(av)
#print the data
#print(country.perc10)

# Calculate a Three-way risk/reliability/type contigency table
library(lattice)
av$simpletype <- as.character(av$Type)
av$simpletype[grep(';', av$simpletype)] <- "Multiples"
av$simpletype <- factor(av$simpletype)

rrt.df = data.frame(table(av$Risk, av$Reliability, av$simpletype))
colnames(rrt.df) <- c("Risk", "Reliability", "simpletype", "Freq")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df,
          main= "Risk ~ Reliability | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))

#filter out the noisy scanning host type
rrt.df <- subset(rrt.df, simpletype != "Scanning Host")
levelplot(Freq ~ Reliability*Risk|simpletype, data = rrt.df,
          main="Risk ~ Reliability | Type", ylab ="Risk", xlab = "Reliability",
          shrink = c(0.5,1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))

#filter out the noisy Malware Domain type
rrt.df = subset(rrt.df,
                !(simpletype %in% c("Malware distribution",
                                    "Malware Domain")))
levelplot(Freq ~ Reliability*Risk|simpletype, data = rrt.df,
          main="Risk ~ Reliability | Type", ylab ="Risk", xlab = "Reliability",
          shrink = c(0.5,1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))