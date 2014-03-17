#Extract longitutde/latitude pairs from AlienVault data
avRep <- "data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP", "Reliability", "Risk", "Type", "Country", "Locale", "Coords", "x")

#create a vector of lat/long data by splitting on "," in the Coords column
av.coords.vec <- unlist(strsplit(as.character(av.df$Coords), ","))
#convert the vector into a 2 column matrix
av.coords.mat <- matrix(av.coords.vec, ncol=2, byrow=TRUE)
#project into a data frame
av.coords.df <- as.data.frame(av.coords.mat)
#name columns
colnames(av.coords.df) <- c("lat","long")
# convert the chars to numerical values
av.coords.df$long <- as.double(as.character(av.coords.df$long))
av.coords.df$lat <- as.double(as.character(av.coords.df$lat))
