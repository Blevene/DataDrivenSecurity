# Requires frames from longlatv.R
# Incorporate IANA ipv4 allocations to augment IP Address Data
require(stringr)

ianaURL <- "http://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv"
ianaData <- "data/ipv4-address-space.csv"
if (file.access(ianaData)) {
  download.file(ianaURL, ianaData)
}
# read the iana file
iana <- read.csv(ianaData)

#clean up the iana prefeixes
iana$Prefix <- sub("^(00|0)", "", iana$Prefix, perl=TRUE)
iana$Prefix <- sub("/8$", "", iana$Prefix, perl=TRUE)

#define a function to strip 'n' character from a string (char vector)
#and return the shortened string.
#this function is vectorized, you can pass a single string or a vector at a time
rstrip <- function(x, n) {
  substr(x, 1, nchar(x)-n)
}

#extract only the prefix from the Alienvault list
av.IP.prefix <- rstrip(str_extract(as.character(av.df$IP),
                                   "^([0-9]+)\\."), 1)

# use 'sapply()' to apply a pattern
av.df$Designation <- sapply(av.IP.prefix, function(ip) {
  iana[iana$Prefix == ip, ]$Designation
})

#actually see the output
summary(factor(av.df$Designation))