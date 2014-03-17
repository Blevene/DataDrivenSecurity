# URL for Alienvault IP Rep DB, this is a specific version
# to follow along with the book's analytics
avURL <- "http://datadrivensecurity.info/book/ch03/data/reputation.data"

# relative path to downloaded data
avRep <- "data/reputation.data"

#if wrapper to test if we need to download the file again
if (file.access(avRep)) {
  download.file(avURL,avRep)
}