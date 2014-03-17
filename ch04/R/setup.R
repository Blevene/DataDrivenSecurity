#set working directory
setwd("~/Documents/DataDrivenSecurity/ch04")

#make sure packages are installed
pkg <- c("bitops", "ggplot2", "maps", "maptools",
         "sp", "grid", "car")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

