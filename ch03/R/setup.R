# Make sure the packages for this chapter are installed
# Install them if necessary
pkg <- c("ggplot2", "scales", "maptools", "sp", "maps", "grid", "car" )
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

