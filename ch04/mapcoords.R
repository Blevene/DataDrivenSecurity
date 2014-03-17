#Requires object created by longlatav.R
# R code to extrract long/lat pairs from alienvault data 
# needed plotting and mapping functions
library(maps)
library(ggplot2)
library(RColorBrewer)
library(scales)

#extract a color pallette from RColorBrewer
set2 <- brewer.pal(8,"Set2")

#extract polygon information for world map, don't include antarctica
world <- map_data('world')
world <- subset(world, region != "Antarctica")

#Plot the map with points marking lat/long of geocoded entries

gg <- ggplot()
gg <- gg + geom_polygon(data=world, aes(long, lat, group=group),
                        fill="white")
gg <- gg + geom_point(data=av.coords.df, aes(x=long, y=lat),
                      color=set2[2], size=1, alpha=0.1)
gg <- gg + labs(x="", y="")
gg <- gg + theme(panel.background=element_rect(fill=alpha(set2[3],0.2),colour='white'))
gg