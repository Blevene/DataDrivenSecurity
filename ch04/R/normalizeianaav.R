# Plot Charts from IANA data
# Flatten the data frame by making one entry per "count" type
# versus the counts in individual columns
# need 'melt()' function from reshape package
# to transform the data into a data frame

library(reshape)
library(grid)
library(gridExtra)

#normalize the IANA and AV values to % so bar chart scales
# match and make it easier to compare

combined.df$IANA.pct <- 100 * (combined.df$IANA.Block.Count / sum(combined.df$IANA.Block.Count))
combined.df$AV.pct <- 100 * (combined.df$AlienVault.IANA.Count / sum(combined.df$AlienVault.IANA.Count))
combined.df$IANA.vs.AV.pct <- combined.df$IANA.pct - combined.df$AV.pct