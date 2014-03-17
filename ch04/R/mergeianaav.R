# Code to extract IANA block assignments and compare w/AlienVault groups
# create a new dataframe from the iana designation factors

iana.df <- data.frame(table(iana$Designation))
colnames(iana.df) <- c("Registry", "IANA.Block.Count")

# make a data frame of the counts of the av iana designation factor
tmp.df <- data.frame(table(factor(av.df$Designation)))
colnames(tmp.df) <- c("Registry", "AlienVault.IANA.Count")

# merge (join) the data fames on the reg column
combined.df <- merge(iana.df, tmp.df)
# display the new data frame in order of IANA Block Count (highest -> lowest)
print(combined.df[with(combined.df, order(-IANA.Block.Count)),],
      row.names=FALSE)