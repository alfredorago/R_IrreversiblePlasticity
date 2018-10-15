### Import and visualize simulation results
library(plyr)

# Select which simulation to load
simulDir <- "./files"
simulID <- "PHEN_0000_C03"
simulFiles <- list.files(path = simulDir, pattern = simulID)
simulFiles

# Import all simulation to single data.frame
simulData <- ldply(lapply(X = file.path(simulDir, simulFiles), read.table))
head(simulData)
summary(simulData)

