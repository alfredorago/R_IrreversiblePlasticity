### Import and visualize simulation results
library(plyr)

# Select which simulation to load
simulDir <- "./files"
simulID <- "PHEN_0000_C03"
simulFiles <- list.files(path = simulDir, pattern = simulID)
simulFiles

# Import all simulation to single data.frame
simulData <- ldply(lapply(X = file.path(simulDir, simulFiles), read.table))
names(simulData) <- c("Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")

head(simulData)
summary(simulData)

# Visualize fitness over time
library(ggplot2)
ggplot(data = simulData, mapping = aes(x = Generation, y = Fitness, group = Replicate)) +
  geom_smooth(method = "lm") + geom_point()

ggplot(data = simulData, mapping = aes(x = Generation, y = Phenotype, group = Replicate)) +
  geom_smooth(method = "lm") + geom_point() +
  facet_grid(Environment ~ Trait)