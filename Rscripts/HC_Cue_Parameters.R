#### Parameter space analysis for env cues in memoryless RNN with logistic activation and HC

## Initialize packages
library(plyr)
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(reshape2)

## Initialize output path
graphdir <- file.path(getwd(), "Graphics/HC_Cue_Parameters")
dir.create(graphdir)
# Store image size in mm
a4 = c(201, 297)

## Import simulation results
ResultDirs <- list.dirs(path = "../../Simulation_results/20190318", full.names = T)[-1]

# For each folder, compile all replicates in a single data.frame within a list
Phenotypes <- lapply(ResultDirs, function(x){
  filenames <- list.files(x, pattern="PHEN*")
  filenames <- file.path(x, filenames)
  dtable <- bind_rows(lapply(filenames, fread))
})

# Convert list to named data.frame
names(Phenotypes) <- str_extract(ResultDirs, pattern = ".{4}$")
Phenotypes <- ldply(Phenotypes)
colnames(Phenotypes) <- c("Cue", "Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")
Phenotypes$Cue <- as.ordered(as.numeric(Phenotypes$Cue))
Phenotypes$Trait <- as.factor(Phenotypes$Trait)
head(Phenotypes)

## Trait values over time/cues
Traits <- Phenotypes[which(Phenotypes$Individual==1), ]

ggplot(data = Traits, mapping = aes(x = Generation, y = Phenotype, col = Cue)) +
  geom_point() +
  geom_smooth() + 
  facet_wrap(Trait~.) +
  ggtitle(label = "Trait evolution over time")
ggsave(file.path(graphdir, "TraitOverTime.pdf"), height = a4[1], width = a4[2], units = 'mm')

ggplot(data = Traits, mapping = aes(x = Cue, y = Phenotype)) +
  geom_point() +
  geom_smooth() + 
  facet_grid(Trait~Generation) +
  ggtitle(label = "Trait for each cue value, at different time intervals")
ggsave(file.path(graphdir, "TraitCueValues.pdf"), height = a4[1], width = a4[2], units = 'mm')

## Plot fitness over time for each simulation
Fitness = Phenotypes[,c("Cue", "Replicate", "Generation", "Fitness")]
Fitness = unique(Fitness)

ggplot(data = Fitness, mapping = aes(x = Generation, y = Fitness, col = Cue)) + 
  geom_point() + 
  geom_smooth(mapping = aes(group = Cue)) + 
  scale_y_log10() +
  ggtitle("Fitness over time with different environmental cues")
ggsave(filename = file.path(graphdir, "FitnessOverTime.pdf"), height = a4[1], width = a4[2], units = 'mm')

ggplot(data = Fitness, mapping = aes(x = Cue, y = Fitness)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(Generation~.) + 
  scale_y_log10() +
  ggtitle("Fitness for each cue value, at different time intervals")
ggsave(filename = file.path(graphdir, "FitnessCueValues.pdf"), height = a4[1], width = a4[2], units = 'mm')



## Read connection weights
# Store filenames for all GRNs
GRNs <- lapply(ResultDirs, function(x){
  list.files(x, pattern = "GRN*", full.names = T)
})
# Extract weights for the first individual of each file, and store in list of lists (each file is nested within its simulation)
Weights <- lapply(GRNs, function(x){
  sapply(x, function(y){
    scan(file=y, what=numeric(), skip=12, n=4)
  })
})

names(Weights) = str_extract(ResultDirs, pattern = ".{4}$")

# Convert in list of data.frames, with filename as row id and 1 column per weight
Weights <- lapply(Weights, function(x){ 
  as.data.frame(t(x))
})
# Convert in single data.frame, with first column identifying input cue
Weights <- ldply(Weights)
names(Weights) <- c("Cue", "W1", "W2", "W3", "W4")
# Merge with Fitness data
Fitness <- cbind(Fitness, Weights[,-1])
# Melt into tall format
Fitness = melt(data = Fitness, id.vars = c("Cue", "Replicate", "Generation", "Fitness"), variable.name = "W_ID", value.name = "W_Magnitude")

# Plot weights over time
ggplot(data = Fitness, mapping = aes(x = Generation, y = W_Magnitude, col = W_ID)) + 
  geom_point() +
  geom_line() +
  facet_grid(Replicate~Cue)+
  ggtitle("Evolution of Weights across time for different \n cue values (columns) and replicates (rows)")
ggsave(filename = file.path(graphdir, "WeightsOverTime.pdf"), height = a4[1], width = a4[2], units = 'mm')
