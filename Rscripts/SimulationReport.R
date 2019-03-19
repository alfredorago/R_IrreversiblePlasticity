### Import and visualize simulation results
library(plyr)

# Select which simulation to load
# Currently loads entire directory, separating training and test by suffix
simulDir <- "../../Simulation_results/20190215/Homeostat_POP_hiSS/"
simulID <- c("PHEN_TR", "PHEN_TE")
simulFiles <- lapply(X = simulID, FUN = function(x){list.files(path = simulDir, pattern = x)})
simulFiles <- lapply(simulFiles, FUN = function(x){file.path(simulDir, x)})
names(simulFiles) <- c("TR","TE")

# Import all simulation to single data.frame
simulData <- lapply(simulFiles, FUN = function(x){
  ldply(.data = x, .fun = read.table)
})
simulData <- ldply(simulData)
names(simulData) <- c("Training", "Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")

simulData[,c(1:2,4:6)] <- lapply(simulData[,c(1:2,4:6)], factor)

head(simulData)
summary(simulData)

# Plot simulation Results
library(ggplot2)
library(reshape2)
graphdir<-file.path(getwd(), "Graphics", "SimulationReport")
dir.create(graphdir, recursive = T)

# Visualize fitness over time
ggplot(data = simulData[which(simulData$Generation!=1),],
  mapping = aes(x = Generation, y = Fitness)) +
  geom_violin(aes(group = as.factor(Generation))) +
  facet_wrap(~Replicate) +
  ylim(c(0, max(simulData$Fitness)))
ggsave(filename = file.path(graphdir, "FitnessOverTime.pdf"), device = 'pdf')

# Visualize phenotypes over time
ggplot(data = simulData, mapping = aes(x = Generation, y = Phenotype)) +
  geom_violin(aes(group = as.factor(Generation))) +
  facet_grid(Replicate ~ Environment + Trait)
ggsave(filename = file.path(graphdir, "PhenotypesOverTime.pdf"), device = 'pdf')


# Visualize Phenotype correlations over time
multiData = simulData
multiData$Trait = as.factor(
  paste("Trait", multiData$Trait, sep = "_")
)
multiData = melt(
  multiData, 
  id.vars = c("Training", "Replicate", "Generation", "Individual", "Environment", "Trait", "Fitness"))
multiData = dcast(
  multiData, 
  formula = Training + Replicate + Generation + Individual + Environment + Fitness ~ Trait)

head(multiData)

ggplot(
  data = multiData, 
  mapping = aes(x = Trait_1, y = Trait_2, col = as.factor(Environment))
) +
  geom_point() + 
  facet_grid(Generation ~ Replicate)
ggsave(filename = file.path(graphdir, "2D_evolution.pdf"), device = "pdf", width = 14, height = 10)

# Animate
library(gganimate)

HC_anim <- ggplot(
  data = multiData, 
  mapping = aes(x = Trait_1, y = Trait_2, col = as.factor(Environment))
) +
  geom_point() +
  facet_wrap(~Replicate) +
  transition_time(Generation) +
  ease_aes('linear') +
  labs(title = "Generation: 'Generation'", x = "Trait 1", y = "Trait 2", col = "Environment") 

animate(HC_anim, duration = 20, fps = 24, rewind = T, width = 500, height = 400, res = 120)

anim_save(filename = "Homeostat2.gif", path = graphdir)

# Draw population ellipsoids
ggplot(
  data = multiData, 
  mapping = aes(x = Trait_1, y = Trait_2, col = as.factor(Environment))
) +
  stat_ellipse(mapping = aes(group = as.factor(Environment))) + 
  facet_grid(Replicate ~ as.factor(Generation) )
ggsave(filename = file.path(graphdir, "Ellipsoids.pdf"), width = 10, height = 5)


## Apply within-group centering and re-plot 
# checks only changes in ellipsoid shape rather than position
multiDataZ <- ddply(
  .data = multiData, 
  .variables = .(Replicate),
  .fun = summarize, 
  Generation = Generation,
  Environment = Environment,
  Individual = Individual,
  Trait_1 = Trait_1 - mean(Trait_1),
  Trait_2 = Trait_2 - mean(Trait_2)
)  

ggplot(
  data = multiDataZ, 
  mapping = aes(x = Trait_1, y = Trait_2, col = as.factor(Environment))
) +
  geom_point(alpha = 0.1) + 
  stat_ellipse(mapping = aes(group = as.factor(Environment))) + 
  facet_grid(Replicate ~ as.factor(Generation)) +
  scale_x_continuous(limits = c(-.25,.5)) +
  scale_y_continuous(limits = c(-.5,.25)) +
  theme_bw()
ggsave(filename = file.path(graphdir, "Ellipsoids_rescaled.pdf"), width = 10, height = 5)

# Average and best phenotype and fitness over each time step
avgData = ddply(
  .data = simulData, 
  .variables = .(Replicate, Generation, Environment, Trait), 
  .fun = summarize,
  PhenMean = mean(Phenotype),
  PhenSd = sd(Phenotype),
  FitnessMean = mean(Fitness),
  FitnessSd = sd(Fitness)
)

# Plot average fitness with 1 SD around it
ggplot(data = avgData, 
  mapping = aes(x = Generation, y = FitnessMean)) +
  geom_pointrange(mapping = aes(ymin = FitnessMean - FitnessSd, ymax = FitnessMean + FitnessSd)) +
  geom_line() +
  facet_grid(Environment ~ Replicate) +
  scale_y_log10()

# Split population averages by trait
avgMultiData = avgData
avgMultiData$Trait = as.factor(
  paste("Trait", avgMultiData$Trait, sep = "_")
)

avgMultiData = melt(
  avgMultiData, 
  id.vars = c("Replicate", "Generation", "Environment", "Trait", "FitnessMean", "FitnessSd"))
avgMultiData = dcast(
  avgMultiData, 
  formula = Replicate + Generation + Environment + FitnessMean + FitnessSd ~ Trait + variable)

head(multiData)