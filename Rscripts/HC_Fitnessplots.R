### Plot fitness across training and test simulation

## Final product: generations on x axis and fitness on y axis
# Show clearly shift between training and test
# Have a different color for each type of test simulation (same problem/different problem)

library(plyr)
library(stringr)


# Define main simulation directory
simulDir <- file.path("../Simulation_results/20190521/logit/")

## Import and annotate phenotype files from training simulation
trainDir <- dir(path = simulDir, pattern = ".0", full.names = T)
trainPheno <- list.files(path = trainDir, pattern = "PHEN.*", full.names = T)
trainPhenoList <- lapply(
  X = trainPheno, 
  FUN = fread, 
  header = FALSE,
  col.names = c("Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")
)
names(trainPhenoList) <- str_extract(string = as.character(trainPheno), pattern = "(?<=[//])[A-Z]0")
trainPhenoData <- ldply(trainPhenoList)
trainPhenoData$training <- factor(x = rep(1, nrow(trainPhenoData)), levels = c(0,1))
trainPhenoData$problem1 <- factor(x = str_extract(string = trainPhenoData$.id, pattern = "^."))
trainPhenoData$problem2 <- factor(x = str_extract(string = trainPhenoData$.id, pattern = ".$"), levels = c(0,"A","B") )

## Import and annotate phenotype files from test simulations
testDir <- dir(path = simulDir, pattern = "[^0]$", full.names = T)
testPheno <- list.files(path = testDir, pattern = "PHEN.*", full.names = T)
testPhenoList <- lapply(
  X = testPheno, 
  FUN = fread, 
  header = FALSE,
  col.names = c("Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")
)
names(testPhenoList) <- str_extract(string = as.character(testPheno), pattern = "(?<=[//])[A-Z]{2}")
testPhenoData <- ldply(testPhenoList)
testPhenoData$training <- factor(x = rep(0, nrow(trainPhenoData)), levels = c(0,1))
testPhenoData$problem1 <- factor(x = str_extract(string = testPhenoData$.id, pattern = "^."))
testPhenoData$problem2 <- factor(x = str_extract(string = testPhenoData$.id, pattern = ".$"), levels = c(0,"A","B") )
# Warning, code below assumes same number of generations for each test run
testPhenoData$Generation <- testPhenoData$Generation+max(trainPhenoData$Generation)

## Merge files
PhenoData <- rbind(trainPhenoData, testPhenoData)
PhenoData$Group <- factor(paste0(PhenoData$.id, PhenoData$Replicate))

## Filter only one individual per simulation per time point
PhenoData <- PhenoData[which(PhenoData$Individual==1),]
PhenoData <- PhenoData[,c(".id", "Replicate", "Generation", "Fitness", "training", "problem1", "problem2", "Group")]
PhenoData <- unique(PhenoData)
## Plot
ggplot(data = PhenoData, 
  mapping = aes(x = Generation, y = Fitness, col = problem2, group = Group)) +
  geom_line() +
  facet_grid(.~problem1)
ggsave(filename = file.path(simulDir, "fitness.pdf"))
