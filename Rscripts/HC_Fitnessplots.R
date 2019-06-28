  ### Plot fitness across training and test simulation
  
  ## Final product: generations on x axis and fitness on y axis
  # Show clearly shift between training and test
  # Have a different color for each type of test simulation (same problem/different problem)
  
  library(plyr)
  library(stringr)
  library(data.table)
  library(ggplot2)
  
  # Define main simulation directory
  simulDir <- file.path("../Simulation_results/20190626/")
  
  ## Import and annotate phenotype files from training simulation
  trainDir <- dir(path = simulDir, pattern = ".0", full.names = T)
  trainPheno <- list.files(path = trainDir, pattern = "PHEN.*", full.names = T)
  trainPhenoList <- lapply(
    X = trainPheno, 
    FUN = fread, 
    header = FALSE,
    col.names = c("Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")
  )
  
  # Compose .id as problem1, problem2, source, replicate
  names(trainPhenoList) <- paste(
    str_extract(string = as.character(trainPheno), pattern = "(?<=[//])[A-Z]{1}0"),
    "R0", # No source replicate since training, keeping formatting constant  
    str_extract(string = as.character(trainPheno), pattern = "(?<=[_])R[0-9]{2}"),
    sep = "_"
  )
  
  trainPhenoData <- ldply(trainPhenoList)
  trainPhenoData$Training <- factor(x = rep(1, nrow(trainPhenoData)), levels = c(0,1))
  trainPhenoData$Problem1 <- factor(x = str_extract(string = trainPhenoData$.id, pattern = "^."), 
    levels = c("0" ,"A" ,"B" ,"D", "E", "F", "N"))
  trainPhenoData$Problem2 <- factor(x = str_extract(string = trainPhenoData$.id, pattern = "(?<=^[A-Z])."), 
    levels = c("0" ,"A" ,"B" ,"D", "E", "F", "N") )
  trainPhenoData$Source <- factor(str_extract(string = trainPhenoData$.id, pattern = "(?<=_)R[0-9]{1,2}"))
  
  ## Import and annotate phenotype files from test simulations
  
  testDir <- dir(path = simulDir, pattern = "[A-Z]$", full.names = T)
  replicates <- apply(expand.grid("R", 1:3), 1, paste, collapse="")
  testDir <- apply(expand.grid(testDir, replicates), 1, paste, collapse="/")
  testDir
  testPheno <- list.files(path = testDir,
    pattern = "PHEN.*", 
    full.names = T)
  testPhenoList <- lapply(
    X = testPheno, 
    FUN = fread, 
    header = FALSE,
    col.names = c("Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")
  )
  # Compose .id as problem1, problem2, source, replicate
  names(testPhenoList) <- paste(
    str_extract(string = as.character(testPheno), pattern = "(?<=[//])[A-Z]{2}"),
    str_extract(string = as.character(testPheno), pattern = "(?<=[//])R[0-9]{1,2}"), # assumes source populations to have index R1 to R99 
    str_extract(string = as.character(testPheno), pattern = "(?<=[_])R[0-9]{2}"),
    sep = "_"
    )
  
  testPhenoData <- ldply(testPhenoList)
  testPhenoData$Training <- factor(x = rep(0, nrow(trainPhenoData)), levels = c(0,1))
  testPhenoData$Problem1 <- factor(x = str_extract(string = testPhenoData$.id, pattern = "^."), 
    levels = c("0", "A","B", "D", "E", "F", "N"))
  testPhenoData$Problem2 <- factor(x = str_extract(string = testPhenoData$.id, pattern = "(?<=^[A-Z])."), 
    levels = c("0", "A","B", "D", "E", "F", "N") )
  testPhenoData$Source <- factor(str_extract(string = testPhenoData$.id, pattern = "(?<=_)R[0-9]{1,2}"))
  # Warning, code below assumes same number of generations for each test run
  testPhenoData$Generation <- testPhenoData$Generation+max(trainPhenoData$Generation)
  
  
  ## Merge files
  PhenoData <- rbind(trainPhenoData, testPhenoData)
  
  ## Filter only one individual per simulation per time point
  PhenoData <- PhenoData[which(PhenoData$Individual==1 & PhenoData$Environment==1),]
  PhenoData <- PhenoData[,c(".id", "Replicate", "Generation", "Fitness", "Training", "Problem1", "Problem2", "Source")]
  PhenoData <- PhenoData[which(duplicated(PhenoData[,c("Replicate", "Generation", "Training", "Problem1", "Problem2", "Source")])==FALSE),]
  
  # Annotate initial training with appropriate problem
  PhenoData$Problem2[which(PhenoData$Problem2=='0')] <- PhenoData$Problem1[which(PhenoData$Problem2=='0')]
  
  
  
  ## Main test plot: mostly useful to see all data is here and formatted correctly
  ggplot(data = PhenoData, 
    mapping = aes(x = Generation, y = Fitness, col = Problem2, group = .id)) +
    geom_line() + geom_point() + 
    facet_wrap(Problem1~.) +
    scale_color_brewer(type = 'qual', palette = 3)
  ggsave(filename = file.path(simulDir, "fitness.pdf"))

  ## Plot1: Compare AB, BA and NA/NB
  # This plot shows that plasticity makes evolution irreversible
  # Consider adding problem F to show that new step functions can evolve 
  dataPlot1 <- PhenoData[grep(pattern = "^[A,B,N][0,A,B]", x = PhenoData$.id),]
  ggplot(
    data = dataPlot1, 
    mapping = aes(
      x = Generation,
      y = Fitness,
      col = Problem2,
      group = .id)
  ) +
    geom_point() + 
    geom_line() + 
    facet_wrap(Problem1~.) +
    scale_color_brewer(type = "qual", palette = 3) 
  