  ### Plot fitness across training and test simulation
  
  ## Final product: generations on x axis and fitness on y axis
  # Show clearly shift between training and test
  # Have a different color for each type of test simulation (same problem/different problem)
  
  library(data.table)
  library(tidyverse)
  
  # Define conversion table from problem code to problem name
  problem_codes <- 
    data.frame(
      problem_names = c("n", "a", "b", "d", "e", "f"),
      problem_codes = c("122345", "312452", "254213", "223344", "443322", "224411")
    )
  
  # Define main simulation directory
  simulDir <- file.path("../Simulation_results")
  
  ## Import and annotate phenotype files from training simulation
  trainDir <- dir(path = simulDir, pattern = "[a,b,n]_train", full.names = T)
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
  
  testDir <- dir(path = simulDir, pattern = "[a,b,n]_test", full.names = T)
  testPheno <- list.files(path = testDir,
    pattern = "PHEN.*", 
    full.names = T) %>% 
    magrittr::extract(1:10)
  
  testPhenoList <- lapply(
    X = testPheno, 
    FUN = fread, 
    header = FALSE,
    col.names = c("Replicate", "Generation", "Individual", "Environment", "Trait", "Phenotype", "Fitness")
  )
  
  # Compose .id as problem1, problem2, source, replicate
  names(testPhenoList) <- 
    testPheno %>% 
    str_extract(string = ., pattern = "PHE[^.]*")
  
  # Add columns source_problem, source_replicate, source_time, target_problem
  test_pheno_data <-
    testPhenoList %>% 
    bind_rows(., .id = "simulation_id") %>% 
    tidyr::extract(
      col = "simulation_id",
      into = c("source_problem", "source_replicate", "source_timepoint", "target_problem"),
      regex = "PHE_([0-9]{6})_C_1_(R[0-9,_]{2})_T([0-9]{2})PHEN_TE_([0-9]{6}).*",
      remove = TRUE
    ) %>% 
    mutate(
      source_problem = factor(
        x = source_problem, 
        levels = problem_codes$problem_codes, 
        labels = problem_codes$problem_names),
      target_problem = factor(
        x = target_problem,
        levels = problem_codes$problem_codes, 
        labels = problem_codes$problem_names)
    )
  
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
    scale_color_brewer(type = "qual", palette = 3) +
    theme_linedraw() + 
    theme(panel.grid = element_blank(), panel.border = element_blank()) 
  
  ggsave(filename = file.path(simulDir, "Fig1_FitnessABBA.pdf"),
    device = "pdf", width = 297, height = 210, units = "mm"  
  )

  ## Plot 2: Test set on new step function (F)  
  ## Need to run FF simulations
  dataPlot1 <- PhenoData[grep(pattern = "^[A,B,N,F][0,F]", x = PhenoData$.id),]
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
    scale_color_brewer(type = "qual", palette = 3) +
    theme_linedraw() + 
    theme(panel.grid = element_blank(), panel.border = element_blank()) 
  
  ggsave(filename = file.path(simulDir, "Fig2_FitnessF.pdf"),
    device = "pdf", width = 297, height = 210, units = "mm"  
  )
  
  ## Plot 3: Complex to linear, can improve but only if also congruent with biases
  ## Need D0 to DD 
  dataPlot1 <- PhenoData[grep(pattern = "^[A,B,N][0,D]", x = PhenoData$.id),]
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
    scale_color_brewer(type = "qual", palette = 3) +
    theme_linedraw() + 
    theme(panel.grid = element_blank(), panel.border = element_blank()) 
  
  ggsave(filename = file.path(simulDir, "Fig3_Fitness_complextolinear.pdf"),
    device = "pdf", width = 297, height = 210, units = "mm"  
  )
  
  ## Plot 4: linear to linear
  # Needs NE/ND
  dataPlot1 <- PhenoData[grep(pattern = "^[D,E][0,E,D]", x = PhenoData$.id),]
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
    scale_color_brewer(type = "qual", palette = 3) +
    theme_linedraw() + 
    theme(panel.grid = element_blank(), panel.border = element_blank()) 
  
  ggsave(filename = file.path(simulDir, "Fig4_Fitness_lineartolinear.pdf"),
    device = "pdf", width = 297, height = 210, units = "mm"  
  )
  