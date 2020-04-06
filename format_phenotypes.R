# Compile fitness and phenotype data for the entire experiment

# This script loads all PHE files and compiles them, annotating the relevant unique information
# Note that we filter only one individual per timepoint/environment combination
library(data.table)
library(tidyverse)
library(magrittr)

# Define conversion table from problem code to problem name
problem_codes <- 
  data.frame(
    problem_names = c("n", "a", "b", "d", "e", "f"),
    problem_codes = c("122345", "312452", "254213", "223344", "443322", "224411")
  )

# Define main simulation directory
simul_dir <- file.path("../Simulation_results")

## Import and annotate phenotype files from training simulation
train_dir <- dir(path = simul_dir, pattern = "[a-z]_train", full.names = T)
train_pheno <- list.files(path = train_dir, pattern = "PHE.*", full.names = T)
train_pheno_list <- lapply(
  X = train_pheno, 
  FUN = fread, 
  header = FALSE,
  col.names = c("replicate", "generation", "individual", "environment", "trait", "phenotype", "fitness")
) %>% 
  purrr::set_names(
    str_extract(string = train_pheno, pattern = "PHEN_TR_[^.]*")
  )

# Add columns source_problem, source_replicate, source_time, target_problem
train_pheno_data <-
  train_pheno_list %>% 
  bind_rows(., .id = "simulation_id") %>% 
  dplyr::filter(individual == 1) %>% 
  mutate(
    source_problem = NA,
    source_replicate = NA,
    source_generation = NA,
    target_replicate = replicate,
    target_generation = generation,
    target_problem = str_extract(string = simulation_id, pattern = "[0-9]{6}"),
    target_problem = factor(
      x = target_problem,
      levels = problem_codes$problem_codes, 
      labels = problem_codes$problem_names)
  ) %>% 
  dplyr::select(
    c("source_problem", "source_replicate", "source_generation", "target_problem", "target_replicate", "target_generation", "environment", "trait", "phenotype", "fitness")
  )

## Import and annotate phenotype files from test simulations

test_dir <- dir(path = simul_dir, pattern = "[a-z]_test", full.names = T)
test_pheno <- list.files(
  path = test_dir,
  pattern = "PHE.*", 
  full.names = T) 

test_pheno_list <- lapply(
  X = test_pheno, 
  FUN = fread, 
  header = FALSE,
  col.names = c("replicate", "generation", "individual", "environment", "trait", "phenotype", "fitness")
) %>% 
  purrr::set_names(
    str_extract(string = test_pheno, pattern = "PHE[^.]*")  
  )

# Add columns source_problem, source_replicate, source_time, target_problem
test_pheno_data <-
  test_pheno_list %>% 
  bind_rows(., .id = "simulation_id") %>% 
  dplyr::filter(individual == 1) %>% 
  tidyr::extract(
    col = "simulation_id",
    into = c("source_problem", "source_replicate", "source_generation", "target_problem"),
    regex = "PHE_([0-9]{6})_.*_R_?([0-9]{1,2})_T([0-9]{2})PHEN_TE_([0-9]{6}).*",
    remove = F
  ) %>% 
  mutate(
    target_generation = generation,
    target_replicate = replicate,
    source_problem = factor(
      x = source_problem, 
      levels = problem_codes$problem_codes, 
      labels = problem_codes$problem_names),
    target_problem = factor(
      x = target_problem,
      levels = problem_codes$problem_codes, 
      labels = problem_codes$problem_names),
    source_generation = 
      factor(x = as.integer(source_generation), levels = c(1:10), labels = unique(target_generation)) %>% 
      as.character %>% as.numeric,
    target_generation = target_generation %>% add(5e5)
  ) %>% 
  dplyr::select(
    c("source_problem", "source_replicate", "source_generation", "target_problem", "target_replicate", "target_generation", "environment", "trait", "phenotype", "fitness")
  )

## Merge files
pheno_data <- 
  rbind(train_pheno_data, test_pheno_data) 

## Save results
write_csv(x = pheno_data, path = here::here("results/format_phenotypes/all_fitness_results.csv"))

# Must be imported with read_csv(col_types = "ffdffdiidd")