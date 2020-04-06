### Plot fitness across training and test simulation

## Final product: generations on x axis and fitness on y axis
# Show clearly shift between training and test
# Have a different color for each type of test simulation (same problem/different problem)

library(tidyverse)
library(magrittr)

pheno_data <- read_csv(file = here::here("results/format_phenotypes/all_fitness_results.csv"),
                       col_types = "ffdffdiidd")

# reduce to one entry per individual (we remove data on phenotype in each environment)

fitness_data <- 
  pheno_data %>% 
  dplyr::filter(environment == 1) %>% 
  dplyr::select(
    -c(environment, trait)
  ) %>% 
  mutate(
    source_problem = fct_explicit_na(source_problem, na_level = 'training'),
    source_replicate = fct_explicit_na(source_replicate, na_level = 'training'),
    id = paste(source_problem, target_problem, source_replicate, target_replicate, source_generation, sep = ":")
  ) 

## Main test plot: mostly useful to see all data is here and formatted correctly
ggplot(data = fitness_data, 
       mapping = aes(x = target_generation, y = fitness, 
                     col = target_problem, group = id)) +
  geom_line() + geom_point() + 
  facet_wrap(source_problem~.) +
  scale_color_brewer(type = 'qual', palette = 3)

ggsave(filename = file.path(simulDir, "fitness.pdf"))

## Plot1: Compare AB, BA and NA/NB
# This plot shows that plasticity makes evolution irreversible
# Consider adding problem F to show that new step functions can evolve 
fitness_data %>% 
  filter(
    target_problem %in% c("a", "b", "n")
  ) %>% 
  ggplot(
  mapping = aes(
    x = target_generation,
    y = fitness,
    col = source_replicate,
    group = id)
) +
  geom_point() + 
  geom_line() + 
  facet_grid( source_problem ~ target_problem) +
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
