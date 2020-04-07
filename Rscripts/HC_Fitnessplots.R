### Plot fitness across training and test simulation

## Final product: generations on x axis and fitness on y axis
# Show clearly shift between training and test
# Have a different color for each type of test simulation (same problem/different problem)

library(tidyverse)
library(magrittr)

fitness_data <- 
  read_csv(file = here::here("results/format_phenotypes/fitness_results.csv"),
           col_types = "fidffid") %>% 
  mutate(
    source_problem = fct_explicit_na(source_problem, na_level = 'training') %>% 
      fct_relevel(c("training", "n", "a", "b")),
    source_replicate = ifelse(is.na(source_replicate), target_replicate, source_replicate) %>% as.factor(),
    source_generation = ifelse(is.na(source_generation), 0, source_generation),
    target_generation_2 = target_generation - source_generation,
    target_replicate = target_replicate %>% as.factor(),
    source_id = paste(source_problem, source_replicate, sep = ":"),
    target_id = paste(target_problem, target_replicate, sep = ":"),
    simulation_id = paste(source_problem, source_id, target_problem, target_id, sep = ":")
  ) 

## Plot1: Compare AB, BA and NA/NB
# This plot shows that plasticity makes evolution irreversible
# Consider adding problem F to show that new step functions can evolve 
fitness_data %>% 
  filter(
    target_problem %in% c("a", "b", "n"),
    source_generation == 500000 | source_generation == 0
  ) %>% 
  ggplot(
  mapping = aes(
    x = target_generation_2,
    y = fitness,
    group = simulation_id,
    col = source_replicate)
) +
  geom_smooth(se = FALSE, alpha = 0.5) + 
  geom_point(alpha = 0.7) + 
  facet_grid(target_problem ~ source_problem) +
  theme_linedraw() + 
  scale_x_log10() +
  theme(panel.grid = element_blank(), panel.border = element_blank())

ggsave(filename = file.path(simulDir, "Fig1_FitnessABBA.pdf"),
       device = "pdf", width = 297, height = 210, units = "mm"  
)

# Test if there is a relationship between fitness in training and final fitness in testing
train_fitness <-
  fitness_data %>% 
  filter(source_problem == 'training') %>% 
  dplyr::select(starts_with("target"), fitness) %>% 
  dplyr::select(-target_generation_2) %>% 
  dplyr::select(-ends_with("id")) %>% 
  rename_at(.tbl = ., 
            .vars = vars(starts_with("target")), 
            .funs = ~ str_replace(string = .x, "target_", "")
  ) %>% 
  rename_all(.tbl = .,
             .funs = ~ paste("training", .x, sep = "_"))

test_fitness <-
  fitness_data %>% 
  filter(source_problem != 'training' & target_generation_2 == 5e5) %>% 
  dplyr::select(-starts_with("target_generation")) %>% 
  dplyr::select(-ends_with("id")) %>% 
  rename_at(.tbl = ., 
            .vars = vars(starts_with("source_")), 
            .funs = ~ str_replace(string = .x, "source", "training")
            ) %>% 
  rename_at(.tbl = ., 
            .vars = vars(starts_with("target_")), 
            .funs = ~ str_replace(string = .x, "target", "testing")
  ) %>% 
  rename("testing_fitness" = "fitness")

matched_fitness <- 
  right_join(train_fitness, test_fitness)

ggplot(
  data = matched_fitness,
  mapping = aes(
    x = training_fitness,
    y = testing_fitness,
    col = as.factor(training_generation)
  )
) +
  geom_point() + geom_smooth() +
  facet_grid(training_problem ~ testing_problem)

# Compare training vs testing fitness over time
matched_fitness %>% 
  pivot_longer(data = ., 
               cols = c(training_fitness, testing_fitness), 
               names_to = "training", values_to = "fitness", names_pattern = "(.*)_fitness"
  ) %>% 
  ggplot(
  data = .,
  mapping = aes(
    x = training_generation + 1,
    y = fitness,
    col = training,
    # group = paste(training_replicate, testing_replicate, training, sep = ":")
  )
) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = F, alpha = 0.3) +
  scale_x_log10() +
  facet_grid(training_problem ~ testing_problem, switch = "y") +
  labs(
    title = 
      "Fitness in the training simulations (pink) compared with maximum fitness the trained organism can reach
    after 5e5 generations in the corresponding test environment (blue)",
    x = "Traning generations",
    y = "Fitness"
  )

ggsave(filename = here::here("results/HC_Fitnessplots/train_vs_test_finalpoint.pdf"), 
       width = 10)

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
