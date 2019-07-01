#### Plot reaction norms from GRNs

### Import library and define functions
library(tidyr)
library(plyr)
library(ggplot2)
library(stringr)
activation <- function(x){
  y <- tanh(x)
  y <- (y+1)/2
  y
}

### Define development
develop <- function(cues, epigen, grn, devtime, mzmat, linear){
  # For every cue in our range, iterate hidden layer
  sapply(cues, function(c){
    for (i in 1:devtime) {
      transient <- epigen
      transient <- transient*1 # (decay term)
      transient[1] <- (c + transient[1])
      transient[2:length(transient)] <- transient[2:length(transient)]
      transient <- transient %*% grn
      epigen <- epigen + transient
      if (linear!=1){
        epigen <- activation(epigen)
        if(any(epigen<0)) 
          print("Error: Negative Concentrations")
      } 

    }
    epigen
  }
  ) %>% 
    # Multiply final hidden layer states by output matrix
    apply(MARGIN = 2, function(x){mzmat %*% x}) %>%
    # convert to data.frame with rows named after inputs 
  matrix() %>% data.frame(row.names = round(cues, digits = 2))
}

### Define global parameter values
devtime1 <- 20
cues1    <- seq(0.5, -0.5, length.out = 100)
ngenes   <- 4
epigen1  <- rep(0.5, ngenes)
mzmat1  <- rep(1/ngenes, ngenes) %>% matrix(nrow = 1)
linear1 <- 0

### Import gene networks
GRN_maindir <- file.path("../Simulation_results/20190626/")
GRN_path <- GRN_maindir %>% list.dirs(full.names = T)
GRN_files <- list.files(path = GRN_path, pattern = "GRN*", full.names = T)
# Extract weights for the first individual of each file, and store in list of lists (each file is nested within its simulation)
# This step requires ~6 seconds
GRNs <- lapply(GRN_files, function(x){
    scan(file=x, what=numeric(), skip=13, n=ngenes^2) %>% matrix(nrow = ngenes)
})

ProblemID = str_extract(string = GRN_files, pattern = "[A-Z][A-Z,0]")

ReplicateID = str_extract(string = GRN_files, pattern = "R_?[0-9]{1,2}(?=_T[0-9]{2}.dat)") %>%
  str_replace(pattern = "_", replacement = "0")

# Source currently depends on directory structure. Could theoretically work from filenames alone
Source = ifelse(grepl(pattern = "/R[0-9]/", x = GRN_files),
  str_extract(string = GRN_files, pattern = "(?<=[//])R[0-9]{1,2}"),
  "R0"
)
  
Timepoint = str_extract(GRN_files, pattern = "T[0-9]{2}(?=[/.dat])") 

names(GRNs) = paste(ProblemID, Source, ReplicateID, Timepoint, sep = "_")

## Develop GRNs and store results in data.frame w one row per individual/repliate and one col per environment
phenotypes <- lapply(GRNs, function(x){
  develop(cues = cues1, epigen = epigen1, grn = x, devtime = devtime1, mzmat = mzmat1, linear = linear1)
})
phenotypes <- lapply(phenotypes, function(x){
  x$cues <- row.names(x)
  x
}) %>% ldply()
colnames(phenotypes) <-c("ID", "Value", "Cue")

phenotype_annotation <- str_extract_all(string = phenotypes$ID, pattern = "[A-Z,0-9]{2,3}") %>%
  ldply()
colnames(phenotype_annotation) <- c("Problem", "Source", "Replicate", "Timepoint")
phenotype_annotation$Training <- factor(str_extract(string = phenotype_annotation$Problem, pattern = "^."))
phenotype_annotation$Test <- factor(str_extract(string = phenotype_annotation$Problem, pattern = ".$"))
phenotype_annotation$Timepoint <- as.numeric(str_extract(string = phenotype_annotation$Timepoint, pattern = "[0-9]."))

phenotypes <- cbind(phenotype_annotation, phenotypes)

phenotypes$Cue <- as.numeric(phenotypes$Cue)

## Import cue/target pairs for plotting
# Data are contained in lines 1 (targets) and 13 (cues)

problem <- lapply(X = GRN_files, FUN = function(x){
  read.fwf(x, widths = c(31, 13, 17, 17, 17, 17, 17), n = 13)[c(13,1),-1] %>%
    t() %>%
    as.data.frame()
})
names(problem) <- names(GRNs)
problem <- ldply(problem)
names(problem) <- c('ID', 'Cue', 'Value')
problem$Problem <- as.factor(str_extract(string = problem$ID, pattern = "[A-Z][A-Z,0]"))
problem <- unique(problem[,c('Cue','Value','Problem')])

problem_annotation <- unique(phenotype_annotation[,c("Problem", "Timepoint", "Training", "Test")])
problem <- merge(x = problem, y = problem_annotation, all.x = T, all.y = T, by = "Problem")

# extendedPhenotype <- merge(phenotypes, problem, by = 'ID', all.x = T, all.y = F)

## Plot final reaction norms
ggplot(data = phenotypes, mapping = aes(x = Cue, y = Value, group = ID)) +
  geom_line() +
  ggtitle(basename(GRN_path)) +
  ylim(c(0,1)) +
  scale_color_brewer(type = 'seq') +
  facet_grid(Training+Test~Timepoint) +
  geom_point(data = problem, mapping = aes(group = NA)) 

ggsave(
  filename = file.path(GRN_maindir, "ReactionNormMatrix.pdf"), 
  device = "pdf", width = 300, height = 300, units = "mm")

# Fig 1: RN ABBA
dataplot <- phenotypes[which(phenotypes$Training%in%c('A','B','N') & phenotypes$Test%in%(c('0','A','B','N'))),]
problemplot <- problem[which(problem$Training%in%c('A','B','N') & problem$Test%in%(c('0','A','B','N'))),]

ggplot(data = dataplot, mapping = aes(x = Cue, y = Value, group = ID, col = Timepoint)) +
  geom_line() +
  ylim(c(0,1)) +
  facet_grid(Training~Test, switch = 'y') +
  geom_point(data = problemplot, mapping = aes(group = NA, col = -10)) +
  scale_colour_distiller(type = 'div', palette = 1) +
  theme_light()

ggsave(
  filename = file.path(GRN_maindir, "RN_ABBA.pdf"), 
  device = "pdf", width = 297, height = 210, units = "mm")

# Fig 2: RN ABFN, shows that plasticity is not a dead end but can learn similar functions
dataplot <- phenotypes[which(phenotypes$Training%in%c('A','B','N','F') & phenotypes$Test%in%(c('0','F'))),]
problemplot <- problem[which(problem$Training%in%c('A','B','N', 'F') & problem$Test%in%(c('0','F'))),]

ggplot(data = dataplot, mapping = aes(x = Cue, y = Value, group = ID, col = Timepoint)) +
  geom_line() +
  ylim(c(0,1)) +
  facet_grid(Training~Test, switch = 'y') +
  geom_point(data = problemplot, mapping = aes(group = NA, col = -10)) +
  scale_colour_distiller(type = 'div', palette = 1) +
  theme_light()

ggsave(
  filename = file.path(GRN_maindir, "RN_ABFN.pdf"), 
  device = "pdf", width = 297, height = 210, units = "mm")

# Fig 3: RN ABDN, shows that complex to simple transitions constrain adaptation
dataplot <- phenotypes[which(phenotypes$Training%in%c('A','B','N','D') & phenotypes$Test%in%(c('0','D'))),]
problemplot <- problem[which(problem$Training%in%c('A','B','N', 'D') & problem$Test%in%(c('0','D'))),]

ggplot(data = dataplot, mapping = aes(x = Cue, y = Value, group = ID, col = Timepoint)) +
  geom_line() +
  ylim(c(0,1)) +
  facet_grid(Training~Test, switch = 'y') +
  geom_point(data = problemplot, mapping = aes(group = NA, col = -10)) +
  scale_colour_distiller(type = 'div', palette = 1) +
  theme_light()

ggsave(
  filename = file.path(GRN_maindir, "RN_ABFD.pdf"), 
  device = "pdf", width = 297, height = 210, units = "mm")

# Fig 3: RN EDDE, shows that even simple to simple transitions cause constraint (might want to add simple to complex)
dataplot <- phenotypes[which(phenotypes$Training%in%c('E','D') & phenotypes$Test%in%(c('0','D','E'))),]
problemplot <- problem[which(problem$Training%in%c('E','D') & problem$Test%in%(c('0','D','E'))),]

ggplot(data = dataplot, mapping = aes(x = Cue, y = Value, group = ID, col = Timepoint)) +
  geom_line() +
  ylim(c(0,1)) +
  facet_grid(Training~Test, switch = 'y') +
  geom_point(data = problemplot, mapping = aes(group = NA, col = -10)) +
  scale_colour_distiller(type = 'div', palette = 1) +
  theme_light()

ggsave(
  filename = file.path(GRN_maindir, "RN_EDDE.pdf"), 
  device = "pdf", width = 297, height = 210, units = "mm")
