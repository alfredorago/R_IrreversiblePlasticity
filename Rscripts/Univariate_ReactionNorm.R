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
GRN_path <- file.path("../Simulation_results/20190524/E0/")
GRN_files <- list.files(path = GRN_path, pattern = "GRN*", full.names = T)
# Extract weights for the first individual of each file, and store in list of lists (each file is nested within its simulation)
GRNs <- lapply(GRN_files, function(x){
    scan(file=x, what=numeric(), skip=13, n=ngenes^2) %>% matrix(nrow = ngenes)
})
ReplicateID = str_extract_all(GRN_files, pattern = "R.[0-9]{1,}")
ReplicateID = sapply(ReplicateID, function(x){x[length(x)]})
Timepoint = str_extract_all(GRN_files, pattern = "T[0-9]{2}")
Timepoint = sapply(Timepoint, function(x){x[length(x)]})
names(GRNs) = paste(ReplicateID, Timepoint, sep = "_")

## Develop GRNs and store results in data.frame w one row per individual/repliate and one col per environment
phenotypes <- lapply(GRNs, function(x){
  develop(cues = cues1, epigen = epigen1, grn = x, devtime = devtime1, mzmat = mzmat1, linear = linear1)
})
phenotypes <- lapply(phenotypes, function(x){
  x$cues <- row.names(x)
  x
}) %>% ldply()
colnames(phenotypes) <-c("ID", "Value", "Cue")
phenotypes$Replicate <- as.numeric(str_extract(phenotypes$ID, pattern = "(?<=R_)[0-9]{1,2}"))
phenotypes$Timepoint <- as.numeric(str_extract(phenotypes$ID, pattern = "(?<=T)[0-9]{2}"))
#phenotypes$ID <- factor(phenotypes$ID, levels = c(unique(phenotypes$ID), "problem"))
phenotypes$Cue <- as.numeric(phenotypes$Cue)

## Import cue/target pairs for plotting
# Data are contained in lines 1 (targets) and 13 (cues)
problem = read.fwf(file = GRN_files[[1]], widths = c(31, 13, 17, 17, 17, 17, 17), n = 13)[c(13,1),]
problem = as.data.frame(t(problem[,-1]))
colnames(problem) <- c("Cue", "Value")
problem$ID <- "p"
problem$Replicate <- 0
problem$Timepoint <- -1

## Plot final reaction norms
ggplot(data = phenotypes, mapping = aes(x = Cue, y = Value, group = ID, col = Timepoint)) +
  geom_line(aes(alpha=0.5)) +
  geom_point(data = problem, mapping = aes(x=Cue, y=Value)) +
  ggtitle(basename(GRN_path)) +
  ylim(c(0,1)) +
  scale_color_viridis_c()

ggsave(
  filename = file.path(GRN_path, paste0(basename(GRN_path), '.pdf')), 
  device = "pdf")
