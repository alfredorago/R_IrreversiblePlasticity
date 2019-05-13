#### Plot reaction norms from GRNs

### Import library and define functions
library(tidyr)
library(plyr)
activation <- function(x){
  y <- tanh(x)
  y <- (y+1)/2
  y
}

### Define development
develop <- function(cues, epigen, grn, devtime, mzmat){
  # For every cue in our range, iterate hidden layer
  sapply(cues, function(c){
    for (i in 1:devtime) {
      epigen[1] <- (c + epigen[1])/2
      epigen[which(epigen<0)] <- 0
      epigen <- epigen %*% grn
      epigen <- activation(epigen)
      epigen[which(epigen<0)] <- 0
    }
    epigen
  }
  ) %>% 
    # Multiply final hidden layer states by output matrix
    apply(MARGIN = 2, function(x){mzmat %*% x}) %>%
    # convert to data.frame with rows named after inputs 
  matrix() %>% data.frame(row.names = round(cues, digits = 2))
}

### Functional test for development with pre-set parameters
devtime1 <- 20
cues1    <- seq(1, -1, length.out = 100)
ngenes   <- 4
epigen1  <- rep(0.5, ngenes)
mzmat1  <- rep(1/ngenes, ngenes) %>% matrix(nrow = 1)

### Import gene networks
GRN_files <- list.files(path = "../Simulation_results/20190513/Training_Problem_A", pattern = "GRN*", full.names = T)
# Extract weights for the first individual of each file, and store in list of lists (each file is nested within its simulation)
GRNs <- lapply(GRN_files, function(x){
    scan(file=x, what=numeric(), skip=13, n=ngenes^2) %>% matrix(nrow = ngenes)
})
names(GRNs) = str_extract(GRN_files, pattern = "R.[0-9]{1,}")

