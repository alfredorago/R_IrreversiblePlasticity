#### Plot reaction norms from GRNs

### Import library and define functions
library(tidyr)
library(plyr)
library(ggplot2)
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

# ### Functional test for development with pre-set parameters
# devtime1 <- 20
# cues1 <- seq(1, -1, length.out = 10)/2
# grn1 <- matrix(nrow = 4, data = c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1))
# epigen1 <- rep(0.5, ncol(grn1))
# mzmat1 <- matrix(data = rep(1, nrow(grn1)), nrow = 1)
# develop(cues1, epigen1, grn1, devtime1, mzmat1)

### Define global parameter values
devtime1 <- 20
cues1    <- seq(0.5, -0.5, length.out = 100)
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


## Develop GRNs and store results in data.frame w one row per individual/repliate and one col per environment
phenotypes <- lapply(GRNs, function(x){
  develop(cues = cues1, epigen = epigen1, grn = x, devtime = devtime1, mzmat = mzmat1)
})
phenotypes <- lapply(phenotypes, function(x){
  x$cues <- row.names(x)
  x
}) %>% ldply()
colnames(phenotypes) <-c("Replicate", "Value", "Cue")
phenotypes$Cue <- as.numeric(phenotypes$Cue)

## Import cue/target pairs for plotting
# Data are contained in lines 1 (targets) and 13 (cues)
problem = read.fwf(file = GRN_files[[1]], widths = c(31, 13, 17, 17, 17, 17, 17), n = 13)[c(13,1),]
problem = as.data.frame(t(problem[,-1]))
colnames(problem) <- c("Cue", "Value")
problem$Replicate <- NA

## Plot final reaction norms
ggplot(data = phenotypes, mapping = aes(x = Cue, y = Value, group = Replicate)) +
  geom_line(aes(alpha=0.5)) +
  geom_point(data = problem, mapping = aes(x=Cue, y=Value))

