#### Plot reaction norms from GRNs

### Import library and define functions
library(tidyr)
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
cues1 <- seq(1, -1, length.out = 10)/2
grn1 <- matrix(nrow = 4, data = c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1))
epigen1 <- rep(0.5, ncol(grn1))
mzmat <- matrix(data = rep(1, nrow(grn1)), nrow = 1)
develop(cues1, epigen1, grn1, devtime1, mzmat)
