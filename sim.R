library(tidyverse)
source('functions.R')

main <- function(n_majority, n_minority, auc_majority, auc_minority, ratio_pos_case, 
                 n_replications=1000, return_both_aucs=FALSE) {
  abroca_null <- replicate(n_replications, {
    tryCatch({
      suppressMessages({simulate_abroca(n_majority=n_majority, 
                                            n_minority=n_minority,
                                            ratio_pos_case=ratio_pos_case,
                                            auc_majority=auc_majority, 
                                            auc_minority=auc_minority,
                                            return_both_aucs=return_both_aucs)})
    }, error=function(e){print(e)})
  })
}

results <- list()
for (ratio_minority in c(0.5)) {   # Specify these to simulate different conditions
  for (ratio_pos_case in c(0.5)) {
    for (auc_minority in c(0.8)) {
      for (n in c(seq(500, 1500, 500))) { 
        ref <- paste(ratio_minority, ratio_pos_case, auc_minority, n, sep='-')
        cat(format(Sys.time(), "%H:%M:%S"), '\n') # For timing the simulation, add a logger to these
        cat('Simulating for n =', ref, '\n')
        abrocas <- main(n_majority=n*(1-ratio_minority), n_minority=n*ratio_minority, 
                        auc_majority=0.8, auc_minority=auc_minority, 
                        ratio_pos_case=ratio_pos_case, 
                        n_replications=1000, 
                        return_both_aucs=TRUE) 
        results[[ref]] <- abrocas
      }
    }
  }
}

saveRDS(results, 'myoutput.rds')
