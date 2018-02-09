library(ProjectTemplate)
library(parallel)
load.project()

source("./src/VariableSelection2.R")
cl <- makeCluster(NUM_CORES, type = 'FORK')

clusterEvalQ(cl, {source("./src/VariableSelection2.R")})

## ---- Global variables

settings <- expand.grid(lambda = lambda_vec, seed = seed, method = c("BART", "SoftBART"))
settings_list <- split(settings, 1:NROW(settings))

clusterApplyLB(cl, settings_list, sim_rep_)
