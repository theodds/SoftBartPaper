library(ProjectTemplate)
library(parallel)
load.project()

## ---- Global variables

set.seed(1234)
P          <- 100
num_lambda <- 20
sigma      <- 1
NREP       <- 20
seed       <- sample(1:1E6, size = NREP)
lambda_vec <- seq(from = .1, to = 1, length = num_lambda)
NUM_CORES  <- detectCores() - 1

cl <- makeCluster(NUM_CORES, type = 'SOCK')
clusterExport(cl, varlist = c("P", "num_lambda", "sigma",
                              "NREP", "seed", "lambda_vec"))
clusterEvalQ(cl, {library(ProjectTemplate); load.project()})

## Get the results ----

settings <- expand.grid(lambda = lambda_vec, seed = seed,
                        method = c("DART", "SoftBART"))
settings_list <- split(settings, 1:NROW(settings))

results_1 <- clusterApplyLB(cl, settings_list, sim_rep_)
results_2 <- map_df(settings_list, .f = ~ as_tibble(sim_rep_(.x)))

results <- cbind(results_2, settings)
names(results)[10] <- "Method"


p_1 <- ggplot(results, aes(x = lambda,
                           y = F_1,
                           color = Method,
                           shape = Method)) +
  geom_smooth(span = .5, se = FALSE) +
  stat_summary(fun.y = mean, geom = "point") + theme_bw() +
  xlab("$\\lambda$") + ylab("$F_1$")

p_2 <- ggplot(results, aes(x = lambda,
                           y = precision,
                           color = Method,
                           shape = Method)) +
  geom_smooth(span = 1, se = FALSE) +
  stat_summary(fun.y = mean, geom = 'point') + theme_bw() +
  xlab("$\\lambda$") + ylab("Precision")

p_3 <- ggplot(results, aes(x = lambda,
                           y = recall,
                           color = Method,
                           shape = Method)) +
  geom_smooth(span = .3, se = FALSE) +
  stat_summary(fun.y = mean, geom = 'point') + theme_bw() +
  xlab("$\\lambda$") + ylab("Recall")

p_4 <- ggplot(results, aes(x = lambda,
                           y = rmse,
                           color = Method,
                           shape = Method)) +
  geom_smooth(span = .3, se = FALSE) +
  stat_summary(fun.y = mean, geom = 'point') + theme_bw() +
  xlab("$\\lambda$")

grid.arrange(p_1, p_2, p_3, p_4, nrow = 2)
