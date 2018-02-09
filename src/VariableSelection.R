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


## Write Functions ----

posterior_inclusion_probs <- function(fit) {
  return(colMeans(fit$var_counts > 0))
}

select_vars_softbart <- function(fit) {
  probs <- posterior_inclusion_probs(fit)
  selected <- which(probs >= 0.5)
  excluded <- which(probs < 0.5)

  return(list(selected = selected, excluded = excluded))
}

prec_rec_stats <- function(selected, truth) {
  true_positives <- sum(selected %in% truth)
  false_positives <- length(selected) - true_positives
  false_negatives <- length(truth) - true_positives
  precision <- (true_positives) / (true_positives + false_positives)
  recall <- (true_positives) / (true_positives + false_negatives)
  F_1 <- 2 * precision * recall / (precision + recall)

  return(list(true_positives = true_positives,
              false_positives = false_positives,
              false_negatives = false_negatives,
              precision = precision,
              recall = recall,
              F_1 = F_1))
}

test_prec_rec <- function() {
  selected <- c(1,2,3,5,6,7)
  truth <- c(1,2,3,4,5)
  test_out <- prec_rec_stats(selected, truth)
  stopifnot(test_out$true_positives == 4)
  stopifnot(test_out$false_positives == 2)
  stopifnot(test_out$false_negatives == 1)
  stopifnot(test_out$precision == 4 / 6)
  stopifnot(test_out$recall == 4/(4+1))
  print("All tests passed!")
  return(TRUE)
}

## ---- Data simulation functions

fried_2 <- function(x, lambda){
  10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2+lambda*10*x[,4]+5*lambda*x[,5]
}

simulate_fried_data <- function(p, n_train = 250, n_test = 1000, sigma = 1, lambda = 1) {

  X_train <- matrix(runif(n_train * p), nrow = n_train)
  mu_train <- fried_2(X_train, lambda)
  X_test <- matrix(runif(n_test * p), nrow = n_test)
  mu_test <- fried_2(X_test, lambda)
  Y_train <- mu_train + rnorm(n = n_train, mean = 0, sd = sigma)
  Y_test <- mu_test + rnorm(n = n_test, mean = 0, sd = sigma)

  return(list(X_train = X_train, Y_train = Y_train, mu_train = mu_train,
              X_test = X_test, Y_test = Y_test, mu_test = mu_test))

}

get_fried_stats_bart <- function(fit, truth) {
  selected <- select_vars_softbart(fit)$selected
  truth <- c(1,2,3,4,5)
  out <- prec_rec_stats(selected, truth)
  return(out)
}

## ---- Simulation study functions

fitter <- function(x,y,method) {
  out <- NULL
  if(method == "BART") out <- softbart(X = x,Y = y, X_test = x[1:5,],
                                       hypers = Hypers(x,y,width = 1E-20,
                                                       num_tree = 50),
                                       opts = Opts(update_tau = FALSE))
  if(method == "SoftBART") out <- softbart(X = x, Y = y, X_test = x[1:5,],
                                           hypers = Hypers(x,y, num_tree = 50))
  return(out)
}

sim_rep <- function(p, sigma, method, seed, lambda) {

  key <- paste0("VariableSelection", "P", p, "Method", method, "Seed",
                seed, "Lambda", lambda, ".RData")
  path <- paste0("./CacheVariableSelection/", key)
  if(file.exists(path)) {
    load(path)
    return(results)
  }

  set.seed(seed)
  datum <- simulate_fried_data(p = p, sigma = sigma, lambda = lambda)

  fit <- fitter(datum$X_train,datum$Y_train, method)

  results <- get_fried_stats_bart(fit, 1:5)
  save(results, file = path)

  return(get_fried_stats_bart(fit, 1:5))

}

sim_rep_ <- function(df) {
  lambda <- df$lambda
  seed <- df$seed
  method <- as.character(df$method)
  sim_rep(P, sigma, method, seed, lambda)
}

## Get the results ----

settings <- expand.grid(lambda = lambda_vec, seed = seed, method = c("BART", "SoftBART"))
settings_list <- split(settings, 1:NROW(settings))

results <- map_df(settings_list, .f = ~ as_tibble(sim_rep_(.x)))

results <- cbind(results, settings)

source("./src/PerformanceBySignal.R")

results_2 <- map_df(settings_list, .f = ~ as_tibble(sim_rep_(.x)))
names(results_2) <- "RMSE"

results <- cbind(results, results_2)
names(results)[9] <- "Method"

## Change "BART" to "DART" to reflect the fact that the Dirichlet shrinkage prior was used
levels(results$Method)[1] <- "DART"

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
                           y = RMSE,
                           color = Method,
                           shape = Method)) +
  geom_smooth(span = .3, se = FALSE) +
  stat_summary(fun.y = mean, geom = 'point') + theme_bw() +
  xlab("$\\lambda$")

grid.arrange(p_1, p_2, p_3, p_4, nrow = 2)
