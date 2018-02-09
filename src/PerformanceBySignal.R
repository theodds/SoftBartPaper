library(ProjectTemplate)
load.project()

## Global variables ----

set.seed(1234)
P         <- 100
num_lambda <- 20
## sigma_vec <- sqrt(exp(seq(from = log(1), to = log(10), length = 20)))
sigma     <- 1
NREP      <- 20
seed      <- sample(1:1E6, size = NREP)
lambda_vec <- seq(from = .1, to = 1, length = num_lambda)

## Variables for this simulation ---- 

LAMBDA_IDX <- 1:num_lambda
# LAMBDA_IDX <- 1:5
# LAMBDA_IDX <- 6:10
# LAMBDA_IDX <- 11:15
# LAMBDA_IDX <- 16:20
method <- "SoftBART"

## Function for generate data ----


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

## Simulation study functions ----


fitter <- function(x,y,x_test,method) {
  out <- NULL
  if(method == "BART") out <- softbart(X = x,Y = y, X_test = x_test,
                                       hypers = Hypers(x,
                                                       y,
                                                       width = 1E-20,
                                                       num_tree = 50),
                                       opts = Opts(update_tau = FALSE))
  if(method == "SoftBART") out <- softbart(X = x, Y = y, X_test = x_test,
                                           ## opts = Opts(update_tau_mean = FALSE),
                                           hypers = Hypers(x,y, num_tree = 50)
                                           )
  return(out)
}

get_results <- function(fit, mu_test) {
  resid <- fit$y_hat_test_mean - mu_test
  return(sqrt(mean(resid^2)))
}

sim_rep <- function(p, sigma, method, seed, lambda) {

  ## Load cached result, if it exists ----
  key <- paste0("FriedSignal", "P", p, "Sigma", sigma, "Method", method, "Seed", seed, "Lambda", lambda, ".RData")
  path <- paste0("./CacheFriedSignal/", key)
  if(file.exists(path)) {
    load(path)
    return(results)
  }

  ## Otherwise, do simulation ----

  set.seed(seed)
  datum <- simulate_fried_data(p = p, sigma = sigma, lambda = lambda)

  fit <- fitter(datum$X_train, datum$Y_train, datum$X_test, method)

  results <- get_results(fit, datum$mu_test)
  save(results, file = path)

  return(results)

}

run_experiment <- function(p, sigma, seed_vec, method, lambda_vec) {
  out <- matrix(nrow = length(lambda_vec), ncol = length(seed_vec))
  for(i in 1:length(lambda_vec))
    for(j in 1:length(seed_vec))
      out[i,j] <- sim_rep(p, sigma, method, seed_vec[j], lambda_vec[i])
  return(out)
}

## Do experiment ----

# run_experiment(P, sigma, seed, method, lambda_vec[LAMBDA_IDX])

## Plot results ----

# out_softbart <- run_experiment(P, sigma_vec, seed, "SoftBART")
# out_bart <- run_experiment(P, sigma_vec, seed, "BART")
# 
# out_bart
# 
# results <- tibble(RMSE = c(out_bart, out_softbart),
#                   sigma = rep(rep(sigma_vec, length(seed)), 2),
#                   method = rep(c("BART", "SoftBART"),
#                                each = length(seed) * length(sigma_vec)))
# ggplot(results, aes(x = sigma, y = RMSE, color = method)) +
#   geom_smooth() +
#   theme_bw() +
#   stat_summary(aes(y = ..mean..), geom = 'point')
