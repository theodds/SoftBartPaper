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

## ---- Data simulation functions

fried_2 <- function(x, lambda){
  10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2+lambda*10*x[,4]+5*lambda*x[,5]
}

simulate_fried_data_2 <- function(p, n_train = 250, n_test = 1000, sigma = 1, lambda = 1) {

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

fitter <- function(x,y,x_test,method) {
  out <- NULL
  if(method == "DART") out <- softbart(X = x,Y = y, X_test = x_test,
                                       hypers = Hypers(x,y,width = 1E-20,
                                                       num_tree = 50),
                                       opts = Opts(update_tau = FALSE))
  if(method == "SoftBART") out <- softbart(X = x, Y = y, X_test = x_test,
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
  datum <- simulate_fried_data_2(p = p, sigma = sigma, lambda = lambda)

  fit <- fitter(datum$X_train,datum$Y_train, datum$X_test, method)

  results <- get_fried_stats_bart(fit, 1:5)
  results$rmse <- rmspe(fit$y_hat_test_mean, datum$mu_test)
  save(results, file = path)

  return(results)
}

sim_rep_ <- function(df) {
  lambda <- df$lambda
  seed <- df$seed
  method <- as.character(df$method)
  sim_rep(P, sigma, method, seed, lambda)
}
