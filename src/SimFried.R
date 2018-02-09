library(ProjectTemplate)
load.project()

## Parameters for the simulation ----
num_P <- 20
P_vec <- floor(exp(seq(from = log(5), to = log(1000), length = num_P))) + 1
NREP  <- 5
set.seed(1234)
seed  <- sample(1:1E6, size = NREP)
PIDX <- 1:20

## Function which runs the simulation ----

sim_exp <- function(str, p, n_rep = 5, n_train = 250,
                    n_test = 1000, sigma = 1, seed = NULL) {

  cache_str <- paste0("./CacheSimFried/SimFried", str, "P", p, "Sigma", sigma)
  if(file.exists(cache_str)) {
    load(cache_str)
    return(results)
  }

  f       <- get_f(str)
  results <- numeric(n_rep)

  ## Seeding
  if(is.null(seed)) seed <- sample(1:1E6, n_rep)

  for(i in 1:n_rep) {
    set.seed(seed[i])
    datum <- simulate_fried_data(p = p, n_train = n_train, n_test = n_test, sigma = sigma)
    results[i] <- with(datum, TrainTest(f = f, x_train = X_train, y_train = Y_train,
                                        x_test = X_test, y_test = mu_test))
    msg <- paste0("Finishing iteration ", i, " for P = ", p)
    print(msg)
  }

  save(results, file = cache_str)
  return(results)

}

f_1 <- function(str, sigma) {
  sapply(X = P_vec[PIDX],
         FUN = function(x) sim_exp(str, x, seed = seed, sigma = sigma))
}

results_softbart_1 <- f_1("softbart", 1.0)
results_xbart_1 <- f_1("xbart", 1.0)
results_lasso_1 <- f_1("lasso", 1.0)
results_rf_1 <- f_1("rf", 1.0)
results_xgb_1 <- f_1("xgb", 1.0)
results_dart_1 <- f_1("dart", 1.0)

results_softbart_10 <- f_1("softbart", sqrt(10))
results_xbart_10 <- f_1("xbart", sqrt(10))
results_lasso_10 <- f_1("lasso", sqrt(10))
results_rf_10 <- f_1("rf", sqrt(10))
results_xgb_10 <- f_1("xgb", sqrt(10))
results_dart_10 <- f_1("dart", sqrt(10))

## Plot the figure ------------------------------------------------------------


result_to_df <- function(result, method_name, sigma, P_vec) {
  rmse <- as.numeric(result)
  P <- rep(P_vec, each = nrow(result))
  return(tibble(RMSE = rmse, Method = method_name, sigma = sigma, P = P,
                sigma_sq_str = paste0("$\\sigma^2 = ", sigma, "$")))
}

df1 <- function(x, str) result_to_df(result = x, method_name = str,
                                     sigma = 1, P_vec = P_vec)

results_rf <- df1(results_rf_1, "Random Forest")
results_lasso <- df1(results_lasso_1, "Lasso")
results_dart <- df1(results_dart_1, "DART")
results_softbart <- df1(results_softbart_1, "SoftBART")
results_xbart <- df1(results_xbart_1, "BART-CV")
results_xgb <- df1(results_xgb_1, "Boosting")

df10 <- function(x, str) result_to_df(result = x, method_name = str,
                                      sigma = 10, P_vec = P_vec)

results_rf_ <- df10(results_rf_10, "Random Forest")
results_lasso_ <- df10(results_lasso_10, "Lasso")
results_dart_ <- df10(results_dart_10, "DART")
results_softbart_ <- df10(results_softbart_10, "SoftBART")
results_xbart_ <- df10(results_xbart_10, "BART-CV")
results_xgb_ <- df10(results_xgb_10, "Boosting")

results <- rbind(results_rf, results_rf_,
                 results_lasso, results_lasso_,
                 ## results_xgb, results_xgb_ 
                 ## results_xbart, results_xbart_,
                 ## results_dart, results_dart_,
                 results_softbart, results_softbart_
                 )

p <- ggplot(results, aes(x = P, y = RMSE, color = Method, shape = Method)) 
p <- p + geom_smooth() + facet_wrap(~sigma) + scale_x_log10() + ylim(0, 3.8)
p <- p + stat_summary(fun.y = mean, geom = "point", size = 2)
p + theme_bw()
