library(ProjectTemplate)
load.project()

SEED <- 1234
R <- 20
set.seed(SEED)
seed <- sample(1:99999, R)

f_compare <- function(x, y, K = 5, seed, method, data_name, loss = rmspe) {

  num_rep <- length(seed)
  results <- matrix(NA, K, num_rep)

  f <- get_f(method)

  for(r in 1:num_rep) {
    set.seed(seed[r])
    key <- paste0("Data", data_name, "Seed", seed[r], "Method", method)
    path <- paste0("./CacheRealData/", key, ".RData")
    if(file.exists(path)) {
      load(path)
    } else {
      out <- numeric(K)
      cv_idx <- createFolds(y, k = K)
      for(k in 1:K) {
        cv_k <- cv_idx[[k]]
        y_train <- y[-cv_k]
        x_train <- x[-cv_k, , drop = FALSE]
        y_test <- y[cv_k]
        x_test <- x[cv_k,,drop=FALSE]
        y_hat_test <- f(x_train, y_train, x_test)
        out[k] <- loss(y_hat_test, y_test)
      }
      save(out, file = path)
      print(paste("Finishing fold", k, "in rep", r))
    }
    results[,r] <- out
  }

  out <- list()
  out$results <- results
  out$seed <- seed
  return(out)

}

f_compare_all <- function(X, Y, seed, data_name, K = 5) {
  out <- list()
  R <- length(seed)
  cv_f <- function(f) f_compare(x = X, y = Y, seed = seed, K = K, method = f, data_name = data_name)

  out$softbart <- cv_f("softbart")
  out$xbart <- cv_f("xbart")
  out$lasso <- cv_f("lasso")
  out$rf <- cv_f("rf")
  out$xgb <- cv_f("xgb")
  out$dart <- cv_f("dart")
  out$sbartcv <- cv_f("sbartcv")


  return(out)
}


## Wipp: ----

results_wipp <- f_compare_all(X_wipp, Y_wipp, seed = seed, K = 5)

## BBB: DONE ----

results_bbb <- f_compare_all(X_bbb, Y_bbb, seed = seed, K = 5)

## Triazines: DONE ----
#
results_tri <- f_compare_all(X = X_tri, Y = Y_tri, seed = seed, K = 5)

## AIS: DONE ----

results_ais <- f_compare_all(X = X_ais, Y = Y_ais, seed = seed, K = 5)

## Hatco: DONE ----

results_hatco <- f_compare_all(X = X_hatco, Y = Y_hatco, seed = seed, K = 5)

## Servo ----

results_servo <- f_compare_all(X = X_servo, Y = Y_servo, seed = seed, K = 5)


## Cpu ----

results_cpu <- f_compare_all(X = X_cpu, Y = Y_cpu, seed = seed, K = 5)

## Abalone ----

results_abalone <- f_compare_all(X = as.matrix(X_aba), Y = Y_aba,
                                 seed = seed, K = 5)

## Diamonds: DONE ----

results_diamonds <- f_compare_all(X = X_diamonds,
                                  Y = Y_diamonds, seed = seed, K = 5)

## Tecator: DONE ----

results_tec <- f_compare_all(X = X_tec, Y = Y_tec, seed = seed, K = 5)
