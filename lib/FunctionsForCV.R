get_f <- function(str) {
  if(str == "softbart") return(f_softbart)
  if(str == "xbart") return(f_xbart)
  if(str == "lasso") return(f_lasso)
  if(str == "rf") return(f_rf)
  if(str == "xgb") return(f_xgb)
  if(str == "dart") return(f_dart)
  if(str == "sbartcv") return(f_sbartcv)

  stop("Invalid string")
}



f_sbartcv <- function(x,y,x_test) {

  library(Rcpp)

  num_trees <- c(20,50,200)

  X <- quantile_normalize_bart(rbind(x,x_test))
  x_norm <- X[1:nrow(x),]

  losses <- TreeSelect(x_norm,y)

  num_tree <- num_trees[which.min(losses)]
  softbart(x,y,x_test,
           Hypers(x,y,num_tree = num_tree),
           Opts(num_burn = 2500, num_save = 2500,
                update_tau_mean = FALSE))$y_hat_test_mean
}

f_xbart <- function(x,y,x_test) {
  library(dbarts)
  k <- c(2,3,5)
  ntree <- c(20, 50, 200)

  xbart_fit <- xbart(x, y, k = k, n.tree = ntree, n.threads = 1)

  rmses <- apply(xbart_fit, c(2,3), function(x) sqrt(mean(x^2)))
  best <- which(rmses == min(rmses), arr.ind = TRUE)

  tree_opt <- ntree[best[1]]
  k_opt <- k[best[2]]

  fit_opt <- dbarts::bart(x.train = x, y.train = y, x.test = x_test, k = k_opt,
                          ntree = tree_opt, nskip = 2500, ndpost = 2500)
  return(fit_opt$yhat.test.mean)
}

f_dbart <- function(x,y,x_test) {
  library(dbarts)

  fit_dbart <- dbarts::bart(x.train = x, y.train = y, x.test = x_test,
                            nskip = 2500, ndpost = 2500, ntree = 50)
  return(fit_dbart$yhat.test.mean)
}


f_softbart <- function(x,y,x_test) {
  softbart(x,y,x_test,
           Hypers(x,y,num_tree = 50),
           Opts(num_burn = 2500, num_save = 2500, update_tau_mean = FALSE))$y_hat_test_mean
}

f_dart <- function(x,y,x_test) {
  softbart(x,y,x_test, Hypers(x,y,width = 1E-10, num_tree=50),
           opts = Opts(update_tau = FALSE,
                       num_burn = 2500, num_save = 2500))$y_hat_test_mean
}


f_rf <- function(x,y,x_test) {
  library(randomForest)
  library(caret)
  fit <- train(data.frame(x), y)
  predict(fit, data.frame(x_test))
}

f_lasso <- function(x,y,x_test) {
  library(glmnet)
  fit <- cv.glmnet(x, y)
  predict(fit, x_test)
}

f_xgb <- function(x,y,x_test) {
  library(xgboost)
  library(caret)
  fit <- train(data.frame(x),y,method="xgbTree")
  predict(fit, data.frame(x_test))
}


TreeSelect <- function(X, Y, num_tree_vec = c(20,50,200), K = 5, opts = Opts(), ...) {
  num_prop <- length(num_tree_vec)
  N <- nrow(X)

  folds <- createFolds(Y, k = K)
  loss <- numeric(num_prop)

  for(i in 1:num_prop) {
    for(k in 1:K) {
      test_idx <- folds[[k]]
      train_idx <- setdiff(1:N, test_idx)
      X_train <- X[train_idx,]
      X_test <- X[test_idx,]
      Y_train <- Y[train_idx]
      Y_test <- Y[test_idx]

      hypers <- Hypers(X = X_train, Y = Y_train, num_tree = num_tree_vec[i], ...)

      forest <- MakeForest(hypers, opts)
      mu_hat <- forest$do_gibbs(X_train, Y_train, X_test, opts$num_burn)
      mu_hat <- forest$do_gibbs(X_train, Y_train, X_test, opts$num_save)

      loss[i] <- loss[i] + rmspe(Y_test, mu_hat)
    }
  }

  return(loss)

}

