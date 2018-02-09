#' Does K-fold cross validation
#'
#' Given a function f(x,y,x_test), which outputs predictions for x_test based
#' on training data (x,y), return the results of K-fold cross validation
#'
#' @param x Predictor matrix or data frame
#' @param y Response
#' @param K Number of folds
#' @param R Number of CV replicates
#' @param seed Seed for choosing the folds; if not provided, this is chosen randomly
#' @param loss Loss function; we return loss(y_test, y_hat_test)
#'
#' @return Returns a list with two components: results, which gives the results
#' of the CV, and seed, which says what seed was used
CV <- function(f, x, y, K = 5, R = 1, seed = NULL, loss = rmspe, ...) {

  results <- matrix(NA, K, R)
  if(is.null(seed)) seed <- sample(1:999999, R)
  stopifnot(length(seed) == R)

  for(r in 1:R) {
    set.seed(seed[r])
    cv_idx <- createFolds(y, k = K)
    for(k in 1:K) {
      cv_k <- cv_idx[[k]]
      y_train <- y[-cv_k]
      x_train <- x[-cv_k, , drop = FALSE]
      y_test <- y[cv_k]
      x_test <- x[cv_k,,drop=FALSE]
      y_hat_test <- f(x_train, y_train, x_test, ...)
      results[k,r] <- loss(y_hat_test, y_test)
      print(paste("Finishing fold", k, "in rep", r))
    }
  }

  out <- list()
  out$results <- results
  out$seed <- seed

  return(out)

}

TrainTest <- function(f, x_train, x_test, y_train, y_test, loss = rmspe, ...) {

  y_hat_test <- f(x_train, y_train, x_test, ...)
  return(loss(y_hat_test, y_test))

}

rmspe <- function(x,y) sqrt(mean((x - y)^2))
zero1 <- function(x,y) sum(x != y)
