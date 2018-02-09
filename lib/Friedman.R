fried <- function(x){
  10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2+10*x[,4]+5*x[,5]
}


simulate_fried_data <- function(p, n_train = 250, n_test = 1000, sigma = 1) {

  X_train <- matrix(runif(n_train * p), nrow = n_train)
  mu_train <- fried(X_train)
  X_test <- matrix(runif(n_test * p), nrow = n_test)
  mu_test <- fried(X_test)
  Y_train <- mu_train + rnorm(n = n_train, mean = 0, sd = sigma)
  Y_test <- mu_test + rnorm(n = n_test, mean = 0, sd = sigma)

  return(list(X_train = X_train, Y_train = Y_train, mu_train = mu_train,
              X_test = X_test, Y_test = Y_test, mu_test = mu_test))

}

expit <- function(x) 1.0 / (1.0 + exp(-x))
