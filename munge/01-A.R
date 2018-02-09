# Example preprocessing script.

## Load datasets -------------------------------------------------------------

wipp <- read.csv("datasets/WIPP.csv")
bbb <- read.csv("datasets/r_bbb.csv")
triazines <- read.csv("datasets/triazines.data")
ais <- read.table("./datasets/ais.txt", header = TRUE)
hatco <- read.csv("./datasets/HATCO.csv")
servo <- read.csv("./datasets/servo.data", header = FALSE)
cpu <- read.csv("./datasets/r_cpu.csv")
abalone <- read.csv("./datasets/Abalone.csv")
diamonds <- read.csv("./datasets/diamonds.csv")
tecator <- read.csv("./datasets/Tecator.csv")

## Munge datasets ------------------------------------------------------------


X_wipp <- as.matrix(wipp %>% select(-wipp_y))
Y_wipp <- wipp$wipp_y

X_bbb <- as.matrix(bbb %>% dplyr::select(-logBBB))
Y_bbb <- bbb$logBBB


clean_data <- function(x) {
  P <- ncol(x)
  drops <- which(is.nan(x[1,]))
  return(x[,-drops])
}
X_tri <- as.matrix(triazines[,-61])
X_tri <- quantile_normalize_bart(X_tri)
Y_tri <- triazines[,61]
X_tri <- clean_data(X_tri)

Y_ais <- log(ais$Ferr)
X_ais <- preprocess_df(dplyr::select(ais, -Ferr, -Sport, -Sex))
X_ais <- X_ais$X

Y_hatco <- dplyr::select(hatco, X10)[,1]
X_hatco <- as.matrix(dplyr::select(hatco, -X10))

Y_servo <- log(servo$V5)
X_servo <- dplyr::select(servo, -V5)
X_servo <- as.matrix(preprocess_df(X_servo)$X)

X_cpu <- as.matrix(dplyr::select(cpu, -ERP, -PRP, -Vendor))
X_cpu <- matrix(as.numeric(X_cpu), nrow = nrow(X_cpu), ncol = ncol(X_cpu))
Y_cpu <- log(cpu$PRP)

Y_aba <- log(abalone$Rings)
X_aba <- dplyr::select(abalone, -Rings)
X_aba <- preprocess_df(X_aba)$X

Y_diamonds <- log(as.numeric(diamonds$Price))
X_diamonds <- dplyr::select(diamonds, -Price)
X_diamonds <- preprocess_df(X_diamonds)$X

X_tec <- as.matrix(dplyr::select(tecator, Spect1:PC22))
Y_tec <- sqrt(tecator$fat)
