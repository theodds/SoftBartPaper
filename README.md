# SoftBartPaper

This code reproduces some of the results from the paper 

-   Linero, A.R. and Yang, Y. (2017). *Bayesian tree ensembles that adapt to smoothness and sparsity.* (Submitted)

In particular, we give code for reproducing Figure 5 and Figure 6. We also give some code for reproducing Table 1, but do not provide the datasets. First, the user must install the `ProjectTemplate` and the `SoftBART` package. 

```r
install.packages("ProjectTemplate")
library(devtools)
install_github("theodds/SoftBART")
```

Note that if you are on OSX, you may need to first run the following commands from the terminal:

    curl -O http://r.research.att.com/libs/gfortran-4.8.2-darwin13.tar.bz2
    sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /

To load the needed helper functions and the packages required to run the code, navigate to this directory and run the following two lines of R code: 

```r
library('ProjectTemplate')
load.project()
```

