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

### Helper functions

Helper functions are defined in the `lib/` directory, and are loaded automatically by running `load.project()`. Included are convenience functions for doing cross-validation and functions which fit the required models. Additionally, code specifically for reproducing Figure 6 are in `lib/VariableSelection.R`.

### Reproduce Figure 5

Figure 5 can be reproduced by running `source("src/SimFried.R")`. This runs all the simulations sequentially. To save time, one can do much of the simulation in parallel by running the code separately for the different numbers of predictors (by changing `PIDX`) or by running the simulation separately for each method (e.g., running the simulation for computing `results_softbart` and `results_xbart` in separate processes). To facilitate this, the results for each method/predictor pair is cached in the folder `CacheSimFried/` so that if you run `source("src/SimFried.R")` multiple times the results will not need to be recomputed. Once everything has been computed and cached, running `source("src/SimFried.R")` will then produce the plot immediately.

### Reproduce Figure 6 

Figure 6 can be reproduced by running `source("src/VariableSelection.R")`. This runs the simulation experiment in parallel. In this file, you can specify the NUM_CORES argument to identify the number of cores you wish to use; the experiment is embarrassingly parallel, so using 10 cores will speed the computation up by a factor of 10. Results are cached in `CacheVariableSelection/`. 

### Data analysis

The raw data used for the real data analysis are provided in the `datasets/` folder. These datasets are automatically preprocessed and loaded when running the `load.project()` command. The code associated to reproducing the results in Table 1 is given in `src/TestingRealData.R` As above, our code caches all results. For speed purposes, one can do the analysis for the individual datasets in separate `R` processes; for the `abalone` dataset, it may be advisable to split the different seeds across different processes as well, as this dataset is larger than the others. Results are cached in `CacheRealData/`. 
