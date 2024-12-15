Supplementary information / reproducible research files for the manuscript entitled "Identification of distributions for risks based on the first moment and c-statistic"

Authors: Sadatsafavi, M., Lee, T.Y., Petkau, J.
Code was written by Lee, T.Y.
For questions or comments, please contact mohsen.sadatsafavi@ubc.ca.

The code was written/evaluated in R with the following software versions:
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS 15.1.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Los_Angeles
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] RSSthemes_1.0.0 pROC_1.18.5     mcmapper_0.0.11 lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1
 [7] dplyr_1.1.4     purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1
[13] tidyverse_2.0.0

loaded via a namespace (and not attached):
 [1] gtable_0.3.6      compiler_4.4.1    tidyselect_1.2.1  Rcpp_1.0.13       showtext_0.9-7    scales_1.3.0
 [7] R6_2.5.1          plyr_1.8.9        generics_0.1.3    showtextdb_3.0    munsell_0.5.1     pillar_1.9.0
[13] tzdb_0.4.0        rlang_1.1.4       utf8_1.2.4        stringi_1.8.4     timechange_0.3.0  cli_3.6.3
[19] withr_3.0.2       magrittr_2.0.3    grid_4.4.1        rstudioapi_0.17.1 hms_1.1.3         lifecycle_1.0.4
[25] sysfonts_0.8.9    vctrs_0.6.5       glue_1.8.0        fansi_1.0.6       colorspace_2.1-1  tools_4.4.1
[31] pkgconfig_2.0.3

This folder contains the following data and files that can be used to reproduce the simulation results and figure of the manuscript.

It contains the following:

./figures/:
  fig_sim.jpeg
  Figure 1 of the manuscript.

./results/:
  algos_sol.rds
  A list of two-parameters of the beta, logit-normal, and probit-normal distributions solved by the mcmapper function
  for a grid of prevalence m and c-statistic c values.

  simulation_results.rds
  Processed simulation results.

simulation.R
  An R script that generates the simulation results, stores them, and processes them to create the figure presented in the manuscript.

