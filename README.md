
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcdt

<!-- badges: start -->
<!-- badges: end -->

The goal of dcdt is to enable the dC/dt method for the analysis of Ultra
Analytical Centrifugation (AUC) data.

## Installation

You can install the development version of dcdt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("HaigBishop/dcdt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dcdt)

# Read a X data file
auc.data <- dcdt::readAUC("path/to/your/datafile.X")
```
