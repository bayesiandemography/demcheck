
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demcheck

<!-- badges: start -->

[![Lifecycle
status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/demcheck/workflows/R-CMD-check/badge.svg)](https://github.com/bayesiandemography/demcheck/actions)
<!-- badges: end -->

**demcheck** contains checking functions used by the dem\* packages.
End-users would not normally use it directly.

## Installation

To install from GitHub, use:

``` r
devtools::install_github("bayesiandemography/demcheck")
```

## Usage

``` r
library(demcheck)
chk_is_string("mystring")
chk_is_string(1)
```
