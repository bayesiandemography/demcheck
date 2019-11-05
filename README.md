---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# demcheck

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/bayesiandemography/demcheck.svg?branch=master)](https://travis-ci.com/bayesiandemography/demcheck)
[![Lifecycle status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**demcheck** contains checking functions used by the dem* packages. End-users would not normally use it directly.


## Installation

You can install from GitHub with:

``` r
devtools::install_github("demcheck")
```

## Usage

```r
library(demcheck)
chk_is_string("mystring")
chk_is_string(1)
```


