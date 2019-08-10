---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# demcheck

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/johnrbryant/demcheck.svg?branch=master)](https://travis-ci.org/johnrbryant/demcheck)
[![Lifecycle status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**demcheck** contains checking functions used by the dem* packages. End-users would not normally use it directly.

**demcheck** is still under construction, and not ready for serious use.

## Installation

You can install from GitHub with:

``` r
devtools::install_github("demcheck")
```

## Usage

```r
library(devcheck)
chk_is_string("mystring")
chk_is_string(1)
```


