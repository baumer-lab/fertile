
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/beanumber/fertile.svg?branch=master)](https://travis-ci.org/beanumber/fertile)

fertile: creating optimal conditions for reproducibility
========================================================

The goal of `fertile` is to make creating a reproducible project as easy as possible, for users of all levels of sophistication.

Installation
------------

You can install fertile from github with:

``` r
# install.packages("devtools")
devtools::install_github("beanumber/fertile")
```

Example
-------

From within any R project directory, check your work for reproducibility:

``` r
fertile::check()
#> Found the following files:
#> Checking for reproducibility
#> # A tibble: 13 x 2
#> # Groups:   ext [13]
#>    ext       n
#>    <chr> <int>
#>  1 Rmd     407
#>  2 rmd      17
#>  3 html     12
#>  4 zip      10
#>  5 csv       9
#>  6 ""        7
#>  7 R         7
#>  8 Rd        7
#>  9 htm       3
#> 10 png       3
#> 11 Rproj     2
#> 12 md        1
#> 13 rda       1
```

Implementation
--------------

### Stage 1 (fall 2018)

-   Implement `check()` for diagnosing and cleaning up an existing codebase
    -   modeled on `devtools::check()` and `goodpractice::gp()`
    -   Diagnostic part
        -   scans code files and finds potentially bad paths
        -   checks URLs to ensure they are valid/active
        -   checks DBI connections
        -   scans project directory (recursively) and suggests re-organization of files
        -   grep on file extensions and probably scan through text files
    -   Fix part
        -   fixes paths to be relative to project root (`here::here()`)
        -   move `data` to `data/`
        -   move code to `R/`, or `Rmd/` (or `src/` ?)
        -   move other crap to `inst/`

### Stage 2 (spring 2019)

-   Implement some kind of painless `make`-like functionality
-   See also: [easyMake](https://github.com/GShotwell/easyMake), [drake](https://github.com/RobotLocomotion/drake)

### Stage 3 (?)

-   Self-bundling
-   Certification
