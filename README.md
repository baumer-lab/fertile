
<!-- README.md is generated from README.Rmd. Please edit that file -->
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
ferile::check()
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