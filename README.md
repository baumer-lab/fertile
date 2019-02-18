
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/beanumber/fertile.svg?branch=master)](https://travis-ci.org/beanumber/fertile)

fertile: creating optimal conditions for reproducibility
========================================================

The goal of `fertile` is to make creating a reproducible project as easy as possible, for users of all levels of sophistication.

Installation
------------

You can install `fertile` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("beanumber/fertile")
```

Example
-------

From within any R project directory, check your work for reproducibility:

``` r
fertile::proj_test("tests/testthat/project_noob/")
#> ── Checking for reproducibility ───────────────────────────────────── fertile 0.0.0.9024 ──
#> Warning: package 'bindrcpp' was built under R version 3.4.4
#> Warning: Please include a README file in /Users/audreybertin/Documents/
#> fertile/tests/testthat/project_noob
#> ── Rendering R scripts... ─────────────────────────────────────────── fertile 0.0.0.9024 ──
#> Reading from /Users/audreybertin/Documents/fertile/tests/testthat/project_noob/.fertile_render_log.csv
#> Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone 'default/America/
#> New_York'
#> ── Generating reproducibility report... ───────────────────────────── fertile 0.0.0.9024 ──
#> Reading from /Users/audreybertin/Documents/fertile/tests/testthat/project_noob/.fertile_log.csv
#> Checking for absolute paths...
#> Checking for paths outside project directory...
#> ── Analysis of reproducibility for project_noob ───────────────────── fertile 0.0.0.9024 ──
#> ──   Packages referenced in source code ───────────────────────────── fertile 0.0.0.9024 ──
#> # A tibble: 3 x 3
#>   package       N used_in                               
#>   <chr>     <int> <chr>                                 
#> 1 fertile       1 tests/testthat/project_noob/simple.Rmd
#> 2 rmarkdown     1 tests/testthat/project_noob/simple.Rmd
#> 3 tidyverse     1 tests/testthat/project_noob/simple.Rmd
#> ──   Files present in directory ───────────────────────────────────── fertile 0.0.0.9024 ──
#> # A tibble: 2 x 4
#>   file               ext          size mime                    
#>   <fs::path>         <chr> <fs::bytes> <chr>                   
#> 1 project_noob.Rproj Rproj         204 application/octet-stream
#> 2 simple.Rmd         Rmd           400 text/x-markdown
#> ──   Suggestions for moving files ─────────────────────────────────── fertile 0.0.0.9024 ──
#> # A tibble: 1 x 3
#>   path_rel   dir_rel    cmd                                                
#>   <fs::path> <fs::path> <chr>                                              
#> 1 simple.Rmd vignettes  file_move('tests/testthat/project_noob/simple.Rmd'…
#> ──   Problematic paths logged ─────────────────────────────────────── fertile 0.0.0.9024 ──
#> # A tibble: 4 x 4
#>   path       func      problem                 solution                    
#>   <chr>      <chr>     <chr>                   <chr>                       
#> 1 ../data/d… readr::r… Path is not contained … Move the file and/or use a …
#> 2 ../data/d… readr::r… Path is not contained … Move the file and/or use a …
#> 3 ../data/d… readr::r… Path is not contained … Move the file and/or use a …
#> 4 ../data/d… readr::r… Path is not contained … Move the file and/or use a …
```

`fertile` has two modes:

-   diagnostic or retrospective
-   interactive or prospective

Unlike [drake](https://github.com/ropensci/drake) or [easyMake](https://github.com/GShotwell/easyMake), we actually evaluate all of your code.

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
-   See also: [easyMake](https://github.com/GShotwell/easyMake), [drake](https://github.com/ropensci/drake)

### Stage 3 (?)

-   Self-bundling
-   Certification
