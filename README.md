
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
#> Checking for reproducibility
#> Analyzing project file structure...
#> fertile found the following files:
#> # A tibble: 2 x 2
#> # Groups:   ext [2]
#>   ext       n
#>   <chr> <int>
#> 1 Rmd       1
#> 2 Rproj     1
#> Rendering R scripts...
#> 
#> 
#> processing file: simple.Rmd
#> 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |......................                                           |  33%
#>   ordinary text without R code
#> 
#> 
  |                                                                       
  |...........................................                      |  67%
#> label: unnamed-chunk-2 (with options) 
#> List of 1
#>  $ error: logi TRUE
#> 
#> 
  |                                                                       
  |.................................................................| 100%
#>   ordinary text without R code
#> output file: simple.knit.md
#> /usr/lib/rstudio/bin/pandoc/pandoc +RTS -K512m -RTS simple.utf8.md --to html4 --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output /tmp/RtmpGqrllc/simple.html --smart --email-obfuscation none --self-contained --standalone --section-divs --template /home/bbaumer/R/x86_64-pc-linux-gnu-library/3.4/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable 'theme:bootstrap' --include-in-header /tmp/RtmpGqrllc/rmarkdown-str29ff6f8e3bbe.html --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
#> 
#> Output created: /tmp/RtmpGqrllc/simple.html
#> Generating reproducibility report...
#> Parsed with column specification:
#> cols(
#>   path = col_character(),
#>   func = col_character(),
#>   timestamp = col_datetime(format = "")
#> )
#> Checking for absolute paths...
#> Checking for paths with tildes...
#> Checking for paths that will only work on Windows...
#> Checking for paths that will only work on Mac OS X...
#> Checking for paths that will only work on *NIX...
#> Checking for paths outside project directory...
#> Checking for paths to files that don't exist...
#> # A tibble: 2 x 5
#>   path     func   timestamp           problem         solution            
#>   <chr>    <chr>  <dttm>              <chr>           <chr>               
#> 1 ../data… read_… 2018-10-02 20:38:56 Path is not wi… Move the file and u…
#> 2 ../data… read_… 2018-10-02 20:38:56 File does not … Correct the path to…
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
