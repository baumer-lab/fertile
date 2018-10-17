
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
#> ── Checking for reproducibility ──────────────────────────────────────────────── fertile 0.0.0.9010 ──
#> ── Rendering R scripts... ────────────────────────────────────────────────────── fertile 0.0.0.9010 ──
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
#> /usr/lib/rstudio/bin/pandoc/pandoc +RTS -K512m -RTS simple.utf8.md --to html4 --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output /tmp/RtmpXAKMOR/simple.html --smart --email-obfuscation none --self-contained --standalone --section-divs --template /home/bbaumer/R/x86_64-pc-linux-gnu-library/3.4/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable 'theme:bootstrap' --include-in-header /tmp/RtmpXAKMOR/rmarkdown-str58e44054858e.html --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
#> 
#> Output created: /tmp/RtmpXAKMOR/simple.html
#> ── Generating reproducibility report... ──────────────────────────────────────── fertile 0.0.0.9010 ──
#> Checking for absolute paths...
#> Checking for paths outside project directory...
#> Checking for paths to files that don't exist...
#> ── Analysis of reproducibility ───────────────────────────────────────────────── fertile 0.0.0.9010 ──
#> ── --Packages referenced in source code ──────────────────────────────────────── fertile 0.0.0.9010 ──
#> # A tibble: 2 x 3
#>   package       N used_in                               
#>   <chr>     <int> <chr>                                 
#> 1 fertile       1 tests/testthat/project_noob/simple.Rmd
#> 2 rmarkdown     1 tests/testthat/project_noob/simple.Rmd
#> ── --Files present in directory ──────────────────────────────────────────────── fertile 0.0.0.9010 ──
#> # A tibble: 2 x 4
#>   file               ext          size mime                    
#>   <fs::path>         <chr> <fs::bytes> <chr>                   
#> 1 project_noob.Rproj Rproj         204 application/octet-stream
#> 2 simple.Rmd         Rmd           340 text/x-markdown
#> ── --Suggestions for moving files ────────────────────────────────────────────── fertile 0.0.0.9010 ──
#> # A tibble: 1 x 3
#>   path_rel   dir_rel    cmd                                               
#>   <fs::path> <fs::path> <chr>                                             
#> 1 simple.Rmd vignettes  fs::file_move('tests/testthat/project_noob/simple…
#> ── --Problematic paths logged ────────────────────────────────────────────────── fertile 0.0.0.9010 ──
#> # A tibble: 2 x 4
#>   path       func       problem              solution                     
#>   <chr>      <chr>      <chr>                <chr>                        
#> 1 ../data/d… readr::re… Path is not within … Move the file and use a rela…
#> 2 ../data/d… readr::re… File does not exist  Correct the path to the file
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
