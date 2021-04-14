
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/baumer-lab/fertile.svg?branch=master)](https://travis-ci.org/baumer-lab/fertile)
[![Codecov test
coverage](https://codecov.io/gh/baumer-lab/fertile/branch/master/graph/badge.svg)](https://codecov.io/gh/baumer-lab/fertile?branch=master)

# fertile: creating optimal conditions for reproducibility <img src="man/figures/logo.png" align="right" height="139"/>

The goal of `fertile` is to make creating a reproducible project as easy
as possible, for users of all levels of sophistication.

`fertile` provides a wide variety of checks that can be run on your
project to test different aspects of its reproducibility–including clean
project structure, portability of paths, and use of randomnes–as well as
several functions that will create reproducibility reports for you with
information about referenced packages and file paths.

## Installation

You can install `fertile` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("baumer-lab/fertile")
```

## Reproducibility Reports

From within any R project directory, you can create a report of
reproducibility with `proj_test()` or its smaller component functions.

This report will contain information about packages referenced in
project code, files in your directory and suggestions for reorganizing
them, as well a list of absolute and/or non-portable paths passed to
functions in your code.

``` r
proj_test(noob)
#> ── Checking for reproducibility ──────────────────────────── fertile 1.1.9003 ──
#> ── Rendering R scripts... ────────────────────────────────── fertile 1.1.9003 ──
#> Error: '../data/data.csv' does not exist in current working directory ('/private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_noob').
```

## Reproducibility Checks

There are several functions allowing you to run checks on project
reproducibility.

The function `proj_check()` runs 16 different tests to check your
project for reproduciblity, and provides a summary of checks that you
passed as well as ones to work on and how to improve them. Each of these
checks can also be run individually.

`proj_check_some()` will complete a selection of the checks run by
`proj_check()`, specified by the user through a `dplyr::select`-style
statement.

``` r
proj_check_some(miceps, contains("tidy"), ends_with("root"), has_only_used_files)
#> # A tibble: 2 x 2
#>   culprit                                  expr                                 
#>   <fs::path>                               <glue>                               
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> 2 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> # A tibble: 3 x 2
#>   culprit                                  expr                                 
#>   <fs::path>                               <glue>                               
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> 2 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> 3 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> # A tibble: 6 x 1
#>   path_abs                                                                      
#>   <chr>                                                                         
#> 1 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 2 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 3 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 4 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 5 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 6 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 
#> 
#> # A tibble: 2 x 2
#>   culprit                                  expr                                 
#>   <fs::path>                               <glue>                               
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> 2 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> # A tibble: 3 x 2
#>   culprit                                  expr                                 
#>   <fs::path>                               <glue>                               
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> 2 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> 3 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb3… fs::file_move('/var/folders/v6/f62qz…
#> # A tibble: 6 x 1
#>   path_abs                                                                      
#>   <chr>                                                                         
#> 1 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 2 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 3 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 4 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 5 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
#> 6 /private/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgECpcJ/project_m…
```

## Reproducibility Badges

Users looking for a more visual summary of their reproducibility success
can do so using `proj_badges()`. This function, when run on an R Project
directory, builds an html summary document containing information about
which reproducibility components were met, which ones failed, and areas
on which to focus for improvement.

Additionally, the document contains information about the R version,
computer operating system, user, and list of files that were used in the
generation of the reproducibility summary.

The results of `proj_badges()` look like the example below:

``` r
proj_badges(miceps)
```

**List of components that were met/failed + areas to focus on for
improvement:**

<img src="man/figures/badges-1.png" align="center" height="700"/>

**Technical information about how the report was generated:**

<img src="man/figures/badges-2.png" align="center" height="450"/>

Users can also run the reproducibility checks associated with each
badge—interactively, instead of producing an html report—using the
`proj_check_badge()` function, which takes an argument for the badge
name. See the example below, where all the functions for the
documentation badge are run on a sample project:

``` r
# Options include: "documentation", "tidy-files", "structure",
# "style", "randomness", and "paths"
proj_check_badge(miceps, "documentation")
#> ✓ Checking for clear build chain
#> ✓ Checking for README file(s) at root level
#> ● Checking that code is adequately commented
#>    Problem: Suboptimally commented .R or .Rmd files found
#>    Solution: Add more comments to the files below. At least 10% of the lines should be comments.
#>    See for help: https://intelligea.wordpress.com/2013/06/30/inline-and-block-comments-in-r/
#> # A tibble: 1 x 2
#>   file_name                                                fraction_lines_comme…
#>   <chr>                                                                    <dbl>
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgE…                  0.04
#> ── Summary of fertile checks ─────────────────────────────── fertile 1.1.9003 ──
#> ✓ Reproducibility checks passed: 2
#> ● Reproducibility checks to work on: 1
#> ● Checking that code is adequately commented
#>    Problem: Suboptimally commented .R or .Rmd files found
#>    Solution: Add more comments to the files below. At least 10% of the lines should be comments.
#>    See for help: https://intelligea.wordpress.com/2013/06/30/inline-and-block-comments-in-r/
#> # A tibble: 1 x 2
#>   file_name                                                fraction_lines_comme…
#>   <chr>                                                                    <dbl>
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpgE…                  0.04
```

## Fertile’s Operating Modes

`fertile` has two modes:

-   diagnostic or retrospective
-   interactive or prospective

Reproducibility reports and checks make up the diagnostic part of
`fertile`. The interactive component comes from a system of path
logging:

## Logging and Reporting of Paths

When you have `fertile` loaded, the package will interactively edit a
log file located in your project directory, which will record the
paths/arguments passed to commonly-used input and output functions that
you execute in files or in the console. You can access this file using
`log_report()`, which reads the log, and `log_clear()`, which erases the
log and starts it over.

``` r
log_clear()
library(mime)
library(fertile)
set.seed(10)
read.csv("tests/testthat/data/data.csv")
#>   var1 var2
#> 1    a    2
log_report()
#> # A tibble: 4 x 4
#>   path            path_abs                         func      timestamp          
#>   <chr>           <chr>                            <chr>     <dttm>             
#> 1 package:mime    <NA>                             base::li… 2021-04-14 15:19:18
#> 2 package:fertile <NA>                             base::li… 2021-04-14 15:19:18
#> 3 seed:10         <NA>                             base::se… 2021-04-14 15:19:18
#> 4 tests/testthat… /Users/audreybertin/Documents/f… utils::r… 2021-04-14 15:19:18
```

``` r
log_clear()
log_report()
#> # A tibble: 0 x 0
```

The retrospective functions `proj_check()`, `proj_check_some()`,
`proj_test()`, and the functions related to them harness this same
logging system to produce reports. However, all editing of the
interactive log is independent from the retrospective log, which is not
user-accessible.

### Adding New Functions To Check Paths For

`fertile`, by default, contains a set list of common functions (such as
`read.csv()` from the `base` package and `read_csv()` from `readr`) for
which it can interactively/retroactively catch file path issues.

These are:

-   `utils`: read.csv, read.csv2, read.delim, read.delim2, read.DIF,
    read.fortran, read.fwf, read.table, write.csv

-   `readr`: read\_csv, read\_csv2, read\_delim, read\_file,
    read\_file\_raw, read\_fwf, read\_lines, read\_lines\_raw,
    read\_log, read\_table, read\_table2, read\_tsv, write\_csv

-   `base`: read.dcf, load, source, save

-   `readxl`: read\_excel

-   `stats`: read.ftable

-   `rjson`: fromJSON

-   `foreign`: read.dta, read.mtp, read.spss, read.systat

-   `sas7bdat`: read.sas7bdat

-   `ggplot2`: ggsave

Users may have a desire to add additional functions to the list that can
be checked for file path errors. `fertile` provides several ways in
which to edit this list:

The `add_shim()` function allows users to add a single function to the
list:

``` r
# Add stats::write.ftable to the list of functions that fertile checks for path issues

add_shim(func = "write.ftable", package = "stats")
```

To get a summary of all potential functions that could be added to the
list, users can use `find_all_shimmable_functions()`, which looks
through all of the currently loaded packages in `search()` to find
functions that use file paths.

If desired, all of the functions found by
`find_all_shimmable_functions()` can be added to `fertile`’s path
checking system simultaneously with the function
`add_all_possible_shims()`.

To view and/or edit the list of functions that they have added to file
path consideration, users can call `edit_shims()`. This function
displays the code for all of the functions that have specifically been
added to `fertile`’s path checking system by the user (the ones built
into `fertile` are not editable).

#### Enabling/Disabling File Path Checking

The built-in path checking functions in `fertile` are *always* enabled
when the package is loaded (via `library()`) and disabled when it is
unloaded. There is currently no method which with to disable them while
`fertile` is loaded.

Additional functions added to the file path-checking list by the user
are also automatically enabled/disabled on loading/unloading of
`fertile`, but they can also be enabled/disabled at any time as desired
using `load_shims()` and `unload_shims()`.

## Implementation

### Stage 1

-   Implement `proj_check()` for diagnosing and cleaning up an existing
    codebase

    -   modeled on `devtools::check()` and `goodpractice::gp()`

    -   Diagnostic part

        -   scans code files and finds potentially bad paths
        -   checks URLs to ensure they are valid/active
        -   checks DBI connections
        -   scans project directory (recursively) and suggests
            re-organization of files
        -   grep on file extensions and probably scan through text files

    -   Fix part

        -   fixes paths to be relative to project root (`here::here()`)
        -   move `data` to `data/`
        -   move code to `R/`, or `Rmd/` (or `src/` ?)
        -   move other crap to `inst/`

### Stage 2

-   Implement some kind of painless `make`-like functionality
-   See also: [easyMake](https://github.com/GShotwell/easyMake),
    [drake](https://github.com/ropensci/drake)

### Stage 3

-   Self-bundling
-   Certification

## Citation

``` r
citation("fertile")
#> 
#> To cite fertile in publications use:
#> 
#>   Bertin AM, Baumer BS. Creating optimal conditions for reproducible
#>   data analysis in R with 'fertile'. Stat. 2021;10:e332.
#>   https://doi.org/10.1002/sta4.332
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Creating optimal conditions for reproducible data analysis in R with 'fertile'},
#>     author = {{Bertin} and Audrey M. and {Baumer} and Benjamin S.},
#>     journal = {Stat},
#>     volume = {10},
#>     number = {1},
#>     pages = {e332},
#>     keywords = {uality control, statistical computing, statistical process control, teaching statistics},
#>     year = {2021},
#>     doi = {https://doi.org/10.1002/sta4.332},
#>     url = {https://onlinelibrary.wiley.com/doi/abs/10.1002/sta4.332},
#>     eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1002/sta4.332},
#>     note = {e332 sta4.332},
#>     abstract = {The advancement of scientific knowledge increasingly depends on ensuring that data-driven research is reproducible: that two people with the same data obtain the same results. However, while the necessity of reproducibility is clear, there are significant behavioral and technical challenges that impede its widespread implementation and no clear consensus on standards of what constitutes reproducibility in published research. We present fertile, an R package that focuses on a series of common mistakes programmers make while conducting data science projects in R, primarily through the RStudio integrated development environment. fertile operates in two modes: proactively, to prevent reproducibility mistakes from happening in the first place, and retroactively, analyzing code that is already written for potential problems. Furthermore, fertile is designed to educate users on why their mistakes are problematic and how to fix them.},
#>   }
```

The `fertile` release at the time of publication for the above citation
can be found here:
<https://github.com/baumer-lab/fertile/releases/tag/v1.0>
