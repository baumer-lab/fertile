
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/baumer-lab/fertile.svg?branch=master)](https://travis-ci.org/baumer-lab/fertile)
[![Codecov test
coverage](https://codecov.io/gh/baumer-lab/fertile/branch/master/graph/badge.svg)](https://codecov.io/gh/baumer-lab/fertile?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

# fertile: creating optimal conditions for reproducibility <img src="man/figures/logo.png" align="right" height="139"/>

**Tools to make achieving R project reproducibility easy!**

## Why `fertile`?

-   Addresses all different reproducibility aspects in one place
-   Simple functions w/ minimal arguments
-   Minimal prior knowledge required
-   `R`-specific features
-   Customizable to your needs
-   Has educational features

## Sample Project

`miceps`: variable containing path to directory containing following
project:

<img src="man/figures/sample-project.png" align="center" height="450"/>

## Easily Create Reproducibility Reports

``` r
proj_badges(miceps)
```

<img src="man/figures/badges-1.png" align="center" height="700"/>

 

<img src="man/figures/badges-2.png" align="center" height="450"/>

## Run Reproducibility Checks

`fertile` contains 16 checks on different aspects of reproducibility:

<img src="man/figures/components-summary.png" align="center" height="450"/>

 

Run them individually or in customizable groupings, w/ `proj_check()`,
`proj_check_some()`, or `proj_check_badge()`

``` r
# Individual check
has_well_commented_code(miceps)
#> ● Checking that code is adequately commented
#>    Problem: Suboptimally commented .R or .Rmd files found
#>    Solution: Add more comments to the files below. At least 10% of the lines should be comments.
#>    See for help: https://intelligea.wordpress.com/2013/06/30/inline-and-block-comments-in-r/
#> # A tibble: 1 x 2
#>   file_name                                                fraction_lines_comme…
#>   <chr>                                                                    <dbl>
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/Rtmp3v…                  0.04
```

``` r
# Combined checks
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
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/Rtmp3v…                  0.04
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
#> 1 /var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/Rtmp3v…                  0.04
```

## Warnings For Potentially Non-Reproducible Commands

``` r
read_csv("/Users/audreybertin/Documents/fertile/project_miceps/mice.csv")
#> Checking for absolute paths...
#> Error: Detected absolute paths. Absolute paths are not reproducible and will likely only work on your computer. If you would like to continue anyway, please execute the following command: readr::read_csv('/Users/audreybertin/Documents/fertile/project_miceps/mice.csv')
```

``` r
setwd(miceps)
#> Error: setwd() is likely to break reproducibility. Use here::here() instead.
```

Several data-reading functions built in to `fertile`’s warning system:

<img src="man/figures/shims-list.png" align="center" height="450"/>

 

Customize warning system by:

-   Adding functions (`add_shim()`, `add_all_possible_shims()`)

``` r
# Add stats::write.ftable to the warning system

add_shim(func = "write.ftable", package = "stats")
```

-   Editing functions (`edit_shims()`)
-   Disabling warnings for user-added functions (`unload_shims()`)
-   Enabling warnings for user-added functions (`load_shims()`) — this
    is the *default* setting.

## Installation

You can install `fertile` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("baumer-lab/fertile")
```

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
