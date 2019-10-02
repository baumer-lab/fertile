
library(testthat)
library(fertile)

Sys.setenv("IN_TESTTHAT" = TRUE)

test_check("fertile")

Sys.setenv("IN_TESTTHAT" = FALSE)
