context("noob")

test_that("checks work", {
  x <- file_exists_within(test_paths$path)
  expect_equal(sum(x), 1)

  dir <- here::here("tests", "project_noob")
  rmd <- fs::dir_ls(dir, regexp = "\\.Rmd$")
  expect_equal(nrow(check(dir)), 2)
  rmarkdown::render(rmd, output_dir = tempdir())
  expect_length(fs::dir_ls(tempdir(), regexp = "\\.html$"), 1)
  path_find_file(rmd)
})
