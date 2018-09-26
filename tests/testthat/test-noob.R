context("noob")

test_that("checks work", {
  expect_equal(fs::file_exists(test_paths$path),
               test_paths$file_exists, check.names = FALSE)
  expect_equal(path_within(test_paths$path),
               test_paths$path_within)
  expect_equal(file_exists_within(test_paths$path),
               test_paths$path_within & test_paths$file_exists, check.names = FALSE)

  read_csv_test <- test_paths %>%
    dplyr::pull(path) %>%
    purrr::map(purrr::possibly(read_csv, NA, quiet = FALSE)) %>%
    purrr::map_lgl(tibble::is_tibble)
  expect_equal(read_csv_test, test_paths$read_csv)
})


test_that("rendering works", {
  dir <- here::here("tests", "project_noob")
  rmd <- fs::dir_ls(dir, regexp = "\\.Rmd$")
  expect_equal(nrow(check(dir)), 2)
  rmarkdown::render(rmd, output_dir = tempdir())
  expect_length(fs::dir_ls(tempdir(), regexp = "\\.html$"), 1)
  path_find_file(rmd)
})

test_that("shims work", {

})
