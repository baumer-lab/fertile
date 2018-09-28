context("noob")

test_that("checks work", {
  dir <- here::here("fertile")
  expect_equal(dir, getwd())
  path <- fs::path_expand(test_paths$path)

  expect_equal(fs::file_exists(path),
               test_paths$file_exists, check.names = FALSE)
  expect_equal(path_within(path),
               test_paths$path_within)
  expect_equal(file_exists_within(path),
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

test_that("logging works", {
  log <- touch()
  expect_true(file.exists(log))
  clear()
  expect_false(file.exists(log))
  expect_true(file.exists(touch()))
  expect_error(read_csv("data.csv"))
  expect_equal(nrow(readr::read_csv(log)), 1)
  x <- fs::file_temp(tmp_dir = here::here())
  expect_error(read_csv(x))
  expect_equal(nrow(readr::read_csv(log)), 2)
  proj_root <- here::here()
  expect_error(read_csv(file.path(proj_root, "my_data.csv")))
  expect_equal(file.path(proj_root, "my_data.csv"),
               readr::read_csv(log) %>%
                 dplyr::slice(3) %>%
                 dplyr::pull(path)
  )
})
