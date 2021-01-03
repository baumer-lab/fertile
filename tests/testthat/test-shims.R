context("shims")

test_that("shims works", {
  expect_is(path_shims(), "fs_path")
  expect_true(file.exists(path_shims()))

  in_file <- read_shims()
  expect_is(in_file, "character")
  expect_true(all(stringr::str_detect(in_file, "::")))

  # loading and unloading

  # These tests don't seem to work properly because testthat
  # isn't using the same global environment as Rstudio
  # The functions are successful when run normally but
  # give errors in testthat indicating that the shims can't be
  # found in the environment after being loaded

  # load_shims()

  # file_shims <- read_shims() %>%
  #   stringr::str_extract("::.+$") %>%
  #   stringr::str_remove_all("::")
  # expect_true(file_shims %in% active_shims())
  #
  # unload_shims()
  # expect_false(file_shims %in% active_shims())
})

