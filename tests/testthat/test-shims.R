context("shims")

test_that("shims works", {
  expect_is(path_shims(), "fs_path")
  expect_true(file.exists(path_shims()))

  in_file <- read_shims()
  expect_is(in_file, "character")
  expect_true(all(stringr::str_detect(in_file, "::")))

  # loading and unloading
  load_shims()

  file_shims <- read_shims() %>%
    stringr::str_extract("::.+$") %>%
    stringr::str_remove_all("::")
  expect_true(file_shims %in% active_shims())

  unload_shims()
  expect_false(file_shims %in% active_shims())
})

