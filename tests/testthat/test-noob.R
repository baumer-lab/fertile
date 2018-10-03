context("noob")

test_that("checks work", {
  # is_path_here
  good <- c("data.csv", "data/data.csv", "./data/data.csv", "data/data.rda")
  expect_true(all(is_path_here(good)))

  expect_true(is_path_here("data.csv"))
  expect_true(is_path_here("data/data.csv"))
  expect_true(is_path_here("./data/data.csv"))
  expect_true(is_path_here("data/data.rda"))
  expect_false(is_path_here("/home/bbaumer/data.csv"))
  expect_false(is_path_here("/Users/bbaumer/data.csv"))
  expect_false(is_path_here("~/data.csv"))
  expect_false(is_path_here("/tmp/data.csv"))
  expect_true(is_path_here("../data.csv"))
#  expect_true(is_path_here("~/Dropbox/git/fertile/tests/data/data.csv"))

  # file_exists_here
  expect_false(file_exists_here("data.csv"))
  expect_true(file_exists_here("data/data.csv"))
  expect_true(file_exists_here("./data/data.csv"))
  expect_true(file_exists_here("data/data.rda"))
  expect_false(file_exists_here("/home/bbaumer/data.csv"))
  expect_false(file_exists_here("/Users/bbaumer/data.csv"))
  expect_false(file_exists_here("~/data.csv"))
  expect_false(file_exists_here("/tmp/data.csv"))
  expect_false(file_exists_here("../data.csv"))
#  expect_true(file_exists_here("~/Dropbox/git/fertile/tests/testthat/data/data.csv"))

  expect_equal(nrow(read_csv(test_path("data", "data.csv"))), 1)

  expect_equal(nrow(check_path(test_path("data", "data.csv"))), 0)
  expect_error(check_path(test_path("data.csv")), "don't exist")
  expect_error(check_path(fs::path_abs(test_path("data.csv"))), "absolute")
  expect_error(check_path("~/Dropbox/git/fertile/tests/testthat/data/data.csv"), "absolute")
  expect_error(check_path(path_rel_here(tempfile())), "outside the project")
})


test_that("project checking works", {
  dir <- test_path("project_noob")
  expect_message(x <- proj_test(dir), "reproducibility")
  expect_equal(nrow(x), 1)
  expect_length(fs::dir_ls(tempdir(), regexp = "\\.html$"), 1)
})

test_that("logging works", {
  log <- log_touch()
  expect_true(file.exists(log))
  log_clear()
  expect_false(file.exists(log))
  expect_true(file.exists(log_touch()))

  # read_csv
  expect_error(read_csv("data.csv"))
  expect_equal(nrow(log_report()), 1)

  x <- fs::file_temp(tmp_dir = here::here())
  expect_error(read_csv(x))
  expect_equal(nrow(log_report()), 2)

  proj_root <- here::here()
  expect_error(read_csv(file.path(proj_root, "my_data.csv")))
  expect_equal(file.path(proj_root, "my_data.csv"),
               readr::read_csv(log) %>%
                 dplyr::slice(3) %>%
                 dplyr::pull(path)
  )

  # write_csv
  expect_error(write_csv(mtcars, path = tempfile()))
  expect_equal(log_report() %>%
    dplyr::filter(func == "write_csv") %>%
    nrow(), 1
  )

  # ggsave
  if (require(ggplot2)) {
    ggplot(mtcars, aes(x = disp, y = mpg)) +
      geom_point()
    expect_error(fertile::ggsave(filename = fs::file_temp(ext = ".png")), "absolute")
    expect_equal(log_report() %>%
                   dplyr::filter(func == "ggsave") %>%
                   nrow(), 1
    )
  }
})
