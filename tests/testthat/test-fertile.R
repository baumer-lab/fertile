context("fertile")

test_that("checks work", {
  # is_path_safe
  expect_true(is_path_safe("data.csv"))
  expect_true(is_path_safe("data/data.csv"))
  expect_true(is_path_safe("./data/data.csv"))
  expect_true(is_path_safe("data/data.rda"))
  expect_false(is_path_safe("/home/bbaumer/data.csv"))
  expect_false(is_path_safe("/Users/bbaumer/data.csv"))
  expect_false(is_path_safe("~/data.csv"))
  expect_false(is_path_safe("/tmp/data.csv"))
  expect_false(is_path_safe("../data.csv"))
  expect_false(is_path_safe("../../data.csv"))
  expect_false(is_path_safe("../../../data.csv"))
  expect_true(fs::path_has_parent(fs::path_norm(test_path("../testthat/project_noob/data.csv")),
                                  fs::path_abs(test_path("project_noob"))))
  expect_false(is_path_safe("../project_noob/data.csv"))
  expect_false(is_path_safe("~/Dropbox/git/fertile/tests/data/data.csv"))

  expect_equal(nrow(read_csv(test_path("data", "data.csv"))), 1)

  expect_equal(nrow(check_path(test_path("data", "data.csv"))), 0)
  expect_error(check_path(test_path("data.csv")), "don't exist")
  expect_error(check_path(fs::path_abs(test_path("data.csv"))), "absolute")
  expect_error(check_path("../../../../../../../../../../data.csv"), "outside the project")
})


test_that("logging works", {
  expect_s3_class(proj_root(), "fs_path")
  expect_true(dir.exists(proj_root()))
  expect_equal(fs::path_file(proj_root(test_path("project_noob"))), "project_noob")

  log <- log_touch()
  expect_true(file.exists(log))
  log_clear()
  expect_false(file.exists(log))
  expect_true(file.exists(log_touch()))

  # read_csv
  expect_error(read_csv("data.csv"), "don't exist")
  expect_equal(nrow(log_report()), 1)

  x <- fs::file_temp()
  expect_error(read_csv(x), "absolute")
  expect_equal(nrow(log_report()), 2)
  expect_equal(log_report() %>%
                 dplyr::filter(func == "readr::read_csv") %>%
                 nrow(), 2
  )

  # write_csv
  expect_error(write_csv(mtcars, tempfile()), "absolute")
  expect_equal(log_report() %>%
    dplyr::filter(func == "readr::write_csv") %>%
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

  # setwd
  expect_error(setwd(tempdir()), "setwd")
  # source
  expect_message(source(test_path("script.R")), "Checking")
})
