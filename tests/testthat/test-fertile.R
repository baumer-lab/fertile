context("fertile")

test_that("checks work", {
  # is_path_portable
  expect_true(is_path_portable("data.csv"))
  expect_true(is_path_portable("data/data.csv"))
  expect_true(is_path_portable("./data/data.csv"))
  expect_true(is_path_portable("data/data.rda"))
  expect_false(is_path_portable("/home/bbaumer/data.csv"))
  expect_false(is_path_portable("/Users/bbaumer/data.csv"))
  expect_false(is_path_portable("~/data.csv"))
  expect_false(is_path_portable("/tmp/data.csv"))
  expect_false(is_path_portable("../data.csv"))
  expect_false(is_path_portable("../../data.csv"))
  expect_false(is_path_portable("../../../data.csv"))
  expect_true(path_has_parent(
    path_norm(test_path("../testthat/project_noob/data.csv")),
    path_abs(test_path("project_noob"))
  ))
  expect_false(is_path_portable("../project_noob/data.csv"))
  expect_false(is_path_portable("~/Dropbox/git/fertile/tests/data/data.csv"))


  expect_equal(nrow(readr::read_csv(test_path("data", "data.csv"))), 1)

  expect_equal(nrow(check_path(test_path("data", "data.csv"))), 0)
  # expect_error(check_path(test_path("data.csv")), "does not exist")
  # ^ doesn't work bc test path returns path even if the file does not exist
  expect_error(check_path(path_abs(test_path("data.csv"))), "absolute")
  expect_error(check_path("../../../../../../../../../../data.csv"), "outside the project")

  file <- test_path("project_noob", "simple.Rmd")
  dir <- fs::path_dir(file)
  dir <- sandbox(dir)
  expect_error(check_is_dir(file))
  expect_equal(check_is_dir(dir), dir)
})


test_that("logging works", {
  Sys.setenv("LOGGING_ON" = TRUE)
  expect_s3_class(proj_root(test_path("project_noob")), "fs_path")
  expect_true(dir.exists(proj_root(test_path("project_noob"))))
  expect_equal(path_file(proj_root(test_path("project_noob"))), "project_noob")

  log <- log_touch(test_path("project_noob"))
  expect_true(file.exists(log))
  log_clear(test_path("project_noob"))
  expect_false(file.exists(log))
  expect_true(file.exists(log_touch(test_path("project_noob"))))
  log_clear(test_path("project_noob"))

  Sys.setenv("LOGGING_ON" = FALSE)
})


test_that("shims works", {
  Sys.setenv("LOGGING_ON" = TRUE)


  here::set_here(path = test_path())

  log_clear()

  expect_last_logged <- function(path, func) {
    last_log <- log_report() %>%
      tail(1) %>%
      dplyr::select(-timestamp)

    if (is_file(path) | is_dir(path)) {
      abs <- as.character(fs::path_abs(path))
    } else {
      abs <- NA

      expectation <- tibble(path = as.character(path), path_abs = as.character(abs), func = as.character(func))

      expect_identical(last_log, expectation)
    }
  }



  # read_csv

  csv_path <- test_path("data", "data.csv")
  expect_identical(read_csv(csv_path), readr::read_csv(csv_path))
  expect_last_logged(csv_path, "readr::read_csv")

  x <- file_temp()
  expect_error(readr::read_csv(x), "does not exist")
  expect_error(fertile::read_csv(x), "absolute paths")
  expect_last_logged(x, "readr::read_csv")

  # read.csv

  expect_error(utils::read.csv(x), "cannot open the connection")
  expect_error(read.csv(x), "absolute paths")
  expect_last_logged(x, "utils::read.csv")

  # read.csv2

  expect_error(utils::read.csv2(x), "cannot open the connection")
  expect_error(read.csv2(x), "absolute paths")
  expect_last_logged(x, "utils::read.csv2")

  # read.delim

  expect_error(utils::read.delim(x), "cannot open the connection")
  expect_error(read.delim(x), "absolute paths")
  expect_last_logged(x, "utils::read.delim")

  # read.delim2

  expect_error(utils::read.delim2(x), "cannot open the connection")
  expect_error(read.delim2(x), "absolute paths")
  expect_last_logged(x, "utils::read.delim2")

  # read.DIF

  expect_error(utils::read.DIF(x), "cannot open the connection")
  expect_error(read.DIF(x), "absolute paths")
  expect_last_logged(x, "utils::read.DIF")

  # read.fortran

  expect_error(utils::read.fortran(x, c("F2.1", "F2.0", "I2")), "cannot open the connection")
  expect_error(read.fortran(x, c("F2.1", "F2.0", "I2")), "absolute paths")
  expect_last_logged(x, "utils::read.fortran")

  # read.fwf

  expect_error(utils::read.fwf(x, widths = c(1, 2, 3)), "cannot open the connection")
  expect_error(read.fwf(x, widths = c(1, 2, 3)), "absolute paths")
  expect_last_logged(x, "utils::read.fwf")

  # read.table

  expect_error(utils::read.table(x), "cannot open the connection")
  expect_error(read.table(x), "absolute paths")
  expect_last_logged(x, "utils::read.table")

  # read_csv2

  expect_error(readr::read_csv2(x), "does not exist")
  expect_error(read_csv2(x), "absolute paths")
  expect_last_logged(x, "readr::read_csv2")

  # read_delim

  expect_error(readr::read_delim(x, delim = "|"), "does not exist")
  expect_error(read_delim(x), "absolute paths")
  expect_last_logged(x, "readr::read_delim")

  # read_file

  expect_error(readr::read_file(x), "does not exist")
  expect_error(read_file(x), "absolute paths")
  expect_last_logged(x, "readr::read_file")

  # read_file_raw

  expect_error(readr::read_file_raw(x), "does not exist")
  expect_error(read_file_raw(x), "absolute paths")
  expect_last_logged(x, "readr::read_file_raw")

  # read_fwf

  expect_error(readr::read_fwf(file = path_temp(), fwf_widths(c(20, 10, 12))), "Cannot read file")
  expect_error(read_fwf(x), "absolute paths")
  expect_last_logged(x, "readr::read_fwf")

  # read_lines

  expect_error(readr::read_lines(x), "does not exist")
  expect_error(read_lines(x), "absolute paths")
  expect_last_logged(x, "readr::read_lines")

  # read_lines_raw

  expect_error(readr::read_lines_raw(x), "does not exist")
  expect_error(read_lines_raw(x), "absolute paths")
  expect_last_logged(x, "readr::read_lines_raw")

  # read_log

  expect_error(readr::read_log(x), "does not exist")
  expect_error(read_log(x), "absolute paths")
  expect_last_logged(x, "readr::read_log")

  # read_table

  expect_error(readr::read_table(x), "does not exist")
  expect_error(read_table(x), "absolute paths")
  expect_last_logged(x, "readr::read_table")

  # read_table2

  expect_error(readr::read_table2(x), "does not exist")
  expect_error(read_table2(x), "absolute paths")
  expect_last_logged(x, "readr::read_table2")

  # read_tsv

  expect_error(readr::read_tsv(x), "does not exist")
  expect_error(read_tsv(x), "absolute paths")
  expect_last_logged(x, "readr::read_tsv")

  # read.dcf

  expect_error(base::read.dcf(x), "cannot open the connection")
  expect_error(read.dcf(x), "absolute paths")
  expect_last_logged(x, "base::read.dcf")

  # read_excel

  expect_error(readxl::read_excel(x), "does not exist")
  expect_error(read_excel(x), "absolute paths")
  expect_last_logged(x, "readxl::read_excel")

  # read.ftable

  expect_error(stats::read.ftable(x), "cannot open the connection")
  expect_error(read.ftable(x), "absolute paths")
  expect_last_logged(x, "stats::read.ftable")

  # fromJSON

  expect_error(fromJSON(file = x), "absolute paths")
  expect_last_logged(x, "rjson::fromJSON")

  # read.dta

  expect_error(read.dta(x), "absolute paths")
  expect_last_logged(x, "foreign::read.dta")

  # read.mtp

  expect_error(read.mtp(x), "absolute paths")
  expect_last_logged(x, "foreign::read.mtp")

  # read.spss

  expect_error(read.spss(x), "absolute paths")
  expect_last_logged(x, "foreign::read.spss")

  # read.systat

  expect_error(read.systat(x), "absolute paths")
  expect_last_logged(x, "foreign::read.systat")

  # read.sas7bdat

  expect_error(read.sas7bdat(x), "absolute paths")
  expect_last_logged(x, "sas7bdat::read.sas7bdat")

  # write_csv
  tmp <- tempfile()
  expect_error(write_csv(mtcars, tmp), "absolute")
  expect_last_logged(tmp, "readr::write_csv")

  # write.csv

  expect_error(write.csv(mtcars, tmp), "absolute")
  expect_last_logged(tmp, "utils::write.csv")


  # ggsave
  if (require(ggplot2)) {
    ggplot(mtcars, aes(x = disp, y = mpg)) +
      geom_point()
    png <- file_temp(ext = ".png")
    expect_error(ggsave(filename = png), "absolute")
    expect_last_logged(png, "ggplot2::ggsave")
  }

  # setwd
  expect_error(setwd(tempdir()), "setwd")

  # source
  expect_message(source(test_path("script.R")), "Checking")

  # load
  if ("data" %in% ls()) {
    rm(data)
  }
  load(test_path("data", "data.rda"))
  expect_true("data" %in% ls())

  # save
  if ("save.rda" %in% dir_ls(test_path())) {
    file_delete(test_path("save.rda"))
  }
  expect_message(save(data, file = test_path("save.rda")), "Checking")
  expect_true("save.rda" %in% path_file(dir_ls(test_path())))
  file_delete(test_path("save.rda"))

  # library
  if ("package:datasets" %in% search()) {
    detach("package:datasets", unload = TRUE)
  }
  expect_false("package:datasets" %in% search())
  library(datasets)
  expect_true("package:datasets" %in% search())
  expect_last_logged("package:datasets", "base::library")

  # require
  detach("package:datasets", unload = TRUE)
  expect_false("package:datasets" %in% search())
  expect_true(require(datasets))
  expect_true("package:datasets" %in% search())
  expect_last_logged("package:datasets", "base::require")

  Sys.setenv("LOGGING_ON" = FALSE)
})


# test_that("danger works", {
# wd <- getwd()
# expect_error(setwd(tempdir()), "setwd")
# expect_message(danger(setwd(path_real(tempdir()))), "fertile")
# expect_equal(path_real(getwd()), path_real(tempdir()))
# base::setwd(wd)
# })

test_that("package script works", {
  dir <- sandbox(test_path("project_noob"))

  expect_false(file_exists(fs::path(dir, "install_proj_packages.r")))
  proj_pkg_script(dir)
  expect_true(file_exists(fs::path(dir, "install_proj_packages.r")))
  fs::file_delete(path(dir, "install_proj_packages.r"))
})


test_that("render mode works", {
  dir <- "tests/testthat/project_noob"

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)
  expect_equal(path_file(path_log(dir)), ".fertile_render_log.csv")

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)
  expect_equal(path_file(path_log(dir)), ".fertile_log.csv")
})

test_that("utils work", {
  expect_error(check_is_file("project_noob/random.rmd"), "NOT to a file")
  expect_error(check_is_file(test_path("project_noob")), "NOT to a file")
  expect_equal(check_is_file("project_noob/simple.Rmd"), "project_noob/simple.Rmd")
  expect_equal(check_is_dir(test_path("project_noob")), "project_noob")
  expect_error(check_is_dir("project_noob/simple.Rmd"), "NOT to a directory")
  expect_true(is_image_file("project_miceps/citrate_v_time.png"))
  expect_false(is_image_file("project_miceps/mice.csv"))
  expect_true(is_data_file("project_miceps/mice.csv"))
  expect_true(is_data_file("data/alice.txt"))
  expect_false(is_data_file("project_miceps/proteins_v_time.png"))
  expect_true(is_r_file("project_noob/simple.Rmd"))
  expect_false(is_r_file("project_miceps/proteins_v_time.png"))
  expect_true(is_r_file("project_miceps/README.md"))
  expect_true(is_text_file("project_miceps/README.md"))
  #  expect_true(is_text_file("project_noob/simple.html"))
  expect_true(is_text_file("project_miceps/mice.csv"))
  expect_false(is_text_file("project_miceps/proteins_v_time.png"))

  checks <- c(
    "has_tidy_media",
    "has_tidy_images",
    "has_tidy_code",
    "has_tidy_raw_data",
    "has_tidy_data",
    "has_tidy_scripts",
    "has_readme",
    "has_no_lint",
    "has_proj_root",
    "has_no_nested_proj_root",
    "has_only_used_files",
    "has_clear_build_chain",
    "has_no_absolute_paths",
    "has_only_portable_paths",
    "has_no_randomness"
  )

  checks_list <- list_checks()
  expect_true(sum(checks_list == checks) == 15)
})
