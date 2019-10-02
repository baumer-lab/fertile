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
  expect_true(path_has_parent(path_norm(test_path("../testthat/project_noob/data.csv")),
                                  path_abs(test_path("project_noob"))))
  expect_false(is_path_portable("../project_noob/data.csv"))
  expect_false(is_path_portable("~/Dropbox/git/fertile/tests/data/data.csv"))


  expect_equal(nrow(readr::read_csv(test_path("data", "data.csv"))), 1)

  expect_equal(nrow(check_path(test_path("data", "data.csv"))), 0)
  #expect_error(check_path(test_path("data.csv")), "does not exist")
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
  Sys.setenv("LOGGING_ON" = FALSE)
})


 test_that("shims works", {
  Sys.setenv("LOGGING_ON" = TRUE)

   here::set_here(path = test_path())

   expect_last_logged <- function(path, func) {

     last_log <- log_report() %>%
       tail(1) %>%
       dplyr::select(-timestamp)

     if (is_file(path) | is_dir(path)){
       abs = fs::path_abs(path)
     } else{
       abs = NA

     expectation <- tibble(path = as.character(path), path_abs = as.character(abs), func = func)

     expect_identical(last_log, expectation)

   }}


  # read_csv

   csv_path <- test_path("data", "data.csv")
   expect_identical(read_csv(csv_path), readr::read_csv(csv_path))
   expect_last_logged(csv_path, "readr::read_csv")

   x <- file_temp()
   expect_error(readr::read_csv(x), "does not exist")
   expect_error(fertile::read_csv(x), "absolute paths")
   expect_last_logged(x, "readr::read_csv")

   # write_csv
   tmp <- tempfile()
   expect_error(write_csv(mtcars, tmp), "absolute")
   expect_last_logged(tmp, "readr::write_csv")

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


#test_that("danger works", {
  #wd <- getwd()
  #expect_error(setwd(tempdir()), "setwd")
  #expect_message(danger(setwd(path_real(tempdir()))), "fertile")
  #expect_equal(path_real(getwd()), path_real(tempdir()))
  #base::setwd(wd)
#})


test_that("render mode works", {

  dir <- "tests/testthat/project_noob"

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)
  expect_equal(path_file(path_log(dir)), ".fertile_render_log.csv")

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)
  expect_equal(path_file(path_log(dir)), ".fertile_log.csv")

})

test_that("utils work", {

  expect_error(check_is_file("project_noob/random.rmd"), "NOT to a file")
  #expect_true(check_is_dir(test_path()))
  expect_true(is_image_file("project_miceps/citrate_v_time.png"))
  expect_false(is_image_file("project_miceps/mice.csv"))
  expect_true(is_data_file("project_miceps/mice.csv"))
  expect_true(is_data_file("data/alice.txt"))
  expect_false(is_data_file("project_miceps/proteins_v_time.png"))
  expect_true(is_r_file("project_noob/simple.Rmd"))
  expect_false(is_r_file("project_noob/simple.html"))
  expect_true(is_r_file("project_miceps/README.md"))
  expect_true(is_text_file("project_miceps/README.md"))
  expect_true(is_text_file("project_noob/simple.html"))
  expect_true(is_text_file("project_miceps/mice.csv"))
  expect_false(is_text_file("project_miceps/proteins_v_time.png"))

})
