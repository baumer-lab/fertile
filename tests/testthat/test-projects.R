context("projects")

test_that("noob checking works", {

  # noob
  dir <- test_project("project_noob.zip")

  session_file <- fs::path(dir, ".software-versions.txt")
  if (fs::file_exists(session_file)) {
    fs::file_delete(session_file)
  }
  expect_false(fs::file_exists(session_file))


  proj_render(dir)
  expect_equal(length(render_log_report(dir)$path), 9)
  expect_equal(tail(render_log_report(dir)$path, 1), "LAST RENDERED")


  expect_true(fs::file_exists(session_file))

  x <- proj_check(dir)
  expect_gt(nrow(x), 1)

  y <- proj_check_some(dir, ends_with("root"))
  expect_equal(nrow(y), 2)

  expect_true(has_proj_root(dir)$state)
  expect_false(has_readme(dir)$state)
  expect_true(has_tidy_media(dir)$state)
  expect_true(has_tidy_images(dir)$state)
  expect_true(has_tidy_code(dir)$state)
  expect_true(has_tidy_raw_data(dir)$state)
  expect_true(has_tidy_data(dir)$state)
  expect_true(has_tidy_scripts(dir)$state)

  # data_dir <- sandbox(test_path("data"))

  # expect_true(has_no_randomness(test_dir)$state)
  expect_true(has_no_absolute_paths(dir)$state)
  expect_false(has_only_portable_paths(dir)$state)



  expect_warning(proj_analyze_files(dir), "README")
  expect_warning(x <- proj_test(dir), "README")

  expect_length(fs::dir_ls(tempdir(), regexp = "simple.html$"), 1)
  expect_equal(nrow(x$packages), 3)
  # .Rbuildignore says to ignore .Rproj files!
  expect_equal(nrow(dplyr::filter(x$files, ext != "Rproj")), 1)
  expect_equal(nrow(x$suggestions), 1)
  expect_equal(nrow(x$paths), 2)

  proj_move_files(x$suggestions, execute = FALSE)
  expect_length(fs::dir_ls(dir, type = "dir"), 0)
  proj_move_files(x$suggestions, execute = TRUE)
  expect_length(fs::path_file(fs::dir_ls(dir, type = "dir")), 1)

  fs::dir_delete(dir)
})

test_that("miceps checking works", {

  # miceps
  dir <- test_project("project_miceps.zip")

  x <- proj_test(dir)

  class_list <- c("fertile_check", "fertile_check", "tbl_df", "tbl", "data.frame")
  five_trues <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

  y1 <- proj_check_badge(dir, "tidy-files")
  expect_equal(length(y1), 3)
  # Would not be all true if these columns didn't exist. Instead would be null.
  expect_equal(class(y1$`Checking for no image files at root level`) == class_list, five_trues)
  expect_equal(class(y1$`Checking for no raw data files at root level`) == class_list, five_trues)
  expect_equal(class(y1$`Checking to see if all files in directory are used in code`) == class_list, five_trues)

  y2 <- proj_check_badge(dir, "structure")
  expect_equal(length(y2), 0)
  expect_true(class(y2) == "NULL")

  y3 <- proj_check_badge(dir, "documentation")
  expect_equal(length(y3), 1)
  expect_equal(class(y3$`Checking that code is adequately commented`) == class_list, five_trues)


  y4 <- proj_check_badge(dir, "randomness")
  expect_equal(length(y4), 1)
  expect_equal(class(y4$`Checking for no randomness`) == class_list, five_trues)


  y5 <- proj_check_badge(dir, "style")
  expect_equal(length(y5), 1)
  expect_equal(class(y5$`Checking code style for lint`) == class_list, five_trues)



  expect_equal(nrow(x$packages), 9)
  expect_equal(nrow(dplyr::filter(x$files, ext != "Rproj")), 8)
  expect_equal(nrow(x$suggestions), 7)
  expect_equal(nrow(x$paths), 0)

  proj_move_files(x$suggestions, execute = FALSE)
  expect_length(fs::dir_ls(dir, type = "dir"), 0)
  proj_move_files(x$suggestions, execute = TRUE)
  expect_length(path_file(fs::dir_ls(dir, type = "dir")), 3)

  fs::dir_delete(dir)
})
