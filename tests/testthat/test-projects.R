context("projects")

test_that("noob checking works", {

  # noob
  dir <- test_path("project_noob")
  # test_dir <- sandbox(dir)

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
})

test_that("miceps checking works", {

  # miceps
  dir <- test_path("project_miceps")
  x <- proj_test(dir)

  expect_equal(nrow(x$packages), 9)
  expect_equal(nrow(dplyr::filter(x$files, ext != "Rproj")), 8)
  expect_equal(nrow(x$suggestions), 7)
  expect_equal(nrow(x$paths), 0)

  proj_move_files(x$suggestions, execute = FALSE)
  expect_length(fs::dir_ls(dir, type = "dir"), 0)
  proj_move_files(x$suggestions, execute = TRUE)
  expect_length(path_file(fs::dir_ls(dir, type = "dir")), 3)
})
