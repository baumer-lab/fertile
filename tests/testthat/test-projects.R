context("projects")

test_that("project checking works", {

  # noob
  dir <- test_path("project_noob")
  test_dir <- sandbox(dir)
  expect_warning(proj_analyze_files(test_dir), "README")
  expect_message(x <- proj_test(test_dir), "reproducibility")

  expect_length(fs::dir_ls(tempdir(), regexp = "simple.html$"), 1)
  expect_equal(nrow(x$packages), 2)
  # .Rbuildignore says to ignore .Rproj files!
  expect_equal(nrow(dplyr::filter(x$files, ext != "Rproj")), 1)
  expect_equal(nrow(x$suggestions), 1)
  expect_equal(nrow(x$paths), 1)

  proj_move_files(x$suggestions, execute = FALSE)
  expect_length(fs::dir_ls(test_dir, type = "dir"), 0)
  proj_move_files(x$suggestions, execute = TRUE)
  expect_length(fs::path_file(fs::dir_ls(test_dir, type = "dir")), 1)

  # miceps
  dir <- test_path("project_miceps")
  test_dir <- sandbox(dir)

  x <- proj_analyze(test_dir)

  expect_equal(nrow(x$packages), 5)
  expect_equal(nrow(dplyr::filter(x$files, ext != "Rproj")), 8)
  expect_equal(nrow(x$suggestions), 7)
  expect_equal(nrow(x$paths), NULL)

  proj_move_files(x$suggestions, execute = FALSE)
  expect_length(fs::dir_ls(test_dir, type = "dir"), 0)
  proj_move_files(x$suggestions, execute = TRUE)
  expect_length(fs::path_file(fs::dir_ls(test_dir, type = "dir")), 3)
})
