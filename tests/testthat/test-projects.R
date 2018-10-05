context("projects")

test_that("project checking works", {
  # noob
  dir <- test_path("project_noob")
  expect_message(x <- proj_test(dir), "reproducibility")
  expect_equal(nrow(x), 1)
  expect_length(fs::dir_ls(tempdir(), regexp = "\\.html$"), 1)
  expect_equal(nrow(proj_analyze(dir)), 1)

  fs::dir_copy(dir, tempdir())
  test_dir <- fs::path(tempdir(), "project_noob")
  expect_equal(nrow(proj_analyze(test_dir, execute = TRUE)), 1)
  expect_length(fs::path_file(fs::dir_ls(test_dir, type = "dir")), 1)

  # miceps
  dir <- test_path("project_miceps")
  expect_equal(nrow(proj_analyze(dir)), 7)

  fs::dir_copy(dir, tempdir())
  test_dir <- fs::path(tempdir(), "project_miceps")
  expect_equal(nrow(proj_analyze(test_dir, execute = TRUE)), 7)
  expect_length(fs::path_file(fs::dir_ls(test_dir, type = "dir")), 3)
})
