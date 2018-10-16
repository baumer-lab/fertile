context("projects")

test_that("project checking works", {
  # noob
  dir <- test_path("project_noob")
  expect_message(x <- proj_test(dir), "reproducibility")
  expect_equal(nrow(x), 1)
  expect_length(fs::dir_ls(tempdir(), regexp = "\\.html$"), 1)
  expect_equal(nrow(proj_analyze_files(dir)), 1)
  expect_equal(nrow(proj_analyze_pkgs(dir)), 2)

  test_dir <- fs::path(tempdir(), "project_noob")
  if (fs::dir_exists(test_dir)) {
    fs::dir_delete(test_dir)
  }
  fs::dir_copy(dir, test_dir)
  expect_equal(nrow(proj_analyze_pkgs(test_dir)), 2)
  expect_equal(nrow(proj_analyze_files(test_dir, execute = TRUE)), 1)
  expect_length(fs::path_file(fs::dir_ls(test_dir, type = "dir")), 1)

  # miceps
  dir <- test_path("project_miceps")
  expect_equal(nrow(proj_analyze(dir)[["files"]]), 7)

  test_dir <- fs::path(tempdir(), "project_miceps")
  if (fs::dir_exists(test_dir)) {
    fs::dir_delete(test_dir)
  }
  fs::dir_copy(dir, test_dir)
  expect_equal(nrow(proj_analyze_pkgs(test_dir)), 5)
  expect_equal(nrow(proj_analyze_files(test_dir, execute = TRUE)), 7)
  expect_length(fs::path_file(fs::dir_ls(test_dir, type = "dir")), 3)
})
