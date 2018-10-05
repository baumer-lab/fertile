context("projects")

test_that("project checking works", {
  # noob
  dir <- test_path("project_noob")
  expect_message(x <- proj_test(dir), "reproducibility")
  expect_equal(nrow(x), 1)
  expect_length(fs::dir_ls(tempdir(), regexp = "\\.html$"), 1)

  # miceps
  dir <- test_path("project_miceps")
  proj_analyze(dir)
})
