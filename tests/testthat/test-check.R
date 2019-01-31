context("projects")

test_that("has functions work", {

  dir <- test_path("project_noob")
  test_dir <- sandbox(dir)

  # check_is_dir works
  expect_true(check_is_dir(dir)$state)
  file <- test_path("project_noob/simple.Rmd")
  expect_false(check_is_dir(file)$state)

  seed_old <- .Random.seed
  #   expect_gt(nrow(x <- check(dir)), 1)

  # project roots
  expect_true(has_proj_root(test_dir)$state)
  rproj <- dir_ls(test_dir, regexp = "\\.Rproj$")
  file_delete(rproj)
  expect_false(has_proj_root(test_dir)$state)

  test_dir <- sandbox(dir)
  expect_true(has_no_nested_proj_root(test_dir)$state)
  dir_create(path(test_dir, "R"))
  fake <- path(test_dir, "R", "fake.Rproj")
  file_copy(rproj, fake)
  expect_false(has_no_nested_proj_root(test_dir)$state)

  test_dir <- sandbox(dir)
  expect_false(has_readme(test_dir)$state)
  readme <- path(test_dir, "README.md")
  file_create(readme)
  expect_true(has_readme(test_dir)$state)

  expect_true(has_no_lint(test_dir)$state)

  expect_true(has_clear_build_chain(test_dir)$state)
  file_create(path(test_dir, "second.R"))
  expect_false(has_clear_build_chain(test_dir)$state)
  file_move(path(test_dir, "second.R"), path(test_dir, "02-second.R"))
  file_move(path(test_dir, "simple.Rmd"), path(test_dir, "01-simple.Rmd"))
  expect_true(has_clear_build_chain(test_dir)$state)

  expect_true(has_tidy_media(test_dir)$state)
  expect_true(has_tidy_images(test_dir)$state)
  expect_true(has_tidy_code(test_dir)$state)
  expect_true(has_tidy_raw_data(test_dir)$state)
  expect_true(has_tidy_data(test_dir)$state)

  test_dir <- sandbox(dir)
  expect_true(has_tidy_scripts(test_dir)$state)
  file_create(path(test_dir, "second.R"))
  expect_false(has_tidy_scripts(test_dir)$state)

  # compilation
  expect_true(has_no_absolute_paths(test_dir)$state)
  expect_true(has_only_portable_paths(test_dir)$state)
  expect_true(has_no_randomness(test_dir, seed_old)$state)

})

