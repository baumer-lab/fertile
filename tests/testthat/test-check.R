context("projects")

test_that("has functions work", {

  dir <- test_path("project_noob")
  test_dir <- sandbox(dir)

  # delete files we don't want

  r <- dir_ls(test_dir, regexp = "\\.R$")
  file_delete(r)
  pdf <- dir_ls(test_dir, regexp = "\\.pdf$")
  file_delete(pdf)

  # project roots

  expect_true(has_proj_root(test_dir)$state)
  rproj <- dir_ls(test_dir, regexp = "\\.Rproj$")
  file_delete(rproj)
  expect_false(has_proj_root(test_dir)$state)

  test_dir <- sandbox(dir)

  r <- dir_ls(test_dir, regexp = "\\.R$")
  file_delete(r)
  pdf <- dir_ls(test_dir, regexp = "\\.pdf$")
  file_delete(pdf)

  expect_true(has_no_nested_proj_root(test_dir)$state)
  dir_create(path(test_dir, "R"))
  fake <- path(test_dir, "R", "fake.Rproj")
  file_copy(rproj, fake)
  expect_false(has_no_nested_proj_root(test_dir)$state)

  test_dir <- sandbox(dir)

  r <- dir_ls(test_dir, regexp = "\\.R$")
  file_delete(r)
  pdf <- dir_ls(test_dir, regexp = "\\.pdf$")
  file_delete(pdf)

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

  r <- dir_ls(test_dir, regexp = "\\.R$")
  file_delete(r)
  pdf <- dir_ls(test_dir, regexp = "\\.pdf$")
  file_delete(pdf)

  expect_true(has_tidy_scripts(test_dir)$state)
  file_create(path(test_dir, "second.R"))
  expect_false(has_tidy_scripts(test_dir)$state)

  comments <- test_path("project_comments")
  temp_dir <- sandbox(comments)
  expect_false(has_well_commented_code(temp_dir)$state)
  expect_equal(length(has_well_commented_code(temp_dir)$error), 1)


  # compilation

  noob <- testthat::test_path("project_noob")
  random <- testthat::test_path("project_random")
  random_seed <- testthat::test_path("project_randomseed")

  #data_dir <- sandbox("tests/testthat/data")

  r <- dir_ls(noob, regexp = "\\.R$")
  file_delete(r)

  #.Random.seed is not recognized unless running RStudio
  # so checks involving randomness don't operate correctly in R CMD CHECK

  #expect_true(has_no_randomness(noob)$state)
  #expect_false(has_no_randomness(random)$state)
  #expect_true(has_no_randomness(random_seed)$state)

  #expect_true(has_no_absolute_paths(noob)$state)
  #expect_false(has_only_portable_paths(noob)$state)

  miceps <- testthat::test_path("project_miceps")

  r <- dir_ls(miceps, regexp = "\\.R$")
  file_delete(r)
  pdf <- dir_ls(miceps, regexp = "\\.pdf$")
  file_delete(pdf)

  expect_false(has_only_used_files(miceps)$state)
  expect_true(has_only_used_files(random)$state)

})

