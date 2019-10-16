context("projects")

 test_that("project checking works", {

   # noob
   dir <- test_path("project_noob")
   test_dir <- sandbox(dir)

   x <- check(dir)
   expect_gt(nrow(x), 1)

   y <- check_some(dir, ends_with("root"))
   expect_equal(nrow(y), 2)

   expect_true(has_proj_root(test_dir)$state)
   expect_false(has_readme(test_dir)$state)
   expect_true(has_tidy_media(test_dir)$state)
   expect_true(has_tidy_images(test_dir)$state)
   expect_true(has_tidy_code(test_dir)$state)
   expect_true(has_tidy_raw_data(test_dir)$state)
   expect_true(has_tidy_data(test_dir)$state)

   # delete the files that are not supposed to be there

   r <- dir_ls(test_dir, regexp = "\\.R$")
   file_delete(r)
   pdf <- dir_ls(test_dir, regexp = "\\.pdf$")
   file_delete(pdf)

   expect_true(has_tidy_scripts(test_dir)$state)

   data_dir <- sandbox(test_path("data"))

   expect_true(has_no_randomness(test_dir)$state)
   expect_true(has_no_absolute_paths(test_dir)$state)
   expect_false(has_only_portable_paths(test_dir)$state)


   expect_warning(proj_analyze_files(test_dir), "README")
   expect_warning(x <- proj_test(test_dir), "README")


   expect_length(dir_ls(tempdir(), regexp = "simple.html$"), 1)
   expect_equal(nrow(x$packages), 3)
   # .Rbuildignore says to ignore .Rproj files!
   expect_equal(nrow(dplyr::filter(x$files, ext != "Rproj")), 1)
   expect_equal(nrow(x$suggestions), 1)
   expect_equal(nrow(x$paths), 2)

   proj_move_files(x$suggestions, execute = FALSE)
   expect_length(dir_ls(test_dir, type = "dir"), 0)
   proj_move_files(x$suggestions, execute = TRUE)
   expect_length(path_file(dir_ls(test_dir, type = "dir")), 1)

   # miceps
   dir <- test_path("project_miceps")
   test_dir <- sandbox(dir)
   x <- proj_test(test_dir)

   expect_equal(nrow(x$packages), 9)
   expect_equal(nrow(dplyr::filter(x$files, ext == "csv")), 3)
   expect_equal(nrow(dplyr::filter(x$files, ext == "docx")), 1)
   expect_equal(nrow(dplyr::filter(x$files, ext == "md")), 1)
   expect_equal(nrow(dplyr::filter(x$files, ext == "Rmd")), 1)
   expect_equal(nrow(dplyr::filter(x$files, ext == "png")), 2)
   expect_equal(nrow(dplyr::filter(x$files, ext == "Rproj")), 1)
   expect_equal(sum(x$suggestions$ext == "csv"), 3)
   expect_equal(sum(x$suggestions$ext == "docx"), 1)
   expect_equal(sum(x$suggestions$ext == "Rmd"), 1)
   expect_equal(sum(x$suggestions$ext == "png"), 2)
   expect_equal(nrow(x$paths), 0)

   proj_move_files(x$suggestions, execute = FALSE)
   expect_length(dir_ls(test_dir, type = "dir"), 0)
   proj_move_files(x$suggestions, execute = TRUE)
   expect_length(path_file(dir_ls(test_dir, type = "dir")), 3)
 })

