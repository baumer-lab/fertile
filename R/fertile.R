#' Analyze project for reproducibility
#' @param path Path to package root
#' @return A \code{fertile} object
#' @export
#' @importFrom tibble enframe
#' @importFrom magrittr %>%
#' @importFrom fs dir_ls path_ext
#' @importFrom dplyr select mutate group_by count arrange
#' @examples
#' proj_test()

proj_test <- function(path = ".") {
  message("Checking for reproducibility")

  files <- proj_analyze(path)
  proj_render(path)
  x <- proj_report(path)

  class(x) <- c("fertile", class(x))
  x
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export

proj_analyze <- function(path = ".") {
  message("Analyzing project file structure...")
  files <- fs::dir_ls(path, recursive = TRUE, type = "file") %>%
    tibble::enframe() %>%
    dplyr::select(path = value) %>%
    dplyr::mutate(ext = fs::path_ext(path))

  x <- files %>%
    dplyr::group_by(ext) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n))

  message("fertile found the following files:")
  print(x)

  if (nrow(y <- dplyr::filter(x, ext == "rmd")) > 0) {
    message("Consider renaming `*.rmd` files to `*.Rmd`. Use `?fs::file_move()`")
  }

  invisible(files)
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @importFrom rmarkdown render
#' @importFrom testthat source_file
#' @importFrom purrr map_lgl
#' @export

proj_render <- function(path = ".") {
  message("Rendering R scripts...")
  # find all R, Rmd, rmd files and run them?
  # this is the easyMake part
  dir <- tempdir()

  rmd <- fs::dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.(r|R)md$")
  rmarkdown::render(rmd, output_dir = dir)

  r_script <- fs::dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.R$")
  purrr::map_lgl(r_script, testthat::source_file)
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @importFrom dplyr inner_join select
#' @export

proj_report <- function(path = ".") {
  message("Generating reproducibility report...")
  # tell you what you did wrong
  x <- log_report()
  # run checks on these paths
  y <- check_path(x$path, strict = FALSE)
  dplyr::inner_join(x, y, by = "path") %>%
    dplyr::select(-timestamp)
}

#' @rdname proj_test
#' @inheritParams base::print
#' @export

print.fertile <- function(x, ...) {
  NextMethod(x, ...)
}

