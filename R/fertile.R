#' Analyze project for reproducibility
#' @param path Path to package root
#' @return A \code{fertile} object
#' @export
#' @importFrom tibble enframe
#' @importFrom magrittr %>%
#' @importFrom fs dir_ls path_ext
#' @importFrom dplyr select mutate group_by count arrange
#' @examples
#' proj_analyze()

proj_analyze <- function(path = ".") {
  message("Checking for reproducibility")
  paths <- fs::dir_ls(path, recursive = TRUE, type = "file") %>%
    tibble::enframe() %>%
    dplyr::select(path = value) %>%
    dplyr::mutate(ext = fs::path_ext(path))

  x <- paths %>%
    dplyr::group_by(ext) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n))

  if (nrow(y <- dplyr::filter(x, ext == "rmd")) > 0) {
    message("Consider renaming `*.rmd` files to `*.Rmd`. Use `?fs::file_move()`")
  }

  class(x) <- c("fertile", class(x))
  message("fertile found the following files:")
  x
}

#' @rdname proj_analyze
#' @inheritParams base::print
#' @export

print.fertile <- function(x, ...) {
  NextMethod(x, ...)
}

#' Checking paths and files
#' @name checks
#' @param file a path to a file
#' @export
#' @examples
#' \dontrun{
#' file_check("data.csv")
#' file_check("~/.Rprofile")
#' file_check(tempdir())
#' }

file_check <- function(file) {
  check_path_absolute(file)
  check_path_here(file)
  check_file_here(file)
}
