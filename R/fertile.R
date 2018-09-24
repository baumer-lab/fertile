#' Run reproducibility checks
#' @param path Path to package root
#' @return A \code{\link{fertile}} object
#' @export
#' @importFrom tibble enframe
#' @importFrom magrittr %>%
#' @importFrom fs dir_ls path_ext
#' @importFrom dplyr select mutate group_by count arrange
#' @examples
#' check()

check <- function(path = ".") {
  paths <- fs::dir_ls(path, recursive = TRUE, type = "file") %>%
    tibble::enframe() %>%
    dplyr::select(path = value) %>%
    dplyr::mutate(ext = fs::path_ext(path))

  message("Found the following files:")
  x <- paths %>%
    dplyr::group_by(ext) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n))

  class(x) <- c("fertile", class(x))
  x
}

#' @rdname check
#' @inheritParams base::print
#' @export

print.fertile <- function(x, ...) {
  message("Checking for reproducibility")
  NextMethod(x, ...)
}
