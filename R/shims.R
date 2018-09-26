#' Shims for common input/output functions
#' @export
#' @param file The path
#' @param ... arguments passed to functions
#' @importFrom fs file_exists
#' @importFrom readr read_csv
#' @seealso \code{\link[readr]{read_csv}}
#' @examples
#' if (require(readr)) {
#'   read_csv(test_paths$path)
#' }

read_csv <- function(file, ...) {
  message(paste("Reading data from", file))
  push(file)
  if (fs::file_exists(file) & !path_within(file)) {
    stop(paste(file, "exists, but is outside the project directory"))
  }
  if (!path_within(file)) {
    stop(paste(file, "is not within the project directory"))
  }
  readr::read_csv(file, ...)
}

#' Logging of file paths
#' @importFrom readr write_csv
push <- function(x) {
  readr::write_csv(data.frame(path = x), path = ".fertile_paths.csv", append = TRUE)
}
