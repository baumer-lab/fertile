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
  push(file, "read_csv")
  if (fs::file_exists(file) & !path_within(file)) {
    stop(paste(file, "exists, but is outside the project directory"))
  }
  if (!path_within(file)) {
    stop(paste(file, "is not within the project directory"))
  }
  readr::read_csv(file, ...)
}

#' Logging of file paths
#' @param x the path to capture
#' @param .f the function calling \code{x}
#' @importFrom here here
#' @importFrom readr write_csv read_csv
#' @importFrom tibble tibble add_row
#' @importFrom fs file_create
#' @importFrom dplyr distinct
push <- function(x, .f) {
  log <- here::here(".fertile_paths.csv")
  fs::file_create(log)
  old_paths <- readr::read_csv(log)
  if (nrow(old_paths) < 1) {
    new_paths <- tibble::tibble(path = x, func = .f, timestamp = Sys.time())
  } else {
    new_paths <- old_paths %>%
      tibble::add_row(path = x, func = .f, timestamp = Sys.time()) %>%
      dplyr::distinct()
  }
  readr::write_csv(new_paths, path = log)
}


#' Reporting of logged file paths
#' @export
#' @importFrom here here
#' @importFrom readr read_csv
#' @importFrom fs file_create
#' @examples
#' report()


report <- function() {
  log <- here::here(".fertile_paths.csv")
  fs::file_create(log)
  readr::read_csv(log)
}
