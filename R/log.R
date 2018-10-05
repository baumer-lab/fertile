#' Logging and reporting of file paths
#' @param x the path to capture
#' @param .f the function calling \code{x}
#' @importFrom here here
#' @importFrom readr write_csv read_csv
#' @importFrom tibble tibble add_row
#' @importFrom fs file_create
#' @importFrom dplyr distinct
#' @description These functions provide access to a log file, located at
#' \code{.fertile_paths.csv} in the project directory, that records the executed
#' paths passed to commonly-used input and output functions.
#' @seealso \code{\link{shims}}

log_push <- function(x, .f) {
  log <- log_touch()
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

#' @rdname log_push
#' @export
#' @importFrom here here
#' @importFrom readr read_csv
#' @importFrom fs file_create
#' @examples
#' log_report()

log_report <- function() {
  readr::read_csv(log_touch())
}

#' @rdname log_push
#' @export

log_clear <- function() {
  log <- log_touch()
  if (fs::file_exists(log)) {
    fs::file_delete(log)
  }
}

#' @rdname log_push
#' @export

log_touch <- function() {
  log <- here::here(".fertile_paths.csv")
  fs::file_create(log)
  log
}