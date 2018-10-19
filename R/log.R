#' Logging and reporting of file paths
#' @param x the path to capture
#' @param .f the function calling \code{x}
#' @param path Path to the \code{fertile} log file. Defaults to \code{\link{path_log}}
#' @importFrom readr write_csv read_csv
#' @importFrom tibble tibble add_row
#' @importFrom fs file_create
#' @importFrom dplyr distinct
#' @description These functions provide access to a log file, located at
#' \code{.fertile_paths.csv} in the project directory, that records the executed
#' paths passed to commonly-used input and output functions.
#' @seealso \code{\link{shims}}
#' @export

log_push <- function(x, .f, path = path_log()) {
  log <- log_touch(path)
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
#' @importFrom readr read_csv
#' @examples
#' log_report()

log_report <- function(path = path_log()) {
  suppressMessages(
    readr::read_csv(log_touch(path))
  )
}

#' @rdname log_push
#' @importFrom fs file_exists file_delete
#' @export

log_clear <- function(path = path_log()) {
  log <- log_touch(path)
  if (fs::file_exists(log)) {
    fs::file_delete(log)
  }
}

#' @rdname log_push
#' @importFrom fs file_create
#' @export

log_touch <- function(path = path_log()) {
  fs::file_create(path)
  message(paste("Reading from", path))
  path
}

#' @rdname log_push
#' @importFrom fs path path_abs
#' @export

path_log <- function(path = proj_root()) {
  fs::path_abs(fs::path(path, ".fertile.log.csv"))
}

