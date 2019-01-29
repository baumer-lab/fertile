#' Logging and reporting of file paths
#' @param x the path to capture
#' @param .f the function calling \code{x}
#' @param path Path to the \code{fertile} log file. Defaults to \code{\link{path_log}}
#' @description These functions provide access to a log file, located at
#' \code{.fertile_paths.csv} in the project directory, that records the executed
#' paths passed to commonly-used input and output functions.
#' @seealso \code{\link{shims}}
#' @export

log_push <- function(x, .f, path = path_log()) {
  log <- log_touch(path)
  old_paths <- log_report(log)
  if (nrow(old_paths) < 1) {
    new_paths <- tibble(path = x, func = .f, timestamp = Sys.time())
  } else {
    new_paths <- old_paths %>%
      tibble::add_row(path = x, func = .f, timestamp = Sys.time()) %>%
      dplyr::distinct()
  }
  readr::write_csv(new_paths, path = log)
}

#' @rdname log_push
#' @export
#' @examples
#' log_report()

log_report <- function(path = path_log()) {
  message(paste("Reading from", path))
  readr::read_csv(log_touch(path), col_types = "ccT")
}

#' @rdname log_push
#' @export

log_clear <- function(path = path_log()) {
  log <- log_touch(path)
  if (file_exists(log)) {
    file_delete(log)
  }
}

#' @rdname log_push
#' @export

log_touch <- function(path = path_log()) {
  file_create(path)
  path
}

#' @rdname log_push
#' @export

path_log <- function(path = proj_root()) {
  path_abs(path(path, ".fertile.log.csv"))
}

