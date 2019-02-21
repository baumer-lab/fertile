#' Logging and reporting of file paths
#' @param x the path to capture
#' @param .f the function calling \code{x}
#' @param path Path to the \code{fertile} log file. Defaults to \code{\link{path_log}}
#' @description These functions provide access to a log file, located at
#' \code{.fertile_paths.csv} in the project directory, that records the executed
#' paths passed to commonly-used input and output functions.
#' @seealso \code{\link{shims}}
#' @export

log_push <- function(x, .f, path = proj_root()) {
  log <- log_touch(path)
  old_paths <- log_report(path)
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

log_report <- function(path = proj_root()) {
  message(paste("Reading from", path_log(path)))
  readr::read_csv(log_touch(path), col_types = "ccT")
}

#' @rdname log_push
#' @export

log_clear <- function(path = proj_root()) {
  log <- log_touch(path)
  if (file_exists(log)) {
    file_delete(log)
  }
}

#' @rdname log_push
#' @export

log_touch <- function(path = proj_root()) {
  file_create(path_log(path))
  path_log(path)
}

#' @rdname log_push
#' @export

path_log <- function(path = proj_root()) {


  if(Sys.getenv("FERTILE_RENDER_MODE") == TRUE){

    path_abs(path(path, ".fertile_render_log.csv"))
  }
  else{

    path_abs(path(path, ".fertile_log.csv"))
  }

}


