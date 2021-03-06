#' Logging and reporting of file paths
#' @param x the path to capture
#' @param .f the function calling \code{x}
#' @param path Path to the \code{fertile} log file. Defaults to \code{\link{path_log}}
#' @seealso \code{\link{shims}}
#' @export
#' @keywords internal

log_push <- function(x, .f, path = proj_root()) {
  log <- log_touch(path)
  old_paths <- log_report(path)

  # Get the absolute path of the files
  if (is_file(x) | is_dir(x)) {
    abs <- as.character(fs::path_abs(x))
  } else {
    abs <- NA
  }

  if (nrow(old_paths) < 1) {
    new_paths <- tibble(
      path = as.character(x),
      path_abs = as.character(abs),
      func = as.character(.f),
      "timestamp" = Sys.time()
    )
  } else {
    new_paths <- old_paths %>%
      tibble::add_row(
        path = as.character(x),
        path_abs = as.character(abs),
        func = as.character(.f),
        "timestamp" = Sys.time()
      ) %>%
      dplyr::distinct()
  }


  readr::write_csv(new_paths, file = log)
}

#' Logging and reporting of file paths
#' @description These functions provide access to a log file, located in the
#' directory given by path `x`, that records the executed paths/arguments passed to
#' commonly-used input and output functions in R code located in that directory.
#' @export
#' @param path Path to the directory you want to access the log for
#' @section log_report:
#' Return the log of functions/paths you have used interactively while
#' within a project directory.
#'
#' \code{log_report("directory")}
#' OR, to look at your project root:
#' \code{log_report()}

log_report <- function(path = proj_root()) {
  if (Sys.getenv("FERTILE_RENDER_MODE") == FALSE) {
    message(paste("Reading from", path_log(path)))
  }
  readr::read_csv(log_touch(path), col_types = "cccT")
}


#' @rdname log_report
#' @export
#' @section render_log_report:
#' View the render log for a directory, which shows information about
#' paths, packages, and randomness seeds involved in the rendering
#' of a project's files.
#' \code{render_log_report("directory")}


render_log_report <- function(path = proj_root()) {
  message(paste("Reading from", path_abs(path(path, ".fertile_render_log.csv"))))

  if (has_rendered(path) == FALSE) {
    proj_render(path)
  }

  readr::read_csv(path_abs(path(path, ".fertile_render_log.csv")), col_types = "cccT")
}



#' @rdname log_report
#' @export
#' @section log_clear:
#' Delete/clear the log file for a directory.
#'
#' \code{log_clear("directory")}
#' OR, to clear your project root:
#' \code{log_clear()}

log_clear <- function(path = proj_root()) {
  log <- log_touch(path)
  if (file_exists(log)) {
    file_delete(log)
  }
}


#' Create a new log file and return the path to the log
#' @keywords internal
#' @export

log_touch <- function(path = proj_root()) {
  file_create(path_log(path))
  path_log(path)
}

#' @rdname log_report
#' @export
#' @section path_log:
#' Return the path to the log file in a given directory.
#'
#' \code{path_log("directory")}
#' OR, to get the log file for your project root:
#' \code{path_log()}

path_log <- function(path = proj_root()) {
  if (Sys.getenv("FERTILE_RENDER_MODE") == TRUE) {
    path_abs(path(path, ".fertile_render_log.csv"))
  }
  else {
    path_abs(path(path, ".fertile_log.csv"))
  }
}

#' Utility function to help w/ controlling interactive logging functionality
#' @export
#' @keywords internal

interactive_log_on <- function() {
  if (Sys.getenv("IN_TESTTHAT") == TRUE & Sys.getenv("LOGGING_ON") == TRUE) {
    return(TRUE)
  } else if (Sys.getenv("IN_TESTTHAT") != TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
