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

#' File checks
#' @name checks
#' @param file a path to a file
#' @export
#' @examples
#' checks(test_paths$path[1])
#' checks(test_paths$path[3])

checks <- function(file) {
  check_path_absolute(file)
  check_path_here(file)
  check_file_here(file)
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
  log <- touch()
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
  readr::read_csv(touch())
}

#' @rdname report
#' @export

clear <- function() {
  log <- touch()
  if (fs::file_exists(log)) {
    fs::file_delete(log)
  }
}

#' @rdname report
#' @export

touch <- function() {
  log <- here::here(".fertile_paths.csv")
  fs::file_create(log)
  log
}
