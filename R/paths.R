#' Check if paths ore relative to project root
#' @export
#' @inheritParams fs::path_rel
#' @return a logical vector
#' @examples
#' path_within(test_paths$path)

path_within <- function(path, start = ".") {
  fs::path_rel(path, start) == path
}

#' Find paths in a project
#' @export

path_find_file <- function(path) {
  if (!fs::is_file(path)) {
    stop(glue::glue("{path} is not a file."))
  }
  lines <- readLines(path) %>%
    stringr::str_subset("(read.csv\\(|load\\(|read_csv\\()")
}

#'



#' Check Paths for portability
#' @export
#' @param path a vector of paths
#' @examples
#' path_check(test_paths$path)

path_check <- function(path) {
  dplyr::bind_rows(
    path_tilde(path),
    path_windows(path),
    path_mac(path),
    path_unix(path)
  )
}

path_tilde <- function(path) {
  message("Checking for relative paths...")
  bad <- stringr::str_subset(path, "^~/")
  tibble::tibble(
    path = bad,
    problem = "Contains a tilde",
    solution = 'Move file and use here::here("data", basename(bad))'
  )
}

path_tilde <- function(path) {
  message("Checking for relative paths...")
  bad <- stringr::str_subset(path, "^~/")
  tibble::tibble(
    path = bad,
    problem = "Contains a tilde",
    solution = 'Move file and use here::here("data", basename(bad))'
  )
}

path_windows <- function(path) {
  message("Checking for paths that will only work on Windows...")
  bad <- stringr::str_subset(path, "^[A-Z]://")
  tibble::tibble(
    path = bad,
    problem = "Contains a drive letter and will likely only work on Windows",
    solution = 'Move file and use here::here("data", basename(bad))'
  )
}

path_mac <- function(path) {
  message("Checking for paths that will only work on Mac OS X...")
  bad <- stringr::str_subset(path, "^/Users/.+/")
  tibble::tibble(
    path = bad,
    problem = "/Users/ will likely only work on Mac OS X",
    solution = 'Move file and use here::here("data", basename(bad))'
  )
}

path_unix <- function(path) {
  message("Checking for paths that will only work on *NIX...")
  bad <- stringr::str_subset(path, "^/home/.+/")
  tibble::tibble(
    path = bad,
    problem = "/home/ will likely only work on *NIX",
    solution = 'Move file and use here::here("data", basename(bad))'
  )
}
