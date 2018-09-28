#' Check if paths are relative to project root
#' @rdname checks
#' @export
#' @inheritParams fs::path_rel
#' @importFrom here here
#' @return A logical vector
#' @examples
#' is_path_here(test_paths$path)

is_path_here <- function(path) {
  !grepl("\\.\\.", path_rel_here(path))
}

#' @rdname checks
#' @export

check_path_here <- function(path) {
  if (!is_path_here(path)) {
    stop(paste(path, "is not within the project directory"))
  }
}

check_path_absolute <- function(path) {
  if (fs::is_absolute_path(path)) {
    stop(paste(path, "is an absolute path. Use a relative path instead. See ?fs::path_rel"))
  }
}

#' @rdname checks
#' @export
#' @importFrom fs path_rel
#' @importFrom here here

path_rel_here <- function(path) {
  fs::path_rel(path, start = here::here())
}


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
