#' @rdname check_path
#' @export
#' @importFrom here here
#' @return A logical vector
#' @examples
#' is_path_here(tempfile())

is_path_here <- function(path) {
  !grepl("\\.\\.", path_rel_here(path))
}

#' @rdname check_path
#' @export

check_path_here <- function(path, strict = TRUE) {
  message("Checking for paths outside project directory...")
  bad <- path[!is_path_here(path)]
  out <- tibble::tibble(
    path = bad,
    problem = "Path is not within the project directory",
    solution = 'Move the file and use a relative path. See ?fs::file_move()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected paths that lead outside the project directory")
  }
  out
}

check_path_absolute <- function(path, strict = TRUE) {
  message("Checking for absolute paths...")
  bad <- path[fs::is_absolute_path(path)]
  out <- tibble::tibble(
    path = bad,
    problem = "Absolute paths will likely only work on your computer",
    solution = 'Use a relative path. See ?path_rel_here()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected absolute paths")
  }
  out
}

#' @rdname check_path
#' @export
#' @importFrom fs path_rel
#' @importFrom here here

path_rel_here <- function(path) {
  fs::path_rel(path, start = here::here())
}


#' Check paths for portability
#' @export
#' @param path a vector of paths
#' @param strict logical indicating whether you want to stop on errors
#' @description Check paths for a variety of maladies
#' @examples
#' \dontrun{
#' check_path(tempfile())
#' }
#' check_path(tempfile(), strict = FALSE)
#' check_path(c("data.csv", "~/.Rprofile"), strict = FALSE)

check_path <- function(path, strict = TRUE) {
  dplyr::bind_rows(
    check_path_absolute(path, strict),
    check_path_tilde(path, strict),
    check_path_windows(path, strict),
    check_path_mac(path, strict),
    check_path_unix(path, strict),
    check_path_here(path, strict),
    check_file_here(path, strict)
  )
}

check_path_tilde <- function(path, strict = TRUE) {
  message("Checking for paths with tildes...")
  bad <- stringr::str_subset(path, "^~/")
  out <- tibble::tibble(
    path = bad,
    problem = "Contains a tilde",
    solution = 'Use a relative path. See ?path_rel_here()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected paths with tildes")
  }
  out
}

check_path_windows <- function(path, strict = TRUE) {
  message("Checking for paths that will only work on Windows...")
  bad <- stringr::str_subset(path, "^[A-Z]://")
  out <- tibble::tibble(
    path = bad,
    problem = "Contains a drive letter and will likely only work on Windows",
    solution = 'Use a relative path. See ?path_rel_here()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected paths with Windows drive letters")
  }
  out
}

check_path_mac <- function(path, strict = TRUE) {
  message("Checking for paths that will only work on Mac OS X...")
  bad <- stringr::str_subset(path, "^/Users/.+/")
  out <- tibble::tibble(
    path = bad,
    problem = "/Users/ will likely only work on Mac OS X",
    solution = 'Use a relative path. See ?path_rel_here()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected paths with Mac OS root-level user directories")
  }
  out
}

check_path_unix <- function(path, strict = TRUE) {
  message("Checking for paths that will only work on *NIX...")
  bad <- stringr::str_subset(path, "^/home/.+/")
  out <- tibble::tibble(
    path = bad,
    problem = "/home/ will likely only work on *NIX",
    solution = 'Use a relative path. See ?path_rel_here()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected paths with *NIX home directories")
  }
  out
}
