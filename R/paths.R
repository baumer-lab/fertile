#' @rdname check_path
#' @export
#' @inheritParams fs::path_has_parent
#' @importFrom fs path_has_parent
#' @importFrom purrr map_lgl
#' @return A logical vector
#' @examples
#' is_path_safe(c(tempfile(), "~/.Rprofile", "../data.csv"))

is_path_safe <- function(path, parent = ".") {
  purrr::map_lgl(path, is_path_is_safe_, parent = parent)
}

#' @inheritParams fs::path_has_parent
#' @importFrom fs path_has_parent path_rel path path_norm

is_path_is_safe_ <- function(path, parent = ".") {
  fs::path_has_parent(path, parent) &
    identical(fs::path_rel(path, start = parent), fs::path_norm(fs::path(path)))
}

#' @rdname check_path
#' @export

check_path_is_safe <- function(path, parent = ".", strict = TRUE) {
  message("Checking for paths outside project directory...")
  bad <- path[!is_path_safe(path, parent)]
  out <- tibble::tibble(
    path = bad,
    problem = "Path is not contained within the project directory",
    solution = 'Move the file and/or use a relative path. See ?fs::path_rel()'
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
    solution = 'Use a relative path. See ?path_rel()'
  )
  if (strict && nrow(out) > 0) {
    stop("Detected absolute paths")
  }
  out
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

check_path <- function(path, parent = ".", strict = TRUE) {
  dplyr::bind_rows(
    check_path_absolute(path, strict),
    check_path_is_safe(path, parent, strict),
    check_file_exists(path, strict)
  )
}

