#' @rdname check_path
#' @export
#' @importFrom here here
#' @importFrom fs path_has_parent
#' @importFrom purrr map_lgl
#' @return A logical vector
#' @examples
#' is_path_here(tempfile())

is_path_here <- function(path) {
  purrr::map_lgl(path, fs::path_has_parent, parent = here::here())
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
    check_path_here(path, strict),
    check_file_exists(path, strict)
  )
}

