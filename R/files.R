#' Does file exist in project tree?
#' @rdname checks
#' @export
#' @param path A vector of paths
#' @return A logical vector
#' @examples
#' file_exists_here(test_paths$path)

file_exists_here <- function(path) {
  inside <- is_path_here(path)
  exists <- fs::file_exists(path)
  inside & exists
}

#' @rdname checks
#' @export
#' @examples
#' check_file_here("data.csv")
check_file_here <- function(path) {
  if (!fs::file_exists(path)) {
    stop(paste(path, "cannot be found"))
  }
  if (!is_path_here(path)) {
    stop(paste(path, "exists, but is outside the project directory"))
  }
}

#' Rename R Markdown files
#' @export
#' @importFrom glue glue
#' @importFrom fs is_file path_ext_set
#' @importFrom stringr str_subset
#' @examples
#' rename_Rmd(list.files(recursive = TRUE))

# rename_Rmd <- function(path) {
#   valid <- path[fs::is_file(path)] %>%
#     stringr::str_subset("\\.rmd$")
#   message(glue::glue("Renaming {length(valid)} files to .Rmd"))
#   # fs::path_ext_set(valid, ".Rmd")
# }
