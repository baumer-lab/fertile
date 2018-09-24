#' Does file exist in project tree?
#' @export
#' @param path A vector of paths
#' @return A logical vector
#' @examples
#' file_exists_within(test_paths$path)

file_exists_within <- function(path) {
  inside <- path_within(path)
  exists <- fs::file_exists(path)
  inside & exists
}


#' Rename R Markdown files
#' @export
#' @importFrom glue glue
#' @importFrom fs is_file path_ext_set
#' @importFrom stringr str_subset
#' @examples
#' rename_Rmd(list.files(recursive = TRUE))

rename_Rmd <- function(path) {
  valid <- path[fs::is_file(path)] %>%
    stringr::str_subset("\\.rmd$")
  message(glue::glue("Renaming {length(valid)} files to .Rmd"))
  # fs::path_ext_set(valid, ".Rmd")
}
