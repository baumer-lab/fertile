#' @rdname check_path
#' @export
#' @examples
#' \dontrun{
#' check_file_exists(tempfile())
#' }
check_file_exists <- function(path, strict = TRUE) {
  message("Checking for paths to files that don't exist...")
  bad <- path[!fs::file_exists(path)]
  out <- tibble::tibble(
    path = bad,
    problem = "File does not exist",
    solution = 'Correct the path to the file'
  )
  if (strict && nrow(out) > 0) {
    rlang::abort("Detected paths to files that don't exist")
  }
  out
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
