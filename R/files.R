#' @rdname check_path
#' @export
#' @examples
#' \dontrun{
#' check_file_exists(tempfile())
#' }
check_file_exists <- function(path, strict = TRUE) {
  message("Checking for paths to files that don't exist...")
  bad <- path[!file_exists(path)]
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

#' @rdname checks
#' @export

is_readme_exists <- function(path = ".", ...) {
  length(dir_ls(path, regexp = "README")) > 0
}

#' @rdname checks
#' @export
has_proj_root <- function(path = ".") {
  length(dir_ls(path, regexp = ".Rproj")) == 1
}


#' Rename R Markdown files
#' @export
#' @importFrom glue glue
#' @importFrom stringr str_subset
#' @examples
#' rename_Rmd(list.files(recursive = TRUE))

# rename_Rmd <- function(path) {
#   valid <- path[is_file(path)] %>%
#     stringr::str_subset("\\.rmd$")
#   message(glue::glue("Renaming {length(valid)} files to .Rmd"))
#   # path_ext_set(valid, ".Rmd")
# }
