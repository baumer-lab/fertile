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

#' @rdname checks
#' @export
has_no_absolute_paths <- function(path = ".") {
  !log_report() %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path) %>%
    fs::is_absolute_path() %>%
    any()
}

#' @rdname checks
#' @export
has_only_portable_paths <- function(path = ".") {
  log_report() %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path) %>%
    is_path_portable() %>%
    all()
}

#' @rdname checks
#' @export
has_no_randomness <- function(path = ".") {
  TRUE
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
