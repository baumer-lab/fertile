#' @rdname checks
#' @export

has_readme <- function(path = ".", ...) {
  tibble::tibble(
    state = length(dir_ls(path, regexp = "README")) > 0,
    problem = "No README found in project directory",
    solution = "Create README",
    help = "?fs::file_create()",
    error = list(
      tibble::tibble(
        culprit = "README.md",
        expr = glue("fs::file_create('{culprit}')")
      )
    )
  )
}

#' @rdname checks
#' @export
has_proj_root <- function(path = ".", ...) {
  tibble::tibble(
    state = length(dir_ls(path, regexp = ".Rproj")) == 1,
    problem = "No .Rproj file found",
    solution = "Create RStudio project",
    help = "?usethis::create_project()",
    error = list(
      tibble::tibble(
        culprit = "*.Rproj",
        expr = "usethis::create_project()"
      )
    )
  )
}

#' @rdname checks
#' @export
has_no_absolute_paths <- function(path = ".", ...) {
  paths <- log_report() %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path)

  bad <- paths %>%
    fs::is_absolute_path()

  tibble::tibble(
    state = !any(bad),
    problem = "Absolute paths are likely non-portable",
    solution = "Use relative paths. Move files if necessary.",
    help = "?fs::file_move(); ?fs::path_rel()",
    error = list(
      tibble::tibble(
        culprit = paths[bad],
        expr = glue("fs::file_move({culprit}, here::here()); fs::path_rel({culprit})")
      )
    )
  )
}

#' @rdname checks
#' @export
has_only_portable_paths <- function(path = ".", ...) {
  paths <- log_report() %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path)

  good <- paths %>%
    is_path_portable()

  tibble::tibble(
    state = all(good),
    problem = "Non-portable paths won't necessarily work for others",
    solution = "Use relative paths.",
    help = "?fs::path_rel()",
    error = list(
      tibble::tibble(
        culprit = paths[!good],
        expr = glue("fs::path_rel({culprit})")
      )
    )
  )
}

#' @rdname checks
#' @param seed_old The old seed before the code is rendered
#' @export
has_no_randomness <- function(path = ".", seed_old, ...) {
  tibble::tibble(
    state = identical(seed_old, .Random.seed),
    problem = "Your code uses randomness",
    solution = "Set a seed using `set.seed()` to ensure reproducibility.",
    help = "?set.seed()",
    error = list(
      tibble::tibble(
        culprit = "?",
        expr = glue("set.seed({sample(1:1e5, 1)})")
      )
    )
  )
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
