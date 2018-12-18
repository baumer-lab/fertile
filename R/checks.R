#' @rdname check
#' @export

make_check <- function(fun, name, req_compilation,
                       state, problem, solution, help,
                       errors, ...) {
  x <- tibble::tibble(
    name = name,
    state = state,
    problem = problem,
    solution = solution,
    help = help,
    error = list(errors)
  )
  class(x) <- c("fertile_check", class(x))
  x
}

#' @rdname check
#' @inheritParams base::print
#' @export

print.fertile_check <- function(x, ...) {
  x %>%
    split(.$name) %>%
    purrr::walk(print_one_check)
}

#' @rdname check
#' @importFrom usethis ui_todo ui_done ui_line ui_code_block
#' @export

print_one_check <- function(x, ...) {
  if (x$state) {
    ui_done(x$name)
  } else {
    ui_todo(x$name)
    ui_code_block(" Problem: {x$problem}")
    ui_code_block(" Solution: {x$solution}")
    ui_code_block(" See for help: {x$help}")
    print(purrr::pluck(x$error, 1))
  }
}


#' @rdname check
#' @importFrom mime guess_type
#' @export

has_tidy_media <- function(path = ".", ...) {

  paths <- dir_ls(path)

  bad <- paths %>%
    mime::guess_type() %>%
    grepl("(audio|video)/", .)

  errors <- tibble::tibble(
    culprit = paths[bad],
    expr = glue("fs::file_move('{culprit}', here::here('media/'))")
  )

  make_check(
    name = "Checking for no A/V files at root level",
    state = !any(bad),
    problem = "A/V files in root directory clutter project",
    solution = "Move A/V files to media/ directory",
    help = "?fs::file_move()",
    errors = errors
  )
}
attr(has_tidy_media, "req_compilation") <- FALSE

#' @rdname check
#' @importFrom mime guess_type
#' @export

has_tidy_images <- function(path = ".", ...) {

  paths <- dir_ls(path)

  bad <- paths %>%
    mime::guess_type() %>%
    grepl("image/", .)

  errors <- tibble::tibble(
    culprit = paths[bad],
    expr = glue("fs::file_move('{culprit}', here::here('img/'))")
  )

  make_check(
    name = "Checking for no image files at root level",
    state = !any(bad),
    problem = "Image files in root directory clutter project",
    solution = "Move source files to img/ directory",
    help = "?fs::file_move()",
    error = errors
  )
}
attr(has_tidy_images, "req_compilation") <- FALSE

#' @rdname check
#' @export

has_tidy_code <- function(path = ".", ...) {

  paths <- dir_ls(path)

  bad <- paths %>%
    mime::guess_type() %>%
    grepl("(csrc|c\\+\\+|py|ruby|perl|scala|javascript|java|sql)", .)

  errors <- tibble::tibble(
    culprit = paths[bad],
    expr = glue("fs::file_move('{culprit}', here::here('src/'))")
  )

  make_check(
    name = "Checking for no source files at root level",
    state = !any(bad),
    problem = "Code source files in root directory clutter project",
    solution = "Move source files to src/ directory",
    help = "?fs::file_move()",
    error = errors
  )
}
attr(has_tidy_code, "req_compilation") <- FALSE


#' @rdname check
#' @export

has_tidy_raw_data <- function(path = ".", ...) {

  bad <- path %>%
    dir_info() %>%
    dplyr::mutate(ext = path_ext(path)) %>%
    dplyr::filter(tolower(ext) %in% c("dat", "csv", "tsv", "xml", "json", "zip") |
                    (tolower(ext) == "txt" & size > "10K")) %>%
    dplyr::pull(path)

  errors <- tibble::tibble(
    culprit = bad,
    expr = glue("fs::file_move('{culprit}', here::here('data-raw/'))")
  )

  make_check(
    name = "Checking for no raw data files at root level",
    state = length(bad) == 0,
    problem = "Raw data files in root directory clutter project",
    solution = "Move raw data files to data-raw/ directory",
    help = "?fs::file_move()",
    error = errors
  )
}
attr(has_tidy_raw_data, "req_compilation") <- FALSE

#' @rdname check
#' @export

has_tidy_data <- function(path = ".", ...) {

  bad <- dir_ls(path, regexp = "\\.(rda|rdata)$", ignore.case = TRUE)

  errors <- tibble::tibble(
    culprit = bad,
    expr = glue("fs::file_move('{culprit}', here::here('data/'))")
  )

  make_check(
    name = "Checking for no *.rda files at root level",
    state = length(bad) == 0,
    problem = "R data files in root directory clutter project",
    solution = "Move *.rda files to data/ directory",
    help = "?fs::file_move()",
    error = errors
  )
}
attr(has_tidy_data, "req_compilation") <- FALSE

#' @rdname check
#' @export

has_tidy_scripts <- function(path = ".", ...) {

  bad <- dir_ls(path, regexp = "\\.R$", ignore.case = TRUE)

  errors <- tibble::tibble(
    culprit = bad,
    expr = glue("fs::file_move('{culprit}', here::here('R/'))")
  )

  make_check(
    name = "Checking for no *.R scripts at root level",
    state = length(bad) == 0,
    problem = "R script files in root directory clutter project",
    solution = "Move *.R files to R/ directory",
    help = "?fs::file_move()",
    error = errors
  )
}
attr(has_tidy_scripts, "req_compilation") <- FALSE

#' @rdname check
#' @export

has_readme <- function(path = ".", ...) {
  errors <- tibble::tibble(
    culprit = "README.md",
    expr = glue("fs::file_create('{culprit}')")
  )

  make_check(
    name = "Checking for README file(s) at root level",
    state = length(dir_ls(path, regexp = "^README", ignore.case = TRUE)) > 0,
    problem = "No README found in project directory",
    solution = "Create README",
    help = "?fs::file_create()",
    error = errors
  )
}
attr(has_readme, "req_compilation") <- FALSE

#' @rdname check
#' @export
has_proj_root <- function(path = ".", ...) {
  errors <- tibble::tibble(
    culprit = "*.Rproj",
    expr = "usethis::create_project()"
  )

  make_check(
    name = "Checking for single .Rproj file at root level",
    state = length(dir_ls(path, regexp = "\\.Rproj$", ignore.case = TRUE)) == 1,
    problem = "No .Rproj file found",
    solution = "Create RStudio project",
    help = "?usethis::create_project()",
    error = errors
  )
}
attr(has_proj_root, "req_compilation") <- FALSE

#' @rdname check
#' @export
has_no_absolute_paths <- function(path = ".", ...) {
  paths <- log_report(path) %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path)

  bad <- paths %>%
    fs::is_absolute_path()

  errors <- tibble::tibble(
    culprit = as_fs_path(paths[bad]),
    expr = glue::glue("fs::file_move('{culprit}', here::here()); fs::path_rel('{culprit}')")
  )

  make_check(
    name = "Checking for no absolute paths",
    state = !any(bad),
    problem = "Absolute paths are likely non-portable",
    solution = "Use relative paths. Move files if necessary.",
    help = "?fs::file_move(); ?fs::path_rel()",
    error = errors
  )
}
attr(has_no_absolute_paths, "req_compilation") <- TRUE

#' @rdname check
#' @export
has_only_portable_paths <- function(path = ".", ...) {
  paths <- log_report(path) %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path)

  good <- paths %>%
    is_path_portable()

  errors <- tibble::tibble(
    culprit = as_fs_path(paths[!good]),
    expr = glue("fs::path_rel('{culprit}')")
  )

  make_check(
    name = "Checking for only portable paths",
    state = all(good),
    problem = "Non-portable paths won't necessarily work for others",
    solution = "Use relative paths.",
    help = "?fs::path_rel()",
    error = errors
  )
}
attr(has_only_portable_paths, "req_compilation") <- TRUE

#' @rdname check
#' @param seed_old The old seed before the code is rendered
#' @export
has_no_randomness <- function(path = ".", seed_old, ...) {

  errors <- tibble::tibble(
    culprit = "?",
    expr = glue("set.seed({sample(1:1e5, 1)})")
  )

  make_check(
    name = "Checking for no randomness",
    state = identical(seed_old, .Random.seed),
    problem = "Your code uses randomness",
    solution = "Set a seed using `set.seed()` to ensure reproducibility.",
    help = "?set.seed()",
    error = errors
  )
}
attr(has_no_randomness, "req_compilation") <- TRUE


#' @rdname check
#' @export
has_no_lint <- function(path = ".", ...) {

  files <- fs::dir_ls(path, recursive = TRUE, regexp = "\\.[rR]{1}(md)?$")
  x <- files %>%
    purrr::map(lintr::lint) %>%
    flatten_lints()
  print(x)
  make_check(
    name = "Checking code style for lint",
    state = length(x) == 0,
    problem = "Your code does not conform to tidyverse style",
    solution = "Use lintr::lint_package() for more details",
    help = "?lintr::lint_package()",
    error = NULL
  )
}
attr(has_no_lint, "req_compilation") <- FALSE


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
