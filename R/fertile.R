utils::globalVariables(c(".", "value", "ext", "n", "timestamp", "size", "put_in",
"cmd", "dir_rel", "path_new", "mime", "package", "N"))

#' Analyze project for reproducibility
#' @param path Path to project root
#' @return A \code{fertile} object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' proj_test()
#' }

proj_test <- function(path = ".") {
  msg("Checking for reproducibility")

  report <- proj_analyze(path)
  proj_render(path)
  report$paths <- proj_analyze_paths(path)

  report
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export

proj_analyze <- function(path = ".") {
  pkgs <- proj_analyze_pkgs(path)
  files <- proj_analyze_files(path)
  suggestions <- proj_suggest_moves(files)
  x <- list(proj_dir = path, packages = pkgs, files = files,
            suggestions = suggestions, paths = NULL)
  class(x) <- c("fertile", class(x))
  x
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export

proj_analyze_files <- function(path = ".") {
#  msg("Analyzing project file structure")
  files <- fs::dir_info(path, recursive = TRUE, type = "file") %>%
    dplyr::select(file = path, size) %>%
    dplyr::mutate(ext = fs::path_ext(file),
                  mime = mime::guess_type(file),
                  path_rel = fs::path_rel(file, start = path)
    )
  if (!any(grepl("README", files$file))) {
    rlang::warn(paste("Please include a README file in", fs::path_abs(path)))
  }
  files
}

#' @rdname proj_test
#' @param files List of files returned by \code{\link{proj_analyze}}
#' @export

proj_suggest_moves <- function(files) {
  guess_root <- fs::path_norm(fs::path_common(files$file))
  # if there is only one file in the directory, fix it
  if (!fs::is_dir(guess_root)) {
    guess_root <- fs::path_dir(guess_root)
  }

  files_to_move <- files %>%
    # only suggest moves for files at root level
    dplyr::filter(fs::path_dir(file) == guess_root) %>%
    dplyr::mutate(
      dir_rel = dplyr::case_when(
        grepl("(README|DESCRIPTION|NAMESPACE|LICENSE)", fs::path_file(file)) ~ fs::path("."),
        tolower(ext) == "rproj" ~ fs::path("."),
        tolower(ext) == "r" ~ fs::path("R"),
        tolower(ext) %in% c("rda", "rdata") ~ fs::path("data"),
        tolower(ext) %in% c("dat", "csv", "tsv", "xml", "json", "zip") ~ fs::path("data-raw"),
        tolower(ext) == "txt" & size > "10K" ~ fs::path("data-raw"),
        tolower(ext) %in% c("rmd", "rnw", "md") ~ fs::path("vignettes"),
        grepl("csrc", mime) ~ fs::path("src", "c"),
        grepl("c\\+\\+", mime) ~ fs::path("src", "cpp"),
        grepl("py", mime) ~ fs::path("src", "python"),
        grepl("ruby", mime) ~ fs::path("src", "ruby"),
        grepl("perl", mime) ~ fs::path("src", "perl"),
        grepl("scala", mime) ~ fs::path("src", "scala"),
        grepl("javascript", mime) ~ fs::path("src", "javascript"),
        grepl("java", mime) ~ fs::path("src", "java"),
        grepl("sql", mime) ~ fs::path("inst", "sql"),
        grepl("text/", mime) ~ fs::path("inst", "text"),
        grepl("image/", mime) ~ fs::path("inst", "image"),
        grepl("audio/", mime) ~ fs::path("inst", "audio"),
        grepl("video/", mime) ~ fs::path("inst", "video"),
        TRUE ~ fs::path("inst", "other")
      ),
      path_new = fs::path_norm(fs::path(guess_root, dir_rel, fs::path_file(file)))
    ) %>%
    dplyr::filter(fs::path_dir(path_new) != fs::path_dir(file)) %>%
    dplyr::mutate(cmd = paste0("fs::file_move('", file,
                               "', fs::dir_create('", fs::path_dir(path_new), "'))"))
  return(files_to_move)
}


#' @rdname proj_test
#' @param suggestions List of suggestsions returned by \code{\link{proj_analyze}}
#' @param execute Do you want to actually move the files to their recommended location?
#' @export

proj_move_files <- function(suggestions, execute = TRUE) {

  if (execute) {
    eval(parse(text = suggestions$cmd))
  }

}

#' @rdname proj_test
#' @export

proj_analyze_pkgs <- function(path = ".") {
#  msg("Analyzing packages used in project")
  r_code <- fs::dir_ls(path = path, type = "file", recursive = TRUE,
                       regexp = "\\.(?i)(r|rnw|rmd|rpres)$")
  pkgs <- purrr::map(r_code, requirements::req_file) %>%
    purrr::map(tibble::as.tibble) %>%
    purrr::map_dfr(dplyr::bind_rows, .id = "file") %>%
    dplyr::rename(package = value) %>%
    dplyr::group_by(package) %>%
    dplyr::summarize(N = dplyr::n(),
                     used_in = paste(file, collapse = ", ")) %>%
    dplyr::arrange(dplyr::desc(N))
  pkgs
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export

proj_render <- function(path = ".") {
  msg("Rendering R scripts...")
  # capture the .Random.seed
  seed_old <- .Random.seed

  # find all R, Rmd, rmd files and run them?
  # this is the easyMake part
  dir <- tempdir()

  rmd <- fs::dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.(r|R)md$")
  rmarkdown::render(rmd, output_dir = dir)

  r_script <- fs::dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.R$")
  purrr::map_lgl(r_script, testthat::source_file)

  # re-capture the .Random.seed and compare
  if (!identical(seed_old, .Random.seed)) {
    rlang::warn("It appears that your code uses randomness.
                Set a random seed using `set.seed()` to ensure reproducibility.")
  }
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export

proj_analyze_paths <- function(path = ".") {
  msg("Generating reproducibility report...")
  # tell you what you did wrong
  x <- log_report(path_log(path))
  # run checks on these paths
  y <- check_path(x$path, strict = FALSE)
  dplyr::inner_join(x, y, by = "path") %>%
    dplyr::select(-timestamp)
}

#' @rdname proj_test
#' @inheritParams base::print
#' @export

print.fertile <- function(x, ...) {
  msg(paste("Analysis of reproducibility for",
            fs::path_file(fs::path_abs(x$proj_dir))))
  msg("  Packages referenced in source code")
  print(x$packages, ...)
  msg("  Files present in directory")
  print(
    x$files %>%
      dplyr::select(file = path_rel, ext, size, mime) %>%
      dplyr::arrange(mime, ext, file), ...
    )
  msg("  Suggestions for moving files")
  print(dplyr::select(x$suggestions, path_rel, dir_rel, cmd), ...)
  msg("  Problematic paths logged")
  print(x$paths, ...)
}

#' Reproducbility checks
#' @name checks
#' @export
#' @inheritParams proj_root
#' @param ... currently ignore

check <- function(path = ".", ...) {
  checks <- tibble::tribble(
    ~name, ~fun,
    "Checking for single .Rproj file at root level", "has_proj_root",
    "Checking for README file(s) at root level", "is_readme_exists"
  )
  checks$state <- purrr::map_lgl(checks$fun, do.call,
                                 args = list(path = path))
  class(checks) <- c("fertile_check", class(checks))
  print(checks)
}

#' Print method for fertile checks
#' @inheritParams base::print
#' @export

print.fertile_check <- function(x, ...) {
  print_check <- function(row) {
    if (row$state) {
      done(row$name)
    } else
      todo(row$name)
#      if (!is.null(row$error)) {
#        msg(row$error)
#      }
  }

  x %>%
    split(.$fun) %>%
    purrr::walk(print_check)

  msg("Summary of fertile checks")
  done(glue::glue("Reproducibility checks passed: {sum(x$state == TRUE)}"))
  todo(glue::glue("Reproducibility checks to work on: {sum(x$state == FALSE)}"))

  invisible(x)
}

