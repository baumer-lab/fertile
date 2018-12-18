utils::globalVariables(c(".", "value", "ext", "n", "timestamp", "size", "put_in",
"cmd", "dir_rel", "path_new", "mime", "package", "N", "state", "problem", "help",
"solution"))

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
  files <- dir_info(path, recursive = TRUE, type = "file") %>%
    dplyr::select(file = path, size) %>%
    dplyr::mutate(ext = path_ext(file),
                  mime = mime::guess_type(file),
                  path_rel = path_rel(file, start = path)
    )
  if (!any(grepl("README", files$file))) {
    rlang::warn(paste("Please include a README file in", path_abs(path)))
  }
  files
}

#' @rdname proj_test
#' @param files List of files returned by \code{\link{proj_analyze}}
#' @export

proj_suggest_moves <- function(files) {
  guess_root <- path_norm(path_common(files$file))
  # if there is only one file in the directory, fix it
  if (!is_dir(guess_root)) {
    guess_root <- path_dir(guess_root)
  }

  files_to_move <- files %>%
    # only suggest moves for files at root level
    dplyr::filter(path_dir(file) == guess_root) %>%
    dplyr::mutate(
      dir_rel = dplyr::case_when(
        grepl("(README|DESCRIPTION|NAMESPACE|LICENSE)", path_file(file)) ~ path("."),
        tolower(ext) == "rproj" ~ path("."),
        tolower(ext) == "r" ~ path("R"),
        tolower(ext) %in% c("rda", "rdata") ~ path("data"),
        tolower(ext) %in% c("dat", "csv", "tsv", "xml", "json", "zip") ~ path("data-raw"),
        tolower(ext) == "txt" & size > "10K" ~ path("data-raw"),
        tolower(ext) %in% c("rmd", "rnw", "md") ~ path("vignettes"),
        grepl("csrc", mime) ~ path("src", "c"),
        grepl("c\\+\\+", mime) ~ path("src", "cpp"),
        grepl("py", mime) ~ path("src", "python"),
        grepl("ruby", mime) ~ path("src", "ruby"),
        grepl("perl", mime) ~ path("src", "perl"),
        grepl("scala", mime) ~ path("src", "scala"),
        grepl("javascript", mime) ~ path("src", "javascript"),
        grepl("java", mime) ~ path("src", "java"),
        grepl("sql", mime) ~ path("inst", "sql"),
        grepl("text/", mime) ~ path("inst", "text"),
        grepl("image/", mime) ~ path("inst", "image"),
        grepl("audio/", mime) ~ path("inst", "audio"),
        grepl("video/", mime) ~ path("inst", "video"),
        TRUE ~ path("inst", "other")
      ),
      path_new = path_norm(path(guess_root, dir_rel, path_file(file)))
    ) %>%
    dplyr::filter(path_dir(path_new) != path_dir(file)) %>%
    dplyr::mutate(cmd = paste0("file_move('", file,
                               "', fs::dir_create('", path_dir(path_new), "'))"))
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
  r_code <- dir_ls(path = path, type = "file", recursive = TRUE,
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

  # find all R, Rmd, rmd files and run them?
  # this is the easyMake part
  dir <- tempdir()

  rmd <- dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.(r|R)md$")
  suppressMessages(
    rmarkdown::render(rmd, output_dir = dir)
  )

  r_script <- dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.R$")
  suppressMessages(
    purrr::map_lgl(r_script, testthat::source_file)
  )
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
            path_file(path_abs(x$proj_dir))))
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
#' @description A laundry list of small checks that help make your project
#' more likely to be reproducible
#' @export
#' @importFrom usethis ui_todo ui_done
#' @importFrom rlang eval_tidy sym
#' @importFrom glue glue
#' @inheritParams proj_root
#' @param ... currently ignored
#' @return a \code{\link[tibble]{tibble}} of checks and their results

check <- function(path = ".", ...) {
  # Set up checks
  checks <- c(
    "has_tidy_media",
    "has_tidy_images",
    "has_tidy_code",
    "has_tidy_raw_data",
    "has_tidy_data",
    "has_tidy_scripts",
    "has_readme",
    "has_no_lint",
    "has_proj_root",
    "has_no_absolute_paths",
    "has_only_portable_paths",
    "has_no_randomness"
  )

  needs_compile <- function(x) {
    attr(rlang::eval_tidy(rlang::sym(x)), "req_compilation")
  }

  must_compile <- checks %>%
    purrr::map_lgl(needs_compile)

  # Capture state information
  seed_old <- .Random.seed
  # log_old <- log_report(path)

  # Compile if necessary
  if (any(must_compile)) {
    msg("Compiling...")
    tryCatch(
      proj_render(path),
      error = function(e) {
        message(glue::glue("{e}\n"))
      }
    )
  }

  # Run the checks
  msg("Running reproducibility checks")
  # Need tidy eval here!!

  args <- rlang::exprs(path = path, seed_old = seed_old)
  out <- purrr::map_dfr(checks, rlang::exec,
                      path = path, seed_old = seed_old) %>%
    dplyr::mutate(fun = checks)

  class(out) <- c("fertile_check", class(out))

  # Display the checks
  print(out)

  cat("\n")
  msg("Summary of fertile checks")
  cat("\n")
  ui_done(glue::glue("Reproducibility checks passed: {sum(out$state)}"))
  if (any(out$state == FALSE)) {
    ui_todo(glue::glue("Reproducibility checks to work on: {sum(!out$state)}"))
    out %>%
      dplyr::filter(state == FALSE) %>%
      dplyr::select(problem, solution, help) %>%
      print()
  }

  invisible(out)
}

