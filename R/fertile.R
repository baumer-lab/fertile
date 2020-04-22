utils::globalVariables(c(".", "value", "ext", "n", "timestamp", "size",
                         "put_in", "cmd", "dir_rel", "path_new", "mime",
                         "package", "N", "state", "problem", "help", "func",
                         "solution", "filename", "desc", "modification_time", "install_call",
                         "fertile"))

#' Analyze project for reproducibility
#' @param path Path to project root
#' @return A \code{fertile} object
#' @export
#' @importFrom magrittr %>%
#' @section proj_test:
#' Create a full report of project reproducibility. Includes:
#' packages referenced in code, files in the directory and suggestions for moving them,
#' and a list of paths that are not portable.
#'
#' \code{proj_test("your project directory")}

proj_test <- function(path = ".") {
  msg("Checking for reproducibility")

  if (has_rendered(path) == FALSE){
    proj_render(path)
  }

  report <- proj_analyze(path)

  report$paths <- proj_analyze_paths(path)

  report
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export
#' @section proj_analyze:
#' Very similar to proj_test, except that this function does NOT
#' include a report of paths that are not portable.
#'
#' \code{proj_analyze("your project directory")}


proj_analyze <- function(path = ".") {

  if (has_rendered(path) == FALSE){
    proj_render(path)
  }

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
#' @section proj_analyze_files:
#' Provides a report of files present in a provided project directory.
#' Includes information about file size, extension, and a guess about the file type.
#'
#' \code{proj_analyze_files("your project directory")}

proj_analyze_files <- function(path = ".") {
#  msg("Analyzing project file structure")

  files <- dir_info(path, recurse = TRUE, type = "file") %>%
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
#' @param files List of files returned by \code{\link{proj_analyze_files}}
#' @export
#' @section proj_suggest_moves:
#' Takes a list of files returned by \code{\link{proj_analyze_files}}
#' and makes suggestions or where the files should be moved, as well
#' as a command for how to move them there.
#'
#' \code{files <- proj_analyze_files("your project directory")}
#'
#' \code{proj_suggest_moves(files)}

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
#' @param suggestions List of suggestsions returned by \code{\link{proj_suggest_moves}}
#' @param execute Do you want to actually move the files to their recommended location?
#' @export
#' @section proj_move_files:
#' Execute the suggested commands returned by \code{\link{proj_suggest_moves}}.
#'
#' \code{files <- proj_analyze_files("your project directory")}
#'
#' \code{suggestions <- proj_suggest_moves(files)}
#'
#' \code{proj_move_files(suggestions)}

proj_move_files <- function(suggestions, execute = TRUE) {

  if (execute) {
    eval(parse(text = suggestions$cmd))
  }

}

#' @rdname proj_test
#' @export
#' @section proj_analyze_pkgs:
#' Returns all of the packages loaded in R code files as well as the
#' name of the files where they were referenced.
#'
#' \code{proj_analyze_pkgs("your project directory")}

proj_analyze_pkgs <- function(path = ".") {
#  msg("Analyzing packages used in project")
  r_code <- dir_ls(path = path, type = "file", recurse = TRUE,
                       regexp = "\\.(?i)(r|rnw|rmd|rpres)$")
  pkgs <- purrr::map(r_code, req_file) %>%
    purrr::map(tibble::as.tibble) %>%
    purrr::map_dfr(dplyr::bind_rows, .id = "file") %>%
    dplyr::rename(package = value) %>%
    dplyr::group_by(package) %>%
    dplyr::summarize(N = dplyr::n(),
                     used_in = paste(file, collapse = ", ")) %>%
    dplyr::arrange(dplyr::desc(N))
  pkgs
}

#' Utility function for proj_pkg_script
#' @param pkg_name Name of package to generate install script for
#' @export
#' @keywords internal

generate_script <- function(pkg_name, vector = c()) {

  # check if package is available on CRAN
  not_on_cran <- as.logical(available::available_on_cran((pkg_name)))

  if(not_on_cran == TRUE){
    new_line <- sprintf("#Package '%s' not available on CRAN. Look on Github for the package author instead", pkg_name)
  }else{
    new_line <- sprintf("install.packages('%s')", pkg_name)
  }

  vector <- c(vector, new_line)
  vector

}

#' Generate an R script to install all of the packages
#' required to run the R/Rmd files in an R project.
#' Once generated, the script can be found in the root
#' directory of the project.
#' @param path Path to project root
#' @return An R script file ("install_proj_packages.r")
#' @export

proj_pkg_script <- function(path = ".") {

  # Delete the existing script (if it exists) so we can overwrite it
  if(file.exists(fs::path(path,"install_proj_packages.r"))){
    fs::file_delete(fs::path(path,"install_proj_packages.r"))
  }

  pkgs <- proj_analyze_pkgs(path)$package


  install_calls <- purrr::map_chr(pkgs, generate_script)

  cat("# Run this script to install the required packages for this R project.",
      install_calls,
      file=fs::path(path,"install_proj_packages.r"),
      sep="\n ",
      append=TRUE)
}




#' Render files in a project directory to update the render log file
#' @keywords internal
#' @inheritParams proj_test
#' @importFrom tibble tibble
#' @export


proj_render <- function(path = ".", ...) {

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)
  Sys.setenv("LOGGING_ON" = TRUE)

  log_clear(path)

  msg("Rendering R scripts...")


  log_push(x = "Seed @ Start", .f = .Random.seed[2], path = path)

  # find all R, Rmd, rmd files and run them?
  # this is the easyMake part
  dir <- tempdir()


  rmd <- dir_ls(path, recurse = TRUE, type = "file", regexp = "\\.(r|R)md$")
  r_script <- dir_ls(path, recurse = TRUE, type = "file", regexp = "\\.R$")



  exe <- tibble(
    path = c(rmd, r_script),
    filename = path_file(path)
  )
  exe <- withr::with_locale(c(LC_COLLATE = "C"),
                            dplyr::arrange(exe, filename))



  my_fun <- function(path) {
    if (grepl("\\.R$", path)) {
      testthat::source_file(path)
    } else {
      rmarkdown::render(path, output_dir = dir, quiet = TRUE)
    }
  }

  suppressMessages(
    purrr::map_chr(exe$path, my_fun)
  )


  log_push(x = "Seed @ End", .f = .Random.seed[2], path = path)
  # even if a file is empty, its render log will not be
  log_push(x = "LAST RENDERED", .f = "proj_render", path = path)
  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)
  Sys.setenv("LOGGING_ON" = FALSE)

}

#' @rdname proj_test
#' @inheritParams proj_test
#' @export
#' @section proj_analyze_paths:
#' Looks at paths used in R code located in a project directory and
#' reports paths that are absolute or that reference a location outside
#' the project directory.
#'
#' \code{proj_analyze_paths("your project directory")}

proj_analyze_paths <- function(path = ".") {

  if (has_rendered(path) == FALSE){
    proj_render(path)
  }

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)

  msg("Generating reproducibility report...")
  # tell you what you did wrong
  x <- log_report(path)
  # run checks on these paths
  y <- check_path(x$path, strict = FALSE)

  y %>%
    select(-path)


  return (dplyr::bind_cols(dplyr::semi_join(x, y, by = "path"), y) %>%
    dplyr::select(-timestamp))

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)
}

#' @inheritParams base::print
#' @export
#' @keywords internal

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
#' @importFrom rlang dots_list
#' @param path Directory you want to check.
#'
#' Note: For \link{proj_check_some}, which does not take a default path,
#' if you want to check your current directory, enter \code{"."} as your path.
# #' @return a \code{\link[tibble]{tibble}} of checks and their results
#' @section proj_check:
#' Runs all individual checks together and provides a report
#' of which passed, which failed, why they failed, and suggestions
#' for how to work on them.
#'
#' \code{proj_check("your project directory")}

proj_check <- function(path = ".") {


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
    "has_no_nested_proj_root",
    "has_only_used_files",
    "has_clear_build_chain",
    "has_no_absolute_paths",
    "has_only_portable_paths",
    "has_no_randomness"
  )



  needs_compile <- function(x) {
    attr(rlang::eval_tidy(rlang::sym(x)), "req_compilation")
  }

  must_compile <- checks %>%
    purrr::map_lgl(needs_compile)

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

  args <- rlang::exprs(path = path)
  out <- purrr::map_dfr(checks, rlang::exec,
                      path = path) %>%
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
      #dplyr::select(problem, solution, help) %>%
      print()
  }

  invisible(out)
}



#' Reproducbility checks
#' @rdname proj_check
#' @export
#' @import tidyselect
#' @importFrom usethis ui_todo ui_done
#' @importFrom rlang eval_tidy sym
#' @importFrom glue glue
#' @importFrom rlang dots_list
#' @inheritParams proj_check
#' @param ... One or more unquoted expressions separated by commas,
#' containing information about the checks you would like to complete.
#' These should be written as if they are being passed to dplyr's \link[dplyr]{select}.
#'
#' An example statement might be:
#'
#' \code{ends_with("root"), contains("tidy"), -has_tidy_scripts}
#'
#'
# #' @return a \code{\link[tibble]{tibble}} of checks and their results
#' @section proj_check_some:
#' Complete a specified selection of checks by harnessing
#' tidy evaluation.
#'
#' \code{proj_check_some("your project directory", contains("tidy"), ends_with("root"), -has_tidy_raw_data)}


proj_check_some <- function(path, ...) {

  #arguments <- as.list(match.call(expand.dots = FALSE))

  #print(quote(arguments$path))

  #if(is_dir(as.character(arguments$path))) {
  #  dir = as.character(arguments$path)
  #}else{
  #  dir = "."
  #}

  #print(dir)


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
    "has_no_nested_proj_root",
    "has_only_used_files",
    "has_clear_build_chain",
    "has_no_absolute_paths",
    "has_only_portable_paths",
    "has_no_randomness"
  )



  df <- data.frame(matrix(ncol = length(checks), nrow = 0))
  colnames(df) <- checks

  # if(dir == ".") {
  #    df <- df %>% dplyr::select(path, ...)
  #  }else{
  #   df <- df %>% dplyr::select(...)
  #  }

  if (missing(...) == FALSE){
    df <- df %>% dplyr::select(...)
  }

  checks <- colnames(df)


  needs_compile <- function(x) {
    attr(rlang::eval_tidy(rlang::sym(x)), "req_compilation")
  }

  must_compile <- checks %>%
    purrr::map_lgl(needs_compile)

  # Capture state information
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

  args <- rlang::exprs(path = path)
  out <- purrr::map_dfr(checks, rlang::exec,
                        path = path) %>%
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
      #dplyr::select(problem, solution, help) %>%
      print()
  }

  invisible(out)
}
