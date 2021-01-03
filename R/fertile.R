utils::globalVariables(c(
  ".", "value", "ext", "n", "timestamp", "size",
  "put_in", "cmd", "dir_rel", "path_new", "mime",
  "package", "N", "state", "problem", "help", "func",
  "solution", "filename", "desc", "modification_time", "install_call",
  "fertile", "built_in", "on_cran", "on_github", "pkg", "quoted",
  "fraction_lines_commented", "group", "file_name_full", "check_name",
  "pkgs_with_func", "fs"
))



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

  if (has_rendered(path) == FALSE) {
    proj_render(path)
  }

  pkgs <- proj_analyze_pkgs(path)
  files <- proj_analyze_files(path)
  suggestions <- proj_suggest_moves(files)
  paths <- proj_analyze_paths(path)
  x <- list(
    proj_dir = path, packages = pkgs, files = files,
    suggestions = suggestions, paths = paths
  )
  class(x) <- c("fertile", class(x))
  x
}

#' @rdname proj_test
#' @export
#' @section proj_analyze:
#' Very similar to proj_test, except that this function does NOT
#' include a report of paths that are not portable.
#'
#' \code{proj_analyze("your project directory")}


proj_analyze <- function(path = ".") {
  if (has_rendered(path) == FALSE) {
    proj_render(path)
  }

  pkgs <- proj_analyze_pkgs(path)
  files <- proj_analyze_files(path)
  suggestions <- proj_suggest_moves(files)
  x <- list(
    proj_dir = path, packages = pkgs, files = files,
    suggestions = suggestions
  )
  class(x) <- c("fertile", class(x))
  x
}

#' @rdname proj_test
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
    dplyr::mutate(
      ext = path_ext(file),
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
    dplyr::mutate(cmd = paste0(
      "file_move('", file,
      "', fs::dir_create('", path_dir(path_new), "'))"
    ))
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
  r_code <- dir_ls(
    path = path, type = "file", recurse = TRUE,
    regexp = "\\.(?i)(r|rnw|rmd|rpres)$"
  )
  pkgs <- purrr::map(r_code, req_file) %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::map_dfr(dplyr::bind_rows, .id = "file") %>%
    dplyr::rename(package = value) %>%
    dplyr::group_by(package) %>%
    dplyr::summarize(
      N = dplyr::n(),
      used_in = paste(file, collapse = ", ")
    ) %>%
    dplyr::arrange(dplyr::desc(N))
  pkgs
}

#' Generate an R script to install all of the packages
#' required to run the R/Rmd files in an R project.
#' Once generated, the script can be found in the root
#' directory of the project.
#' @param path Path to project root
#' @param script Location/name of script file destination.
#' @return Path to an R script file ("install_proj_packages.R")
#' @export

proj_pkg_script <- function(path = ".",
                            script = fs::path(path, "install_proj_packages.R")) {

  # Delete the existing script (if it exists) so we can overwrite it
  if (file.exists(script)) {
    fs::file_delete(script)
  }

  pkgs <- proj_analyze_pkgs(path)$package

  # check if package is available on CRAN
  pkg_df <- tibble::enframe(pkgs, name = NULL, value = "pkg") %>%
    mutate(
      built_in = pkgs %in% c(
        "stats", "graphics", "grDevices", "tools",
        "utils", "datasets", "methods", "base"
      ),
      on_cran = purrr::map_lgl(pkg, ~ !as.logical(available::available_on_cran(.x))),
      # on_github = purrr::map_lgl(pkg, ~ !purrr::pluck(available::available_on_github(.x), "available")),
      msg = ifelse(
        on_cran,
        paste0("install.packages('", pkg, "')"),
        paste("# remotes::install_github('<repo>/", pkg, "') -- you need to find the value of <repo>")
      )
    ) %>%
    filter(!built_in)

  cat(
    "# Run this script to install the required packages for this R project.",
    "# Packages hosted on CRAN...",
    pkg_df %>%
      filter(on_cran) %>%
      mutate(quoted = paste0("'", pkg, "'")) %>%
      dplyr::pull(quoted) %>%
      paste(collapse = ", ") %>%
      paste("install.packages(c(", ., "))"),
    file = script,
    sep = "\n",
    append = TRUE
  )

  cat(
    "# Packages (likely) hosted on GitHub...",
    pkg_df %>%
      filter(!on_cran) %>%
      dplyr::pull(msg),
    file = script,
    sep = "\n",
    append = TRUE
  )

  return(fs::as_fs_path(script))
}

#' Return a sessionInfo() style report of the packages/software versions
#' that your R project was last run successfully on.
#' @export
#' @param path path to the project directory


proj_dependency_report <- function(path = proj_root()) {
  message(paste("Reading from", path_abs(path(path, ".software-versions.txt"))))

  if (has_rendered(path) == FALSE) {
    proj_render(path)
  }

  file.show(path_abs(path(path, ".software-versions.txt")))
}




#' Render files in a project directory to update the render log file
#' @keywords internal
#' @inheritParams proj_test
#' @importFrom tibble tibble
#' @importFrom callr r
#' @importFrom utils capture.output sessionInfo
#' @export


proj_render <- function(path = ".", ...) {
  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)
  Sys.setenv("LOGGING_ON" = TRUE)

  log_clear(path)

  msg("Rendering R scripts...")


  seed <- get0(".Random.seed", envir = .GlobalEnv, ifnotfound = NULL)
  if (is.null(seed)) {
    # Force a random seed to exist if there isn't one
    x <- stats::rnorm(1, 0, 1)
  }
  log_push(x = "Seed @ Start", .f = .Random.seed[2], path = path)


  # find all R, Rmd, rmd files and run them?
  # this is the easyMake part
  dir <- tempdir()


  rmd <- dir_ls(path, recurse = TRUE, type = "file", regexp = "\\.(r|R)md$")
  r_script <- dir_ls(path, recurse = TRUE, type = "file", regexp = "\\.R$")

  fertile_file <- dir_ls(path, recurse = TRUE, type = "file", regexp = "\\install_proj_packages.R")

  true_r_scripts <- setdiff(r_script, fertile_file)

  exe <- tibble(
    path = c(rmd, true_r_scripts),
    filename = path_file(path)
  )
  exe <- withr::with_locale(
    c(LC_COLLATE = "C"),
    dplyr::arrange(exe, filename)
  )


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

  shimmed_paths <- suppressMessages(render_log_report(path)$path)
  loaded_pkgs <- grep("package:", shimmed_paths, value = TRUE)
  loaded_pkgs <- substr(loaded_pkgs, 9, nchar(loaded_pkgs))

  # see if we already have a sessionInfo() file & delete if so
  session_file <- fs::path(path, ".software-versions.txt")

  if (fs::file_exists(session_file)) {
    file_delete(session_file)
  }

  # load packages and generate session info based only on r/rmd files
  r(function(x, y) fertile::to_execute(x, y), args = list(loaded_pkgs, path))

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
  if (has_rendered(path) == FALSE) {
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

  y_2 <- y %>%
    select(-path)


  return(dplyr::bind_cols(dplyr::semi_join(x, y, by = "path"), y_2) %>%
    dplyr::select(-timestamp))

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)
}

#' @inheritParams base::print
#' @export
#' @keywords internal

print.fertile <- function(x, ...) {
  msg(paste(
    "Analysis of reproducibility for",
    path_file(path_abs(x$proj_dir))
  ))
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
  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)


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
    "has_no_randomness",
    "has_well_commented_code"
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
    path = path
  ) %>%
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
      # dplyr::select(problem, solution, help) %>%
      print()
  }

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)

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

  # arguments <- as.list(match.call(expand.dots = FALSE))

  # print(quote(arguments$path))

  # if(is_dir(as.character(arguments$path))) {
  #  dir = as.character(arguments$path)
  # }else{
  #  dir = "."
  # }

  # print(dir)

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)


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
    "has_no_randomness",
    "has_well_commented_code"
  )



  df <- data.frame(matrix(ncol = length(checks), nrow = 0))
  colnames(df) <- checks

  # if(dir == ".") {
  #    df <- df %>% dplyr::select(path, ...)
  #  }else{
  #   df <- df %>% dplyr::select(...)
  #  }

  if (missing(...) == FALSE) {
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
    path = path
  ) %>%
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
      # dplyr::select(problem, solution, help) %>%
      print()
  }

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)

  invisible(out)
}


#' Get reproducibility badges for a project
#' @param path Path to project root
#' @param cleanup Delete intermediate files?
#' @return Path to an html output file summarizing the badges received/failed
#' @export

proj_badges <- function(path = ".", cleanup = TRUE) {
  if (fs::file_exists(fs::path(path, "fertile-badges.html"))) {
    fs::file_delete(fs::path(path, "fertile-badges.html"))
  }


  graphics_include <- c()
  graphics_failed <- c()

  # Record which checks passed/failed

  check_report <- proj_check(path)

  # Assign badges based on check status

  # Badge 1: Project Structure

  if (check_report$state[9] == TRUE &
    check_report$state[10] == TRUE) {
    graphics_include <- graphics_include %>%
      append("structure")
  } else {
    graphics_failed <- graphics_failed %>%
      append("structure")
  }

  # Badge 2: Tidy Files

  if (check_report$state[1] == TRUE &
    check_report$state[2] == TRUE &
    check_report$state[3] == TRUE &
    check_report$state[4] == TRUE &
    check_report$state[5] == TRUE &
    check_report$state[6] == TRUE &
    check_report$state[11] == TRUE) {
    graphics_include <- graphics_include %>%
      append("tidy")
  } else {
    graphics_failed <- graphics_failed %>%
      append("tidy")
  }

  # Badge 3: Documentation

  if (check_report$state[7] == TRUE &
    check_report$state[12] == TRUE &
    check_report$state[16] == TRUE) {
    graphics_include <- graphics_include %>%
      append("documentation")
  } else {
    graphics_failed <- graphics_failed %>%
      append("documentation")
  }

  # Badge 4: File Paths

  if (check_report$state[13] == TRUE &
    check_report$state[14] == TRUE) {
    graphics_include <- graphics_include %>%
      append("paths")
  } else {
    graphics_failed <- graphics_failed %>%
      append("paths")
  }

  # Badge 5: Randomness

  if (check_report$state[15] == TRUE) {
    graphics_include <- graphics_include %>%
      append("randomness")
  } else {
    graphics_failed <- graphics_failed %>%
      append("randomness")
  }

  # Badge 6: Code Style

  if (check_report$state[8] == TRUE) {
    graphics_include <- graphics_include %>%
      append("style")
  } else {
    graphics_failed <- graphics_failed %>%
      append("style")
  }

  # Build a tibble of check names, whether they passed, & associated badges


  check_names <- c(
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
    "has_no_randomness",
    "has_well_commented_code"
  )

  badge_groups <- c(
    "Tidy Files",
    "Tidy Files",
    "Tidy Files",
    "Tidy Files",
    "Tidy Files",
    "Tidy Files",
    "Documentation",
    "Code Style",
    "Project Structure",
    "Project Structure",
    "Tidy Files",
    "Documentation",
    "File Paths",
    "File Paths",
    "Randomness",
    "Documentation"
  )

  checks_tbl <- tibble(check_name = check_names, group = badge_groups, state = check_report$state)


  # Get failed checks
  failed_checks <- checks_tbl %>%
    filter(state == FALSE) %>%
    arrange(desc(group))

  badge_list <- c(
    "Project Structure",
    "Tidy Files",
    "Documentation",
    "File Paths",
    "Randomness",
    "Code Style"
  )

  structure_checks <- failed_checks %>%
    filter(group == "Project Structure") %>%
    select(check_name)

  tidy_checks <- failed_checks %>%
    filter(group == "Tidy Files") %>%
    select(check_name)

  documentation_checks <- failed_checks %>%
    filter(group == "Documentation") %>%
    select(check_name)

  paths_checks <- failed_checks %>%
    filter(group == "File Paths") %>%
    select(check_name)

  randomness_checks <- failed_checks %>%
    filter(group == "Randomness") %>%
    select(check_name)

  style_checks <- failed_checks %>%
    filter(group == "Code Style") %>%
    select(check_name)


  # User

  fullname <- tryCatch(
    {
      fullname <- whoami::fullname()
    },
    error = function(e) {
      return("N/A")
    }
  )

  username <- tryCatch(
    {
      username <- whoami::username()
    },
    error = function(e) {
      return("N/A")
    }
  )

  email <- tryCatch(
    {
      email <- whoami::email_address()
    },
    error = function(e) {
      return("N/A")
    }
  )

  github_username <- tryCatch(
    {
      github_username <- whoami::gh_username()
    },
    error = function(e) {
      return("N/A")
    }
  )


  name_proj <- fs::path_file(proj_root(path))

  # Get last edited history for files in the project folder

  file_names_full <- as.vector(fs::dir_ls(path))
  file_names_short <- fs::path_file(file_names_full)

  files_updated <- tibble(file_name_full = file_names_full, file_name = file_names_short)

  file_history <- files_updated %>%
    mutate(last_edited = fs::file_info(file_name_full)$modification_time) %>%
    select(-file_name_full)


  # Delete existing rmd/html files in tempdir

  temp_rmd <- fs::path(tempdir(), "fertile-badges.Rmd")
  temp_html <- fs::path(tempdir(), "fertile-badges.html")

  if (fs::file_exists(temp_rmd)) {
    fs::file_delete(temp_rmd)
  }

  if (fs::file_exists(temp_html)) {
    fs::file_delete(temp_html)
  }

  # Copy parameterized Rmd to tempdir() --- necessary for opening in Viewer

  fs::file_copy(system.file("fertile-badges.Rmd", package = "fertile"), tempdir())
  badge_rmd <- fs::path(tempdir(), "fertile-badges.Rmd")

  # Define params for Rmarkdown file and render to HTML

  rmarkdown::render(badge_rmd,
    params = list(
      project_name = name_proj,
      awarded = graphics_include,
      failed = graphics_failed,
      failures_structure = nrow(structure_checks) > 0,
      checks_structure = structure_checks,
      failures_tidy = nrow(tidy_checks) > 0,
      checks_tidy = tidy_checks,
      failures_documentation = nrow(documentation_checks) > 0,
      checks_documentation = documentation_checks,
      failures_paths = nrow(paths_checks) > 0,
      checks_paths = paths_checks,
      failures_randomness = nrow(randomness_checks) > 0,
      checks_randomness = randomness_checks,
      failures_style = nrow(style_checks) > 0,
      checks_style = style_checks,
      fullname = fullname,
      username = username,
      email = email,
      github_username = github_username,
      file_history = file_history
    )
  )


  # Copy the HTML into the user's project directory for easy use

  fs::file_copy(fs::path(tempdir(), "fertile-badges.html"), path) # Open HTML in Viewer pane

  viewer <- getOption("viewer")
  viewer(fs::path(tempdir(), "fertile-badges.html"))


  # Return the path to the HTML

  return(fs::path(path, "fertile-badges.html"))
}




#' Add a function to the list of functions that get checked for file path issues.
#' @param func name of function you want to create a shim for (e.g. "\code{read_excel}")
#' @param package name of package that provided function is from (e.g. "\code{readxl}")
#' @param path_arg name of path-related argument in that function (if not
#' specified, fertile will make an educated guess).
#' @export

add_shim <- function(func, package = "", path_arg = "") {

  # Get code to write to file
  func_lines <- get_shim_code(func, package, path_arg)

  # Write code to .Rprofile
  shims <- path_shims()

  cat("", file = shims, sep = "\n", append = TRUE)

  for (line in func_lines) {
    cat(line, file = shims, sep = "\n", append = TRUE)
  }

  cat(paste0("attr(", func, ", 'package') <- 'fertile'"), file = shims, sep = '\n', append = TRUE)
  cat(paste0("attr(", func, ", 'func_name') <- '", func, "'"), file = shims, sep = '\n', append = TRUE)

  # Execute file to make sure new shim is in environment
  base::source(shims)

  msg("Shim created")
}

#' View/edit list of user created shims.
#' @rdname add_shim
#' @export

edit_shims <- function() {
  shims <- path_shims()

  msg("Viewing list of user-added shims")

  # Open Rprofile in editing window
  utils::file.edit(shims)
}

#' Remove all user-added shims from the global environment
#' @rdname add_shim
#' @export

unload_shims <- function() {
  # Remove them from the global environment
  rm(list = active_shims(), envir = .GlobalEnv)
}

#' @rdname add_shim
#' @export

load_shims <- function() {
  shims <- path_shims()
  base::source(shims)
}


#' Write shims for all possible shimmable functions
#' @rdname add_shim
#' @export

add_all_possible_shims <- function() {

  # In Progress

  # Get list of possible shims
  possible_shims <- find_all_shimmable_functions()

  possible_shims_named <- unlist(possible_shims)
  possible_shims_unnamed <- unlist(possible_shims, use.names = FALSE)


  pkgs <- names(possible_shims)
  numbered_shims <- names(possible_shims_named)

  # for each package in our list, pull out the functions and combine them with ::
  shims_with_pkgs <- c()

  for (pkg in pkgs) {
    func_indices <- grep(pkg, numbered_shims)
    shim_names <- paste0(pkg, "::", possible_shims_unnamed[func_indices])
    shims_with_pkgs <- shims_with_pkgs %>% append(shim_names)
  }

  # List of fertile shims
  fertile_shims <- c(
    "utils::read.csv",
    "utils::read.csv2",
    "utils::read.delim",
    "utils::read.delim2",
    "utils::read.DIF",
    "utils::read.fortran",
    "utils::read.fwf",
    "utils::read.table",
    "utils::write.csv",
    "readr::read_csv",
    "readr::read_csv2",
    "readr::read_delim",
    "readr::read_file",
    "readr::read_file_raw",
    "readr::read_fwf",
    "readr::read_lines",
    "readr::read_lines_raw",
    "readr::read_log",
    "readr::read_table",
    "readr::read_table2",
    "readr::read_tsv",
    "readr::write_csv",
    "base::read.dcf",
    "base::load",
    "base::source",
    "base::save",
    "readxl::read_excel",
    "stats::read.ftable",
    "rjson::fromJSON",
    "foreign::read.dta",
    "foreign::read.mtp",
    "foreign::read.spss",
    "foreign::read.systat",
    "sas7bdat::read.sas7bdat",
    "ggplot2::ggsave"
  )

  # List of functions already shimmed by the user
  user_shims <- read_shims()

  # Only take ones that aren't shimmed by fertile or the user
  combined_fertile_shims <- unique(c(fertile_shims, user_shims))

  unwritten_shims <- shims_with_pkgs[!(shims_with_pkgs %in% combined_fertile_shims)]

  for (shim in unwritten_shims) {
    index_funcname <- (gregexpr("::", shim)[[1]][1] + 2)
    func <- substr(shim, index_funcname, nchar(shim))
    pkg <- substr(shim, 0, index_funcname - 3)
    add_shim(func, pkg)
  }
}
