#' Create a fertile check object
#' @keywords internal
#' @param fun Function to run that returns a logical
#' @param name What is the check checking for?
#' @param req_compilation Does the code have to be compiled for the check to
#' work?
#' @param state Logical indicating whether the check passed/failed
#' @param problem Description of the problem
#' @param solution Description of a potential solution
#' @param help References to help files
#' @param errors A \code{\link[tibble]{tibble}} of error messages
#' @seealso \code{\link[usethis]{use_tidy_style}},
#'          \code{\link[fs]{is_absolute_path}}
#' @export

make_check <- function(fun, name, req_compilation,
                       state, problem, solution, help,
                       errors, ...) {
  x <- tibble(
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


#' @inheritParams base::print
#' @export
#' @keywords internal

print.fertile_check <- function(x, ...) {
  x %>%
    split(.$name) %>%
    purrr::walk(print_one_check)
}


#' Print a check function output
#' @importFrom usethis ui_todo ui_done ui_line ui_code_block
#' @export
#' @keywords internal

print_one_check <- function(x, ...) {
  if (x$state) {
    ui_done(x$name)
  } else {
    ui_todo(x$name)
    ui_code_block(" Problem: {x$problem}", copy = FALSE)
    ui_code_block(" Solution: {x$solution}", copy = FALSE)
    ui_code_block(" See for help: {x$help}", copy = FALSE)
    print(purrr::pluck(x$error, 1))
  }
}


#' @rdname proj_check
#' @inheritParams proj_check
#' @importFrom mime guess_type
#' @export
#' @section has_tidy_media:
#' Checks to make sure no audio/video files are found at the
#' root of your project.
#'
#' \code{has_tidy_media("your project directory")}

has_tidy_media <- function(path = ".") {
  check_is_dir(path)
  paths <- dir_ls(path)

  bad <- paths %>%
    mime::guess_type() %>%
    grepl("(audio|video)/", .)

  errors <- tibble(
    culprit = paths[bad],
    expr = glue("fs::file_move('{culprit}', here::here('media/'))")
  )

  make_check(
    name = "Checking for no A/V files at root level",
    state = !any(bad),
    problem = "A/V files in root directory clutter project",
    solution = "Move A/V files to media/ directory",
    help = "?fs::file_move",
    errors = errors
  )
}
attr(has_tidy_media, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @importFrom mime guess_type
#' @export
#' @section has_tidy_images:
#' Checks to make sure no image files are found at the
#' root of your project.
#'
#' \code{has_tidy_images("your project directory")}

has_tidy_images <- function(path = ".") {
  check_is_dir(path)
  paths <- dir_ls(path)

  bad <- paths %>%
    mime::guess_type() %>%
    grepl("image/", .)

  errors <- tibble(
    culprit = paths[bad],
    expr = glue("fs::file_move('{culprit}', here::here('img/'))")
  )

  make_check(
    name = "Checking for no image files at root level",
    state = !any(bad),
    problem = "Image files in root directory clutter project",
    solution = "Move source files to img/ directory",
    help = "?fs::file_move",
    errors = errors
  )
}
attr(has_tidy_images, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_tidy_code:
#' Checks to make sure no source files are found at the
#' root of your project.
#'
#' \code{has_tidy_code("your project directory")}

has_tidy_code <- function(path = ".") {
  check_is_dir(path)
  paths <- dir_ls(path)

  bad <- paths %>%
    mime::guess_type() %>%
    grepl("(csrc|c\\+\\+|py|ruby|perl|scala|javascript|java|sql)", .)

  errors <- tibble(
    culprit = paths[bad],
    expr = glue("fs::file_move('{culprit}', here::here('src/'))")
  )

  make_check(
    name = "Checking for no source files at root level",
    state = !any(bad),
    problem = "Code source files in root directory clutter project",
    solution = "Move source files to src/ directory",
    help = "?fs::file_move",
    errors = errors
  )
}
attr(has_tidy_code, "req_compilation") <- FALSE


#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_tidy_raw_data:
#' Checks to make sure no raw data files are found at the
#' root of your project.
#'
#' \code{has_tidy_raw_data("your project directory")}

has_tidy_raw_data <- function(path = ".") {
  check_is_dir(path)
  bad <- path %>%
    dir_info() %>%
    dplyr::mutate(ext = path_ext(path)) %>%
    dplyr::filter(tolower(ext) %in% c("dat", "csv", "tsv", "xml", "json", "zip") |
                 (tolower(ext) == "txt" & size > "10K")) %>%
    dplyr::pull(path)

  errors <- tibble(
    culprit = bad,
    expr = glue("fs::file_move('{culprit}', here::here('data-raw/'))")
  )

  make_check(
    name = "Checking for no raw data files at root level",
    state = length(bad) == 0,
    problem = "Raw data files in root directory clutter project",
    solution = "Move raw data files to data-raw/ directory",
    help = "?fs::file_move",
    errors = errors
  )
}
attr(has_tidy_raw_data, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_tidy_data:
#' Checks to make sure no .rda files are found at the
#' root of your project.
#'
#' \code{has_tidy_data("your project directory")}

has_tidy_data <- function(path = ".") {
  check_is_dir(path)
  bad <- dir_ls(path, regexp = "\\.(rda|rdata)$", ignore.case = TRUE)

  errors <- tibble(
    culprit = bad,
    expr = glue("fs::file_move('{culprit}', here::here('data/'))")
  )

  make_check(
    name = "Checking for no *.rda files at root level",
    state = length(bad) == 0,
    problem = "R data files in root directory clutter project",
    solution = "Move *.rda files to data/ directory",
    help = "?fs::file_move",
    errors = errors
  )
}
attr(has_tidy_data, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_tidy_scripts:
#' Checks to make sure no .R script files are found at the
#' root of your project.
#'
#' \code{has_tidy_scripts("your project directory")}

has_tidy_scripts <- function(path = ".") {
  check_is_dir(path)
  bad <- dir_ls(path, regexp = "\\.R$", ignore.case = TRUE)

  errors <- tibble(
    culprit = bad,
    expr = glue("fs::file_move('{culprit}', here::here('R/'))")
  )

  make_check(
    name = "Checking for no *.R scripts at root level",
    state = length(bad) == 0,
    problem = "R script files in root directory clutter project",
    solution = "Move *.R files to R/ directory",
    help = "?fs::file_move",
    errors = errors
  )
}
attr(has_tidy_scripts, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_readme:
#' Checks to make sure a README file is found at the
#' root of your project.
#'
#' \code{has_readme("your project directory")}

has_readme <- function(path = ".") {
    check_is_dir(path)
    errors <- tibble(
    culprit = "README.md",
    expr = glue("fs::file_create('{culprit}')")
  )

  make_check(
    name = "Checking for README file(s) at root level",
    state = length(dir_ls(path, regexp = "README", ignore.case = TRUE)) > 0,
    problem = "No README found in project directory",
    solution = "Create README",
    help = "?fs::file_create",
    errors = errors
  )
}
attr(has_readme, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_proj_root:
#' Checks to make sure a single .Rproj file is found
#' at the root of your project.
#'
#' \code{has_proj_root("your project directory")}
has_proj_root <- function(path = ".") {
  check_is_dir(path)


  root <- dir_ls(path, regexp = "\\.Rproj$", ignore.case = TRUE, all = TRUE)

  errors <- tibble(
    culprit = "*.Rproj",
    expr = "usethis::create_project"
  )


  make_check(
    name = "Checking for single .Rproj file at root level",
    state = length(root) == 1,
    problem = "No .Rproj file found",
    solution = "Create RStudio project",
    help = "?usethis::create_project",
    errors = errors
  )
}
attr(has_proj_root, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_no_nested_proj_root:
#' Checks to make sure there are no nested .Rproj
#' files in your project.
#'
#' \code{has_no_nested_proj_root("your project directory")}
has_no_nested_proj_root <- function(path = ".") {
  check_is_dir(path)

  root_projs <- dir_ls(path, regexp = "\\.Rproj$", ignore.case = TRUE)
  all_projs <- dir_ls(path, regexp = "\\.Rproj$",
                      recurse = TRUE, ignore.case = TRUE)


  bad <- setdiff(all_projs, root_projs)

  errors <- tibble(
    culprit = as_fs_path(bad),
    expr = "?"
  )

  make_check(
    name = "Checking for nested .Rproj files within project",
    state = length(bad) == 0,
    problem = "Nested .Rproj file(s) found",
    solution = "Create unnested directories for each project",
    help = "?usethis::create_project",
    errors = errors
  )
}
attr(has_no_nested_proj_root, "req_compilation") <- FALSE

#' @rdname proj_check
#' @inheritParams proj_check
#' @importFrom utils tail
#' @export
#' @section has_well_commented_code:
#' Checks to make sure that all code files are at least 10 percent comments
#' \code{has_well_commented_code("your project directory")}
has_well_commented_code <- function(path = ".") {

  check_is_dir(path)

  # Get code files
  rmd <- as.character(dir_ls(path, recurse = TRUE, type = "file", regexp = "\\.(r|R)md$"))
  r_script <- as.character(dir_ls(path, recurse = TRUE, type = "file", regexp = "\\.R$"))
  fertile_file <- as.character(dir_ls(path, recurse = TRUE, type = "file", regexp = "\\install_proj_packages.R"))
  true_r_scripts <- setdiff(r_script, fertile_file)


  # For each file, count the percentage of characters that are code

  r_comments <- tibble(file_name = character(), fraction_lines_commented = numeric())

  comment_ratio_r <- function(file) {

    # Convert .R file to character vector and delete empty lines
    converted_code <- readr::read_lines(file)
    converted_code <- converted_code[converted_code != ""]

    # Extract the lines that are comments
    comments_only <- converted_code[grepl("^#", converted_code)]

    # Calculate percentage of lines in file that are comments
    ratio <- length(comments_only)/length(converted_code)

    r_comments %>%
      tibble::add_row(file_name = file, fraction_lines_commented = round(ratio,2))
    }



  rmd_comments <- tibble(file_name = character(), fraction_lines_commented = numeric())

  comment_ratio_rmd <- function(file) {

    # Convert .Rmd file to character vector
    converted_rmd <- readr::read_lines(file)
    converted_rmd <- converted_rmd[converted_rmd != ""]

    # Cut out the YAML header
    end_YAML <- grep("---", converted_rmd)[2]
    converted_rmd <- tail(converted_rmd, length(converted_rmd) - end_YAML)

    # Cut out the code chunk markers to leave just code + comments
    code_plus_comments <- converted_rmd[!grepl("^```", converted_rmd)]

    # Get a separate file with JUST the comments (no code)
    chunk_markers <- grep("```", converted_rmd)
    comments_only <- converted_rmd

    for (i in 1:(length(chunk_markers)/2)){
      first_chunk <- grep("```", comments_only)[1:2]
      comments_only <- comments_only[-(first_chunk[1]:first_chunk[2])]
      i = i+1
    }

    # Compare the ratio of comments to total lines
    ratio <- length(comments_only)/length(code_plus_comments)

    rmd_comments %>%
      tibble::add_row(file_name = file, fraction_lines_commented = round(ratio,2))
  }

  r_comments_tbl <- true_r_scripts %>%
    purrr::map(comment_ratio_r)

  rmd_comments_tbl <- rmd %>%
    purrr::map(comment_ratio_rmd)

  all_file_ratios <- dplyr::bind_rows(r_comments_tbl,rmd_comments_tbl)


  # Any files w/ <10% commented code will be flagged

  bad <- all_file_ratios %>% filter(fraction_lines_commented < 0.10)


  make_check(
    name = "Checking that code is adequately commented",
    state = length(bad) == 0,
    problem = "Suboptimally commented .R or .Rmd files found",
    solution = "Add more comments to the files below. At least 10% of the lines should be comments.",
    help = "https://intelligea.wordpress.com/2013/06/30/inline-and-block-comments-in-r/",
    errors = bad
  )
  }

attr(has_well_commented_code, "req_compilation") <- FALSE


#' @rdname proj_check
#' @inheritParams proj_check
#' @importFrom dplyr anti_join semi_join mutate
#' @importFrom tools file_ext file_path_sans_ext
#' @export
#' @section has_only_used_files:
#' Checks to make sure that all the files located
#' in your project directory are being used by/in
#' code from that directory.
#'
#' \code{has_only_used_files("your project directory")}

has_only_used_files <- function(path = "."){

  check_is_dir(path)


  if (!has_rendered(path)) {
    proj_render(path)
  }

  all_files_list <- c(as_fs_path(dir_ls(proj_root(path), recurse = TRUE)))
  all_files <- tibble(path_abs = all_files_list)


  # Find all possible output files that have filenames matching Rmd files

  rmd <- all_files %>%
    filter(file_ext(path_abs) %in% c("rmd", "Rmd")) %>%
    mutate(no_ext = file_path_sans_ext(path_abs))

  possible_rmd_outputs <- all_files %>%
    filter(file_ext(path_abs) %in% c("html", "pdf", "docx")) %>%
    mutate(no_ext = file_path_sans_ext(path_abs))

  matching <- semi_join(possible_rmd_outputs, rmd, by = "no_ext") %>%
    select(path_abs)

  # Find all R files

  r_files <- tibble(path_abs = character())

  add_if_r <- function(file) {
    if (is_r_file(file) == TRUE){
    r_files %>%
      tibble::add_row(path_abs = file)
    }
  }

  r_files <- all_files$path_abs %>%
    purrr::map_dfr(add_if_r)

  # Find files generated by fertile

  fertile_files <- all_files %>%
    filter(grepl("software-versions", path_abs) | grepl("install_proj_packages", path_abs))


  # Remove r files and r output files from consideration

  ignore <- rbind(r_files, matching, fertile_files)


  # Get list of paths used in code

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)

  paths_used <- log_report(path) %>%
    select (path_abs) %>%
    filter(!is.na(path_abs))

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)

  if (nrow(paths_used) == 0){

    # If we use no paths in our code files, just check to
    # see whether we're ignoring all files (basically, whether
    # there are unused output files)

    bad <- rbind(anti_join(all_files, ignore, by = "path_abs"),
                anti_join(ignore, all_files, by = "path_abs"))

  } else {

    # If we used paths in our code, check to see that those
    # paths are linked to files.

    paths_used <- paths_used %>%
      mutate(path_abs = as.character(path_abs))


    paths_to_test <- anti_join(all_files, ignore, by = "path_abs")


    bad <- rbind(
    anti_join(paths_used, paths_to_test, by = "path_abs"),
    anti_join(paths_to_test, paths_used, by = "path_abs"))


  }

  bad_in_dir <- semi_join(all_files, bad)


  make_check(
    name = "Checking to see if all files in directory are used in code",
    state = nrow(bad_in_dir) == 0,
    problem = "You have files in your project directory which are not being used.",
    solution = "Use or delete files.",
    help = "?fs::file_delete",
    errors = bad_in_dir
  )


}
attr(has_only_used_files, "req_compilation") <- TRUE




#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_no_absolute_paths:
#' Checks to make sure paths referenced in your
#' project code are all written as relative, rather
#' than absolute.
#'
#' \code{has_no_absolute_paths("your project directory")}
has_no_absolute_paths <- function(path = ".") {

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)
  check_is_dir(path)

  if (!has_rendered(path)) {
    proj_render(path)
  }


  # has to work if file is empty
  paths <- log_report(path) %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path)

  bad <- paths %>%
    fs::is_absolute_path()

  errors <- tibble(
    culprit = as_fs_path(paths[bad]),
    expr = glue::glue("fs::file_move('{culprit}', here::here()); fs::path_rel('{culprit}')")
  )

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)

  make_check(
    name = "Checking for no absolute paths",
    state = !any(bad),
    problem = "Absolute paths are likely non-portable",
    solution = "Use relative paths. Move files if necessary.",
    help = "?fs::file_move; ?fs::path_rel",
    errors = errors
  )


}
attr(has_no_absolute_paths, "req_compilation") <- TRUE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_only_portable_paths:
#' Checks to make sure all paths referenced
#' in your project code are located within the
#' project directory and are written as reltive,
#' rather than absolute.
#'
#' \code{has_only_portable_paths("your project directory")}

has_only_portable_paths <- function(path = ".") {

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)

  check_is_dir(path)

  if (!has_rendered(path)) {
    proj_render(path)
  }

  paths <- log_report(path) %>%
    dplyr::filter(!grepl("package:", path)) %>%
    dplyr::pull(path)

  good <- paths %>%
    is_path_portable()

  errors <- tibble(
    culprit = as_fs_path(paths[!good]),
    expr = glue("fs::path_rel('{culprit}')")
  )

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)

  make_check(
    name = "Checking for only portable paths",
    state = all(good),
    problem = "Non-portable paths won't necessarily work for others",
    solution = "Use relative paths.",
    help = "?fs::path_rel",
    errors = errors
  )

}
attr(has_only_portable_paths, "req_compilation") <- TRUE

#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_no_randomness:
#' Checks to make sure that code in your project does
#' not use randomness. Your project will pass this check
#' if randomness is used but a seed is also set.
#'
#' \code{has_no_randomness("your project directory")}
has_no_randomness <- function(path = ".") {

  check_is_dir(path)

  if (!has_rendered(path)) {
    proj_render(path)
  }

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)

  log <- log_report(path)

  seeds <- log %>%
    filter(func == "base::set.seed")

  seed_old <- log %>%
    filter(path == "Seed @ Start") %>%
    select(func)

  seed_new <- log %>%
    filter(path == "Seed @ End") %>%
    select(func)

  read_csv_calls <- log %>%
    filter(func == "readr::read_csv")

  read_csv_caused_problem <- FALSE
  RNG_found <- FALSE


  # See if read_csv caused an issue (since it now changes random number generation)
  all_files_list <- c(fs::as_fs_path(fs::dir_ls(proj_root(path), recurse = TRUE)))
  all_files <- tibble(path_abs = all_files_list)

  r_files <- all_files %>%
    filter(tools::file_ext(path_abs) %in% c("rmd", "Rmd", "R", "r"))

  code_lines <- c()
  for (file in r_files$path_abs){
    lines <- readr::read_lines(file)
    code_lines <- code_lines %>% append(lines)
  }

  output <- c()
  #Random numbers generate elsewhere?
  rng_functions <- c('runif\\(', 'rbinom\\(', 'rnorm\\(', 'sample\\(', 'sample_frac\\(', 'sample_n\\(', 'slice_sample\\(',
                     'rgamma\\(', 'rpois\\(', 'rexp\\(', 'rmvnorm\\(', 'rnbinom\\(', 'rbeta\\(', 'rchisq\\(', 'rlogis\\(',
                     'rstab\\(', 'rt\\(', 'rgeom\\(', 'rhyper\\(', 'rwilcox\\(', 'rweibull\\(')

  for (func in rng_functions){
    found <- grep(func, code_lines)
    output <- output %>% append(found)
  }

  if (length(output) > 0){
    RNG_found <- TRUE
  }

  if (nrow(read_csv_calls)>0 & RNG_found == FALSE & !identical(seed_old,seed_new)){
    read_csv_caused_problem <- TRUE
  }


  # If seeds are the same, not flagged
  if (identical(seed_old, seed_new)) {
    result = TRUE
  }
  # If seeds have been set, not flagged
  else if (nrow(seeds) > 0){
    result = TRUE
  }
  # If there was randomness BUT it was caused by read_csv, not flagged
  else if (read_csv_caused_problem == TRUE) {
    result = TRUE

  # Otherwise, flagged (AKA randomness NOT caused by read_csv and where a seed isn't set)
  } else {
    result = FALSE
  }


  errors <- tibble(
    culprit = "?",
    expr = glue("Example: set.seed(1)")
  )

  make_check(
    name = "Checking for no randomness",
    state = result,
    problem = "Your code uses randomness",
    solution = "Set a seed using `set.seed()` to ensure reproducibility.",
    help = "?set.seed",
    errors = errors
  )


}
attr(has_no_randomness, "req_compilation") <- TRUE


#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#'
#' @section has_no_lint:
#' Checks whether your code conforms to tidyverse style.
#'
#' \code{has_no_lint("your project directory")}
has_no_lint <- function(path = ".") {
  check_is_dir(path)
  files <- fs::dir_ls(path, recurse = TRUE, regexp = "\\.[rR]{1}(md)?$")
  x <- files %>%
    purrr::map(lintr::lint) %>%
    flatten_lints()
  print(x)
  make_check(
    name = "Checking code style for lint",
    state = length(x) == 0,
    problem = "Your code does not conform to tidyverse style",
    solution = "Fix code accordinng to Markers. Use usethis::use_tidy_style() to change automatically",
    help = "?usethis::use_tidy_style",
    errors = tibble(culprit = "See 'Markers' tab in Console window to find which code was flagged")
  )
}
attr(has_no_lint, "req_compilation") <- FALSE


#' @rdname proj_check
#' @inheritParams proj_check
#' @export
#' @section has_clear_build_chain:
#' Checks for a clear order in which to run your
#' R scripts.
#'
#' \code{has_clear_build_chain("your project directory")}
has_clear_build_chain <- function(path = ".") {
  check_is_dir(path)
  has_makefile <- length(fs::dir_ls(path, regexp = "^makefile$")) > 0
  has_drakefile <- length(fs::dir_ls(path, regexp = "^\\.drake$")) > 0

  files <- fs::dir_ls(path, recurse = TRUE, regexp = "\\.[rR]{1}(md)?$")
  suppressWarnings(
    files_numbered <- files %>%
      path_file() %>%
      readr::parse_number()
  )

  bad <- files[is.na(files_numbered)]

  errors <- tibble(
    culprit = bad,
    expr = ""
  )

  make_check(
    name = "Checking for clear build chain",
    state = length(files) == 1 || has_makefile || has_drakefile || length(bad) == 0,
    problem = "It is not obvious in what order to run your R scripts",
    solution = "Use a formal build chain system or prefix your files with numbers",
    help = "?drake::drake",
    errors = errors
  )
}
attr(has_clear_build_chain, "req_compilation") <- FALSE
