# stolen from tidyverse
# https://github.com/tidyverse/tidyverse/blob/a720dcd73d9e3fc0ec86317bc0abaf8f0077e8bd/R/utils.R

#' @importFrom crayon bold
#' @importFrom cli rule

msg <- function(text) {
  cli::rule(
    left = crayon::bold(text),
    right = paste0("fertile ", package_version("fertile"))
  ) %>%
    text_col() %>%
    message()
}

#' Check whether a provided path is a directory
#' @param path Path you are wanting to check
#' @importFrom rlang abort
#' @export
#' @family path type checks

check_is_dir <- function(path) {

  if (fs::is_dir(path)) {
    return(path)
  }

  rlang::abort(message = "The path you provided is NOT to a directory.
  Please provide a path to a directory instead.")
}


#' Check whether a provided path is a file
#' @param path Path you are wanting to check
#' @importFrom rlang abort
#' @export
#' @family path type checks

check_is_file <- function(path){
  if (fs::is_file(path)){
    return (path)
  }

  rlang::abort(message = "The path you provided is NOT to a file.
               Please provide a path to a file instead.")

}


#' Utility function to check whether a project has been updated since last rendered
#' @param path Path to the project
#' @keywords internal
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom utils head

has_rendered <- function(path = ".") {

  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)

  if (!fs::file_exists(path_log(path))){
    return(FALSE)
  }


  render_log <- log_report(path)

  last_rendered <- render_log %>%
                      arrange(desc(timestamp)) %>%
                      select(timestamp) %>%
                      head(1)


  rmd <- list.files(path, pattern = "\\.(r|R)md$")
  rscript <- list.files(path, pattern = "\\.R$")

  directory <- fs::dir_info(path)

  last_modified <- directory %>%
                      filter(basename(path) %in% c(rmd, rscript)) %>%
                      arrange(desc(modification_time)) %>%
                      select(modification_time) %>%
                      head(1)

  if (last_modified > last_rendered){
    return (FALSE)
  }

  return(TRUE)

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)

}


#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom crayon white black

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}

#' @importFrom utils packageVersion
#' @importFrom crayon red

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

#' Utility function to create a copy of a project in a temp directory
#' @import fs
#' @inheritParams fs::dir_exists
#' @export
#' @return A temp directory identical to your original directory.
#'
#' For example:
#'
#' \code{path <- "tests/testthat/project_noob"}
#' \code{temp_dir <- sandbox(path)}
#' \code{temp_dir}
#'
#' "/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpwBp1PN/project_noob"

sandbox <- function(path) {
  test_dir <- path(tempdir(), path_file(path))
  if (!fs_path(path) == test_dir) {
    if (dir_exists(test_dir)) {
      dir_delete(test_dir)
    }
    dir_copy(path, test_dir)
  }
  # remove any logs present
  log_clear(test_dir)
  return(test_dir)
}

#' Find the project root, but always return something
#' @inheritParams fs::path_norm
#' @importFrom rprojroot find_root is_rstudio_project has_file is_git_root
#' @export

proj_root <- function(path = ".") {


  root <- tryCatch(
    rprojroot::find_root(
      rprojroot::has_file(".here") |
        rprojroot::is_rstudio_project |
        rprojroot::is_git_root,
      path),
    error = function(e) {
      message(e)
      message(paste("Using working directory", getwd(), "instead"))
      return(getwd())
    }
  )
  return(path(root))
}


#' Override functions masked by fertile and run from the original packages.
#' @param expr Code to run as if \code{fertile} was not loaded
#' @export


danger <- function(expr) {
  detach("package:fertile", unload = TRUE)
  on.exit(require("fertile"), add = TRUE)
  invisible(expr)
}

flatten_lints <- lintr:::flatten_lints

#' Test projects in ZIP files
#' @inheritParams downloader::download
#' @export

check_from_zip <- function(url, ...) {
  lcl <- file_temp()
  downloader::download(url, destfile = lcl, ...)

  files <- utils::unzip(lcl, exdir = tempdir())
  path_dir <- path_common(files)

  sandbox(path_dir)
  x <- proj_check(path_dir)
}


# File type checks

#' Test whether a given path is to an image file
#' @param path Path to file you want to test
#' @export
#' @family file type checks

is_image_file <- function(path){

  check_is_file(path)

  type <- mime::guess_type(path_abs(path))

  if (grepl("image", type)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

#is_data_file

#' Test whether a given path is to a data file
#' @param path Path to file you want to test
#' @export
#' @family file type checks


is_data_file <- function(path){

  check_is_file(path)

  data_extensions <- c("data", "csv", "dat", "xml", "tsv", "json", "xls", "xlsx",
                       "sav", "syd", "mtp", "sas7bdat")

  # check if in extensions

  type <- tools::file_ext(path_abs(path))

  if (type %in% data_extensions){
    return(TRUE)
  }

  size <- file_info(path)$size

  if (type == "txt" & size > "10K"){
    return (TRUE)
  } else {
    return (FALSE)
  }

}



#' Test whether a given path is to a text file
#' @param path Path to file you want to test
#' @export
#' @family file type checks

is_text_file <- function(path){

  check_is_file(path)

  type <- mime::guess_type(path_abs(path))

  if (grepl("text", type)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

#' Test whether a given path is to an R file
#' @param path Path to file you want to test
#' @importFrom tools file_ext
#' @export
#' @family file type checks

is_r_file <- function(path){

  check_is_file(path)

  ext <- file_ext(path_abs(path))
  ext <- tolower(ext)

  if (ext %in% c(
    "rmd", "rproj", "r", "rscript", "rnw", "rda", "rdata") |
    grepl("README.md", path) == TRUE) {
    return (TRUE)
  }else{
    return (FALSE)
  }

}


#' Print a list of the available checks
#' @export
#' @rdname proj_check
#' @section list_checks:
#' Print a list of the available checks
#' provided by fertile for reference purposes

list_checks <- function(){

  msg("The available checks in `fertile` are as follows:")

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

  print(checks)

}




