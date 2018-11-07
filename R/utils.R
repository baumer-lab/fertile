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
#' @importFrom fs path dir_exists dir_delete dir_copy
#' @inheritParams fs::dir_exists
#' @export

sandbox <- function(path) {
  test_dir <- fs::path(tempdir(), fs::path_file(path))
  if (fs::dir_exists(test_dir)) {
    fs::dir_delete(test_dir)
  }
  fs::dir_copy(path, test_dir)
  # remove any logs present
  log_clear(path_log(test_dir))
  return(test_dir)
}

#' Find the project root, but always return something
#' @inheritParams fs::path_norm
#' @importFrom rprojroot find_root is_rstudio_project has_file is_git_root
#' @importFrom fs path
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
  return(fs::path(root))
}

#' Danger block
#' @param expr Code to run as if \code{fertile} was not loaded
#' @export
#' @examples
#' danger(setwd(tempdir()))

danger <- function(expr) {
  detach("package:fertile", unload = TRUE)
  withCallingHandlers(expr, error = function(e) message(e))
  on.exit(require(fertile))
}
