utils::globalVariables(c("value", "ext", "n", "timestamp", "size", "put_in", "cmd"))

#' Analyze project for reproducibility
#' @param path Path to package root
#' @return A \code{fertile} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom fs dir_ls path_ext
#' @examples
#' \dontrun{
#' proj_test()
#' }

proj_test <- function(path = ".") {
  message("Checking for reproducibility")

  files <- proj_analyze(path)
  proj_render(path)
  x <- proj_report(path)

  class(x) <- c("fertile", class(x))
  x
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @param execute Do you want to actually move the files to their recommended location?
#' @importFrom dplyr select mutate group_by count arrange case_when desc pull
#' @importFrom fs path_ext path dir_create path_dir
#' @importFrom mime guess_type
#' @export

proj_analyze <- function(path = ".", execute = FALSE) {
  message("Analyzing project file structure...")
  files <- fs::dir_info(path, recursive = FALSE, type = "file") %>%
    dplyr::select(file = path, size) %>%
    dplyr::mutate(ext = fs::path_ext(file),
                  mime = mime::guess_type(file),
                  path_rel = fs::path_rel(file, start = path),
                  dir_rel = dplyr::case_when(
      grepl("(README|DESCRIPTION|NAMESPACE|LICENSE)", fs::path_file(file)) ~ fs::path(""),
      tolower(ext) == "rproj" ~ fs::path(""),
      tolower(ext) == "r" ~ fs::path("R"),
      tolower(ext) %in% c("rda", "rdata") ~ fs::path("data"),
      tolower(ext) %in% c("dat", "csv", "tsv", "xml", "json", "zip") ~ fs::path("data-raw"),
      tolower(ext) == "txt" & size > "10K" ~ fs::path("data-raw"),
      tolower(ext) %in% c("rmd", "rnw", "md") ~ fs::path("vignettes"),
      grepl("csrc", mime) ~ fs::path("inst/c"),
      grepl("c\\+\\+", mime) ~ fs::path("inst/cpp"),
      grepl("py", mime) ~ fs::path("inst/python"),
      grepl("ruby", mime) ~ fs::path("inst/ruby"),
      grepl("perl", mime) ~ fs::path("inst/perl"),
      grepl("scala", mime) ~ fs::path("inst/scala"),
      grepl("javascript", mime) ~ fs::path("inst/javascript"),
      grepl("java", mime) ~ fs::path("inst/java"),
      grepl("sql", mime) ~ fs::path("inst/sql"),
      grepl("image/", mime) ~ fs::path("inst/image"),
      grepl("audio/", mime) ~ fs::path("inst/audio"),
      grepl("video/", mime) ~ fs::path("inst/video"),
      TRUE ~ fs::path("inst")
      ),
                  path_new = fs::path_tidy(fs::path(path, dir_rel, path_rel))
    )

  files_to_move <- files %>%
    dplyr::filter(fs::path_dir(path_new) != fs::path_dir(file)) %>%
    dplyr::mutate(cmd = paste0("fs::file_move('", file,
                               "', fs::dir_create('", fs::path_dir(path_new), "'))"))

  if (execute) {
    eval(parse(text = files_to_move$cmd))
  }

  files_to_move %>%
    dplyr::select(path_rel, dir_rel, cmd)
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @importFrom rmarkdown render
#' @importFrom testthat source_file
#' @importFrom purrr map_lgl
#' @export

proj_render <- function(path = ".") {
  message("Rendering R scripts...")
  # find all R, Rmd, rmd files and run them?
  # this is the easyMake part
  dir <- tempdir()

  rmd <- fs::dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.(r|R)md$")
  rmarkdown::render(rmd, output_dir = dir)

  r_script <- fs::dir_ls(path, recursive = TRUE, type = "file", regexp = "\\.R$")
  purrr::map_lgl(r_script, testthat::source_file)
}

#' @rdname proj_test
#' @inheritParams proj_test
#' @importFrom dplyr inner_join select
#' @export

proj_report <- function(path = ".") {
  message("Generating reproducibility report...")
  # tell you what you did wrong
  x <- log_report()
  # run checks on these paths
  y <- check_path(x$path, strict = FALSE)
  dplyr::inner_join(x, y, by = "path") %>%
    dplyr::select(-timestamp)
}

#' @rdname proj_test
#' @inheritParams base::print
#' @export

print.fertile <- function(x, ...) {
  NextMethod(x, ...)
}

