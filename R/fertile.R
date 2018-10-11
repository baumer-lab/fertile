utils::globalVariables(c("value", "ext", "n", "timestamp", "size", "put_in",
"cmd", "dir_rel", "path_new"))

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

  proj_analyze(path)
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
  pkgs <- proj_analyze_pkgs(path)
  files <- proj_analyze_files(path, execute)
  return(list(packages = pkgs, files = files))
}


#' @rdname proj_test
#' @inheritParams proj_test
#' @param execute Do you want to actually move the files to their recommended location?
#' @importFrom dplyr select mutate group_by count arrange case_when desc pull
#' @importFrom fs path_ext path dir_create path_dir
#' @importFrom mime guess_type
#' @export

proj_analyze_files <- function(path = ".", execute = FALSE) {
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
#' @importFrom fs dir_ls
#' @importFrom purrr map map_dfr
#' @importFrom dplyr bind_rows
#' @importFrom tibble as.tibble
#' @importFrom requirements req_file
#' @export

proj_analyze_pkgs <- function(path = ".") {
  message("Analyzing packages used in project")
  r_code <- fs::dir_ls(path = path, type = "file", recursive = TRUE,
                       regexp = "\\.(?i)(r|rnw|rmd|rpres)$")
  pkgs <- purrr::map(r_code, requirements::req_file) %>%
    purrr::map(tibble::as.tibble) %>%
    purrr::map_dfr(dplyr::bind_rows, .id = "file")
  pkgs
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

