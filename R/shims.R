#' Shims for common input/output functions
#' @name shims
#' @keywords internal
#' @export

read_csv <- function(file, ...) {
  log_push(file, "readr::read_csv")
  check_path(file)
  readr::read_csv(file, ...)
}

#' @rdname shims
#' @export

read.csv <- function(file, ...) {
  log_push(file, "utils::read.csv")
  check_path(file)
  utils::read.csv(file, ...)
}

#' @rdname shims
#' @export

load <- function(file, envir = parent.frame(), verbose = FALSE) {
  log_push(file, "base::load")
  check_path(file)
  base::load(file, envir, verbose)
}

#' @rdname shims
#' @export

source <- function(file, ...) {
  log_push(file, "base::source")
  check_path(file)
  base::source(file, ...)
}


## Export functions

#' @rdname shims
#' @export

write.csv <- function(x, file, ...) {
  log_push(file, "utils::write.csv")
  check_path(file)
  utils::write.csv(x, file, ...)
}


#' @rdname shims
#' @export

write_csv <- function(x, path, ...) {
  log_push(path, "readr::write_csv")
  check_path(path)
  utils::write.csv(x, path, ...)
}

#' @rdname shims
#' @export

setwd <- function(dir) {
  stop("setwd() is likely to break reproducibility. Use here::here() instead.")
}

#' @rdname shims
#' @export

save <- function(..., list = character(),
                 file = stop("'file' must be specified"),
                 ascii = FALSE, version = NULL, envir = parent.frame(),
                 compress = isTRUE(!ascii), compression_level,
                 eval.promises = TRUE, precheck = TRUE) {
  log_push(file, "base::save")
  check_path(file)
  base::save(..., list = list, file = file, ascii = ascii,
             version = version, envir = envir,
             compress = compress, compression_level = compression_level,
             eval.promises = eval.promises, precheck = precheck)
}

#' @rdname shims
#' @export

ggsave <- function(filename, ...) {
  log_push(filename, "ggplot2::ggsave")
  check_path(filename)
  ggplot2::ggsave(filename, ...)
}

#' @rdname shims
#' @export
#' @inheritParams base::library
#' @seealso \code{\link[base]{library}}

# library <- function(package, help, ...) {
#   pkg <- rlang::enquo(package)
#   pkg_string <- paste0(pkg)
#   log_push(pkg_string, "library")
#   base::library(!!pkg)
# }


