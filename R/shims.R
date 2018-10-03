#' Shims for common input/output functions
#' @name shims
#' @export
#' @param file The path
#' @param ... arguments passed to functions
#' @importFrom fs file_exists
#' @importFrom readr read_csv
#' @seealso \code{\link[readr]{read_csv}}
#' @examples
#' \dontrun{
#' if (require(readr)) {
#'   read_csv(tempfile())
#' }
#' }

read_csv <- function(file, ...) {
  log_push(file, "read_csv")
  check_path(file)
  readr::read_csv(file, ...)
}

#' @rdname shims
#' @inheritParams readr::write_csv
#' @importFrom readr write_csv
#' @export
#' @seealso \code{\link[readr]{write_csv}}
#'
write_csv <- function(x, path, ...) {
  log_push(path, "write_csv")
  check_path(path)
  readr::write_csv(x, path, ...)
}

#' @rdname shims
#' @inheritParams base::setwd
#' @export
#' @seealso \code{\link[base]{setwd}}

setwd <- function(dir) {
  stop("setwd() is likely to break reproducibility. Use here::here() instead.")
}

#' @rdname shims
#' @export
#' @seealso \code{\link[base]{source}}

source <- function(file, ...) {
  log_push(file, "source")
  check_path(file)
  base::source(file, ...)
}

#' @rdname shims
#' @export
#' @inheritParams ggplot2::ggsave
#' @importFrom ggplot2 ggsave
#' @seealso \code{\link[ggplot2]{ggsave}}

ggsave <- function(filename, ...) {
  log_push(filename, "ggsave")
  check_path(filename)
  ggplot2::ggsave(filename, ...)
}



