#' Shims for common input/output functions
#' @name shims
#' @export
#' @param file The path
#' @param ... arguments passed to functions
#' @importFrom fs file_exists
#' @importFrom readr read_csv
#' @importFrom conflicted conflict_prefer
#' @seealso \code{\link[readr]{read_csv}}
#' @examples
#' \dontrun{
#' if (require(readr)) {
#'   read_csv(tempfile())
#' }
#' }

read_csv <- function(file, ...) {
#  conflicted::conflict_prefer("read_csv", "fertile")
  log_push(file, "read_csv")
  file_check(file)
  readr::read_csv(file, ...)
}

#' @rdname shims
#' @inheritParams readr::write_csv
#' @importFrom readr write_csv
#' @export
#' @seealso \code{\link[readr]{write_csv}}
#'
write_csv <- function(x, path, ...) {
  #  conflicted::conflict_prefer("write_csv", "fertile")
  log_push(path, "write_csv")
  file_check(path)
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
#  conflicted::conflict_prefer("read_csv", "fertile")
  log_push(file, "source")
  file_check(file)
  base::source(file, ...)
}



