#' Shims for common input/output functions
#' @export
#' @param file The path
#' @param ... arguments passed to functions
#' @importFrom fs file_exists
#' @importFrom readr read_csv
#' @importFrom conflicted conflict_prefer
#' @seealso \code{\link[readr]{read_csv}}
#' @examples
#' if (require(readr)) {
#'   read_csv(test_paths$path[1])
#' }

read_csv <- function(file, ...) {
#  conflicted::conflict_prefer("read_csv", "fertile")
  push(file, "read_csv")
  checks(file)
  readr::read_csv(file, ...)
}

#' @rdname read_csv
#' @inheritParams base::setwd
#' @export

setwd <- function(dir) {
  stop("setwd() is likely to break reproducibility. Use here::here() instead.")
}

#' @rdname read_csv
#' @export

source <- function(file, ...) {
#  conflicted::conflict_prefer("read_csv", "fertile")
  push(file, "source")
  checks(file)
  base::source(file, ...)
}

