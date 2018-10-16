# Function factory for input shims
#' @importFrom rlang exprs new_function expr caller_env
#' @importFrom purrr map

shim_log_input <- function(.f) {
  rlang::new_function(
    rlang::exprs(file = , ... = ),
    rlang::expr({
      fertile:::log_push(file, !!(deparse(.f)))
      check_path(file)
      (!!.f)(file, ...)
    }),
    rlang::caller_env()
  )
}

input_funs_to_shim <- rlang::exprs(
  utils::read.csv,
  readr::read_csv,
  base::source
)

input_shims <- purrr::map(input_funs_to_shim, shim_log_input)
names(input_shims) <- input_funs_to_shim


#' Shims for common input/output functions
#' @name shims
#' @export
#' @inheritParams readr::read_csv
#' @param ... arguments passed to functions
#' @importFrom readr read_csv
#' @seealso \code{\link[readr]{read_csv}}

read_csv <- input_shims$`readr::read_csv`

#' @rdname shims
#' @export
#' @inheritParams utils::read.csv
#' @importFrom utils read.csv
#' @seealso \code{\link[utils]{read.csv}}

read.csv <- input_shims$`utils::read.csv`

#' @rdname shims
#' @export
#' @seealso \code{\link[base]{source}}

source <- input_shims$`base::source`


# Function factory for output shims

shim_log_output <- function(.f) {
  rlang::new_function(
    rlang::exprs(x = , file = , ... = ),
    rlang::expr({
      fertile:::log_push(file, !!(deparse(.f)))
      check_path(file)
      (!!.f)(x, file, ...)
    }),
    rlang::caller_env()
  )
}

output_funs_to_shim <- rlang::exprs(
  utils::write.csv,
  readr::write_csv
)

output_shims <- purrr::map(output_funs_to_shim, shim_log_output)
names(output_shims) <- output_funs_to_shim

#' @rdname shims
#' @importFrom utils write.csv
#' @export
#' @seealso \code{\link[utils]{write.csv}}
#'
write.csv <- output_shims$`utils::write.csv`

#' @rdname shims
#' @importFrom readr write_csv
#' @inheritParams readr::write_csv
#' @export
#' @seealso \code{\link[readr]{write_csv}}
#'
write_csv <- output_shims$`readr::write_csv`

#' @rdname shims
#' @inheritParams base::setwd
#' @export
#' @seealso \code{\link[base]{setwd}}

setwd <- function(dir) {
  stop("setwd() is likely to break reproducibility. Use here::here() instead.")
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


