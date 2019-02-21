#' Shims for common input/output functions
#' @name shims
#' @keywords internal
#' @export

#' @rdname shims
#' @export

read_csv <- function(file, ...) {
  log_push(file, "readr::read_csv")
  if(Sys.getenv("FERTILE_RENDER_MODE") == FALSE){
    check_path(file)
  }
  readr::read_csv(file, ...)
}

#' @rdname shims
#' @export

read.csv <- function(file, ...) {
  log_push(file, "utils::read.csv")
  if(Sys.getenv("FERTILE_RENDER_MODE") == FALSE){
    check_path(file)
  }
  utils::read.csv(file, ...)
}

#' @rdname shims
#' @export

load <- function(file, envir = parent.frame(), verbose = FALSE) {
  log_push(file, "base::load")
  if(Sys.getenv("FERTILE_RENDER_MODE") == FALSE){
    check_path(file)
  }
  base::load(file, envir, verbose)
}

#' @rdname shims
#' @export

source <- function(file, ...) {
  log_push(file, "base::source")
  if(Sys.getenv("FERTILE_RENDER_MODE") == FALSE){
    check_path(file)
  }
  base::source(file, ...)
}

#' @rdname shims
#' @export

tbl <- function(src, ...) {
  log_push(dplyr::db_desc(src$con), "dplyr::tbl")
  dplyr::tbl(src, ...)
}

#' @rdname shims
#' @export

set.seed <- function(n, ...) {
  log_push(paste("seed", n, sep = ":"), "base::set.seed")
  base::set.seed(n)
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
  rlang::abort(
    "setwd() is likely to break reproducibility. Use here::here() instead.")
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
#' @seealso \code{\link[base]{library}}

# Stolen from https://github.com/r-lib/conflicted/blob/master/R/shim.R

library <- function(package,
                    help,
                    pos = 2,
                    lib.loc = NULL,
                    character.only = FALSE,
                    logical.return = FALSE,
                    warn.conflicts = TRUE,
                    quietly = FALSE,
                    verbose = getOption("verbose")
) {

  if (!missing(package)) {
    package <- package_name(rlang::enquo(package),
                            character.only = character.only)

    log_push(paste("package", package, sep = ":"), "base::library")

    base::library(
      package,
      pos = pos,
      lib.loc = lib.loc,
      character.only = TRUE,
      logical.return = logical.return,
      warn.conflicts = FALSE,
      quietly = quietly,
      verbose = verbose
    )

  } else if (!missing(help)) {
    help <- package_name(rlang::enquo(help), character.only = character.only)
    log_push(paste("package", help, sep = ":"), "base::library")
    base::library(
      help = help,
      character.only = TRUE
    )
  } else {
    base::library(
      lib.loc = lib.loc,
      logical.return = logical.return
    )
  }

}

#' @rdname shims
#' @export
#' @seealso \code{\link[base]{require}}


require <- function(package,
                    lib.loc = NULL,
                    quietly = FALSE,
                    warn.conflicts = TRUE,
                    character.only = FALSE) {

  package <- package_name(rlang::enquo(package),
                          character.only = character.only)

  log_push(paste("package", package, sep = ":"), "base::require")

  base::require(
    package,
    lib.loc = lib.loc,
    quietly = quietly,
    warn.conflicts = FALSE,
    character.only = TRUE
  )

}

package_name <- function(package, character.only = FALSE) {
  if (!character.only) {
    package <- as.character(rlang::quo_expr(package))
  } else {
    package <- rlang::eval_tidy(package)
  }

  if (!is.character(package) || length(package) != 1L) {
    rlang::abort("`package` must be character vector of length 1.")
  }
  if (is.na(package) || (package == "")) {
    rlang::abort("`package` must not be NA or ''.")
  }

  package
}
