#' Shims for common input/output functions
#' @name shims
#' @keywords internal
#' @export

          # Utils import functions

#' @rdname shims
#' @export

read.csv <- function(file, ...) {
  log_push(file, "utils::read.csv")
  check_path_safe(file)
  utils::read.csv(file, ...)
}


#' @rdname shims
#' @export

read.csv2 <- function(file, ...) {
  log_push(file, "utils::read.csv2")
  check_path_safe(file)
  utils::read.csv2(file, ...)
}

#' @rdname shims
#' @export

read.delim <- function(file, ...){
  log_push(file, "utils::read.delim")
  check_path_safe(file)
  utils::read.delim(file, ...)
}


#' @rdname shims
#' @export

read.delim2 <- function(file, ...){
  log_push(file, "utils::read.delim2")
  check_path_safe(file)
  utils::read.delim2(file, ...)
}

#' @rdname shims
#' @export

read.DIF <- function(file, ...){
  log_push(file, "utils::read.DIF")
  check_path_safe(file)
  utils::read.DIF(file, ...)
}

#' @rdname shims
#' @export

read.fortran <- function(file, ...){
  log_push(file, "utils::read.fortran")
  check_path_safe(file)
  utils::read.fortran(file, ...)
}


#' @rdname shims
#' @export

read.fwf <- function(file, ...){
  log_push(file, "utils::read.fwf")
  check_path_safe(file)
  utils::read.fwf(file, ...)

}

#' @rdname shims
#' @export

read.table <- function(file, ...){
  log_push(file, "utils::read.table")
  check_path_safe(file)
  utils::read.table(file, ...)

}


          # Readr import functions


#' @rdname shims
#' @export

read_csv <- function(file, ...) {
  log_push(file, "readr::read_csv")
  check_path_safe(file)
  readr::read_csv(file, ...)

}

#' @rdname shims
#' @export

read_csv2 <- function(file, ...) {
  log_push(file, "readr::read_csv2")
  check_path_safe(file)
  readr::read_csv2(file, ...)

}

#' @rdname shims
#' @export

read_delim <- function(file, ...){
  log_push(file, "readr::read_delim")
  check_path_safe(file)
  readr::read_delim(file, ...)
}


#' @rdname shims
#' @export

read_file <- function(file, ...){
  log_push(file, "readr::read_file")
  check_path_safe(file)
  readr::read_file(file, ...)
}

#' @rdname shims
#' @export

read_file_raw <- function(file){
  log_push(file, "readr::read_file_raw")
  check_path_safe(file)
  readr::read_file_raw(file)
}


#' @rdname shims
#' @export

read_fwf <- function(file, col_positions, ...){
  log_push(file, "readr::read_fwf")
  check_path_safe(file)
  readr::read_fwf(file, col_positions, ...)

}


#' @rdname shims
#' @export

read_lines <- function(file, ...){
  log_push(file, "readr::read_lines")
  check_path_safe(file)
  readr::read_lines(file, ...)
}

#' @rdname shims
#' @export

read_lines_raw <- function(file, ...){
  log_push(file, "readr::read_lines_raw")
  check_path_safe(file)
  readr::read_lines_raw(file, ...)
}

#' @rdname shims
#' @export

read_log <- function(file, ...){
  log_push(file, "readr::read_log")
  check_path_safe(file)
  readr::read_log(file, ...)
}

#' @rdname shims
#' @export

read_table <- function(file, ...){
  log_push(file, "readr::read_table")
  check_path_safe(file)
  readr::read_table(file, ...)

}

#' @rdname shims
#' @export

read_table2 <- function(file, ...){
  log_push(file, "readr::read_table2")
  check_path_safe(file)
  readr::read_table2(file, ...)

}

#' @rdname shims
#' @export

read_tsv <- function(file, ...){
  log_push(file, "readr::read_tsv")
  check_path_safe(file)
  readr::read_tsv(file, ...)
}


    # Base import functions


#' @rdname shims
#' @export

read.dcf <- function(file, ...){
  log_push(file, "base::read.dcf")
  check_path_safe(file)
  base::read.dcf(file, ...)
}


#' @rdname shims
#' @export

load <- function(file, envir = parent.frame(), verbose = FALSE) {
  log_push(file, "base::load")
  check_path_safe(file)
  base::load(file, envir, verbose)
}

#' @rdname shims
#' @export

source <- function(file, ...) {
  log_push(file, "base::source")
  check_path_safe(file)
  base::source(file, ...)
}


      # Readxl import functions


#' @rdname shims
#' @export

read_excel <- function(file, ...){
  log_push(file, "readxl::read_excel")
  check_path_safe(file)
  readxl::read_excel(file, ...)
}



      # Stats import functions


#' @rdname shims
#' @export

read.ftable <- function(file, ...){
  log_push(file, "stats::read.ftable")
  check_path_safe(file)
  stats::read.ftable(file, ...)
}



      # Rjson import functions


#' @rdname shims
#' @export

fromJSON <- function(json_str, file, ...){
  log_push(file, "rjson::fromJSON")
  check_path_safe(file)
  rjson::fromJSON(json_str, file, ...)

}


      # Foreign import functions


#' @rdname shims
#' @export

read.dta <- function(file, ...){
  log_push(file, "foreign::read.dta")
  check_path_safe(file)
  foreign::read.dta(file, ...)

}

#' @rdname shims
#' @export

read.mtp <- function(file){
  log_push(file, "foreign::read.mtp")
  check_path_safe(file)
  foreign::read.mtp(file)

}


#' @rdname shims
#' @export

read.spss <- function(file, ...){
  log_push(file, "foreign::read.spss")
  check_path_safe(file)
  foreign::read.spss(file, ...)

}


#' @rdname shims
#' @export

read.systat <- function(file, ...){
  log_push(file, "foreign::read.systat")
  check_path_safe(file)
  foreign::read.systat(file, ...)

}


      # SAS import functions

#' @rdname shims
#' @export

read.sas7bdat <- function(file, ...){
  log_push(file, "sas7bdat::read.sas7bdat")
  check_path_safe(file)
  sas7bdat::read.sas7bdat(file, ...)

}



      # Non-Import functions


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



        # Export functions


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



      # Packages


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
