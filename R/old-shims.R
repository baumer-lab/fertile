# File containing the old shims that will eventually be removed
# once shim editing functions are fully tested


# Readxl import functions


#' @rdname shims
#' @export

read_excel <- function(file, ...) {
  if (interactive_log_on()) {
    log_push(file, "readxl::read_excel")
    check_path_safe(file, ... = "readxl::read_excel")
    readxl::read_excel(file, ...)
  }
}



# Rjson import functions


#' @rdname shims
#' @export

fromJSON <- function(json_str, file, ...) {
  if (interactive_log_on()) {
    log_push(file, "rjson::fromJSON")
    check_path_safe(file, ... = "rjson::fromJSON")
    rjson::fromJSON(json_str, file, ...)
  }
}


# Foreign import functions


#' @rdname shims
#' @export

read.dta <- function(file, ...) {
  if (interactive_log_on()) {
    log_push(file, "foreign::read.dta")
    check_path_safe(file, ... = "foreign::read.dta")
    foreign::read.dta(file, ...)
  }
}

#' @rdname shims
#' @export

read.mtp <- function(file) {
  if (interactive_log_on()) {
    log_push(file, "foreign::read.mtp")
    check_path_safe(file, ... = "foreign::read.mtp")
    foreign::read.mtp(file)
  }
}


#' @rdname shims
#' @export

read.spss <- function(file, ...) {
  if (interactive_log_on()) {
    log_push(file, "foreign::read.spss")
    check_path_safe(file, ... = "foreign::read.spss")
    foreign::read.spss(file, ...)
  }
}


#' @rdname shims
#' @export

read.systat <- function(file, ...) {
  if (interactive_log_on()) {
    log_push(file, "foreign::read.systat")
    check_path_safe(file, ... = "foreign::read.systat")
    foreign::read.systat(file, ...)
  }
}


# SAS import functions

#' @rdname shims
#' @export

read.sas7bdat <- function(file, ...) {
  if (interactive_log_on()) {
    log_push(file, "sas7bdat::read.sas7bdat")
    check_path_safe(file, ... = "sas7bdat::read.sas7bdat")
    sas7bdat::read.sas7bdat(file, ...)
  }
}
