# These shims are only activated if you've loaded the associated libraries
# Otherwise, they'll do nothing.
# This ensures that they can be under "Suggests" instead of "Imports"


# Readxl import functions


#' @rdname shims
#' @export

read_excel <- function(file, ...) {

  if("readxl" %in% (.packages())){

  if (interactive_log_on()) {
    log_push(file, "readxl::read_excel")
    check_path_safe(file, ... = "readxl::read_excel")
    readxl::read_excel(file, ...)
  }
  }else{
    message("fertile::read_excel() was called, but package 'readxl' is not loaded.")
    message("If you were trying to call another function with the same name, try again, but with '[package name]::' in front.")

  }



  }



# Rjson import functions


#' @rdname shims
#' @export

fromJSON <- function(json_str, file, ...) {

  if("rjson" %in% (.packages())){

  if (interactive_log_on()) {
    log_push(file, "rjson::fromJSON")
    check_path_safe(file, ... = "rjson::fromJSON")
    rjson::fromJSON(json_str, file, ...)
  }
  }else{
    message("fertile::fromJSON() was called, but package 'rjson' is not loaded.")
    message("If you were trying to call another function with the same name,try
          again, but with '[package name]::' in front.")
  }


  }


# Foreign import functions


#' @rdname shims
#' @export

read.dta <- function(file, ...) {

  if("foreign" %in% (.packages())){

  if (interactive_log_on()) {
    log_push(file, "foreign::read.dta")
    check_path_safe(file, ... = "foreign::read.dta")
    foreign::read.dta(file, ...)
  }
  }else{
    message("fertile::read.dta() was called, but package 'foreign' is not loaded.")
    message("If you were trying to call another function with the same name,try again, but with '[package name]::' in front.")
  }
}

#' @rdname shims
#' @export

read.mtp <- function(file) {

  if("foreign" %in% (.packages())){

  if (interactive_log_on()) {
    log_push(file, "foreign::read.mtp")
    check_path_safe(file, ... = "foreign::read.mtp")
    foreign::read.mtp(file)
  }
  }else{
    message("fertile::read.mtp() was called, but package 'foreign' is not loaded.")
    message("If you were trying to call another function with the same name,try again, but with '[package name]::' in front.")
 }
}


#' @rdname shims
#' @export

read.spss <- function(file, ...) {

  if("foreign" %in% (.packages())){


  if (interactive_log_on()) {
    log_push(file, "foreign::read.spss")
    check_path_safe(file, ... = "foreign::read.spss")
    foreign::read.spss(file, ...)
  }
  }else{
    message("fertile::read.spss() was called, but package 'foreign' is not loaded.")
    message("If you were trying to call another function with the same name,try again, but with '[package name]::' in front.")
  }
}


#' @rdname shims
#' @export

read.systat <- function(file, ...) {

  if("foreign" %in% (.packages())){

  if (interactive_log_on()) {
    log_push(file, "foreign::read.systat")
    check_path_safe(file, ... = "foreign::read.systat")
    foreign::read.systat(file, ...)
  }
  }else{
    message("fertile::read.systat() was called, but package 'foreign' is not loaded.")
    message("If you were trying to call another function with the same name,try again, but with '[package name]::' in front.")
  }
}


# SAS import functions

#' @rdname shims
#' @export

read.sas7bdat <- function(file, ...) {

  if("sas7bdat" %in% (.packages())){

  if (interactive_log_on()) {
    log_push(file, "sas7bdat::read.sas7bdat")
    check_path_safe(file, ... = "sas7bdat::read.sas7bdat")
    sas7bdat::read.sas7bdat(file, ...)
  }
  }else{
    message("fertile::read.sas7bdat() was called, but package 'sas7bdat' is not loaded.")
    message("If you were trying to call another function with the same name,try again, but with '[package name]::' in front.")
  }
}
