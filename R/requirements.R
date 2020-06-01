# The code in this file comes from the `requirements` package by Hadley Wickham on Github
# https://github.com/hadley/requirements

#' Extract requirements from code
#'
#' Looks for `::`, `:::`, `library()`, `require()`, `requireNamespace()`,
#' and `loadNamespace()`.
#'
#' @param x Code to examine. Supports unquoting.
#' @export
#' @keywords internal

req_code <- function(x) {
  x <- rlang::enexpr(x)
  unique(find_pkgs_rec(x))
}

#' Find packages
#' @export
#' @keywords internal
find_pkgs_rec <- function(x) {
  if (rlang::is_syntactic_literal(x) || rlang::is_symbol(x)) {
    return(character())
  }

  if (rlang::is_pairlist(x) || is.expression(x)) {
    return(flat_map_chr(as.list(x), find_pkgs_rec))
  }

  if (rlang::is_call(x, c("::", ":::"))) {
    char_or_sym(x[[2]])
  } else if (rlang::is_call(x, c("library", "require"))) {
    x <- rlang::call_standardise(x, env = baseenv())
    if (isTRUE(x$character.only) || identical(x$character.only, quote(T))) {
      if (is.character(x$package)) {
        x$package
      } else {
        character()
      }
    } else {
      char_or_sym(x$package)
    }
  } else if (rlang::is_call(x, c("requireNamespace", "loadNamespace"))) {
    x <- rlang::call_standardise(x, env = baseenv())
    char_or_sym(x$package)
  } else {
    flat_map_chr(as.list(x), find_pkgs_rec)
  }

}


#' Extract requirements from a file
#'
#' @description
#' * `.R`: extracts requirements from parsed code
#'
#' * `.Rmd` & `.Rpres`: requirements from chunks (parsed using regular
#'   expressions to avoid dependency on knitr package). If rmarkdown package is
#'   installed, will also add requirements from custom output type.
#'
#' * `.Rnw`: tangles the document and then extracts from `.R` file.
#'
#' @param path Path to file
#' @export
#' @keywords internal

req_file <- function(path) {
  if (!file.exists(path)) {
    stop("`path` does not exist", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))

  switch(ext,
         r = req_file_r(path),
         rmd = req_file_rmd(path),
         rnw = req_file_rnw(path),
         rpres = req_file_rmd(path),
         character()
  )
}

# .R ----------------------------------------------------------------------

#' File requirements
#' @export
#' @keywords internal
req_file_r <- function(path) {
  tryCatch(
    error = function(err) character(),
    {
      code <- parse(path)
      req_code(!!code)
    }
  )
}

# .Rmd --------------------------------------------------------------------

#' Rmd requirements
#' @export
#' @keywords internal
req_file_rmd <- function(path) {
  lines <- readLines(path)

  chunks <- rmd_chunks(lines)
  chunk_reqs <- flat_map_chr(chunks, req_text)

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    yaml_reqs <- character()
  } else {
    format <- rmarkdown::default_output_format(path)
    yaml_reqs <- req_text(format)
  }

  reqs <- c("rmarkdown", chunk_reqs, yaml_reqs)
  unique(reqs)
}

#' Read chunks
#' @export
#' @keywords internal
rmd_chunks <- function(lines) {
  # From https://github.com/rstudio/rstudio/blob/0edb05f67b4f2eea25b8cfb15f7c64ec9b27b288/src/gwt/acesupport/acemode/rmarkdown_highlight_rules.js#L181-L184
  chunk_start_re <- "^(?:[ ]{4})?`{3,}\\s*\\{[Rr]\\b(?:.*)engine\\s*\\=\\s*['\"][rR]['\"](?:.*)\\}\\s*$|^(?:[ ]{4})?`{3,}\\s*\\{[rR]\\b(?:.*)\\}\\s*$";
  chunk_end_re <- "^(?:[ ]{4})?`{3,}\\s*$"

  chunk_start <- grepl(chunk_start_re, lines, perl = TRUE)
  chunk_end <- grepl(chunk_end_re, lines, perl = TRUE)

  chunk_num <- cumsum(chunk_start)
  in_chunk <- (chunk_num - cumsum(chunk_end)) != 0

  chunks <- split(lines[in_chunk], chunk_num[in_chunk])
  names(chunks) <- NULL

  # Strip off first element, the chunk header
  chunks <- lapply(chunks, function(x) x[-1])
  lapply(chunks, paste, collapse = "\n")
}

#' Read text for requirements
#' @export
#' @keywords internal
req_text <- function(text) {
  tryCatch(
    error = function(err) character(),
    {
      code <- parse(text = text)
      req_code(!!code)
    }
  )
}


# .Rnw --------------------------------------------------------------------

#' Rnw requirements
#' @export
#' @keywords internal
req_file_rnw <- function(path) {
  tempfile <- tempfile()
  on.exit(unlink(tempfile))

  utils::Stangle(path, output = tempfile, quiet = TRUE)
  req_file_r(tempfile)
}


#' Utility function
#' @export
#' @keywords internal
flat_map_chr <- function(x, f, ...) {
  if (length(x) == 0) {
    character()
  } else {
    unlist(lapply(x, f, ...))
  }
}

#' Utility function
#' @export
#' @keywords internal
char_or_sym <- function(x) {
  if (is.character(x)) {
    x
  } else if (is.symbol(x)) {
    as.character(x)
  } else {
    character()
  }
}
