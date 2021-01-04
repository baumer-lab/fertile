#' Load shims into environment when fertile is attached
#' @param libname a character string giving the library directory where the package defining the namespace was found
#' @param pkgname a character string giving the name of the package
.onAttach <- function(libname, pkgname) {
  if (Sys.getenv("IN_TESTTHAT") != TRUE & fs::dir_exists(Sys.getenv("HOME"))) {
    load_shims()
  }
}

#' Remove shims from environment when fertile is detached
#' @param libpath a character string giving the complete path to the package
.onDetach <- function(libpath) {
  if (Sys.getenv("IN_TESTTHAT") != TRUE) {
    unload_shims()
  }
}


# stolen from tidyverse
# https://github.com/tidyverse/tidyverse/blob/a720dcd73d9e3fc0ec86317bc0abaf8f0077e8bd/R/utils.R

#' @importFrom crayon bold
#' @importFrom cli rule

msg <- function(text) {
  cli::rule(
    left = crayon::bold(text),
    right = paste0("fertile ", package_version("fertile"))
  ) %>%
    text_col() %>%
    message()
}


#' load a list of packages and export the session info of them
#' @param packages_to_load vector of package names
#' @param path path of directory we are working in
#' @export
#' @importFrom utils getS3method
#' @keywords internal
to_execute <- function(packages_to_load, path) {
  suppressMessages(purrr::map(packages_to_load, library))

  dependency_info <- capture.output(getS3method("print", "sessionInfo")(sessionInfo()[-8]))

  # if fertile was not one of the packages called by the code, remove it!

  if (!"fertile" %in% packages_to_load) {
    fertile_loc <- grep("fertile", dependency_info)

    spaces <- gregexpr(" ", dependency_info[fertile_loc])[[1]]
    fertile_end <- spaces[2]

    line_of_interest <- dependency_info[fertile_loc]
    replacement_line <- substr(line_of_interest, fertile_end + 1, nchar(line_of_interest))

    dependency_info[fertile_loc] <- replacement_line
  }

  # Remove vector indices for all of the lists

  lines_with_indices <- grep("\\[", dependency_info)

  for (index in lines_with_indices) {
    line_of_interest <- dependency_info[index]
    replacement_line <- substr(line_of_interest, 5, nchar(line_of_interest))
    dependency_info[index] <- replacement_line
  }

  line1 <- paste0(
    "The R project located at '", fs::path_abs(path),
    "' was last run in the following software environment:"
  )
  # Add a piece of text at the top of the file:
  dependency_info <- append("", dependency_info, length(dependency_info))
  dependency_info <- append("", dependency_info, length(dependency_info))
  dependency_info <- append(line1, dependency_info, length(dependency_info))


  writeLines(dependency_info, fs::path(path, ".software-versions.txt"))
}




#' Check whether a provided path is a directory
#' @param path Path you are wanting to check
#' @importFrom rlang abort
#' @export
#' @family path type checks

check_is_dir <- function(path) {
  if (fs::is_dir(path)) {
    return(path)
  }

  rlang::abort(message = "The path you provided is NOT to a directory.
  Please provide a path to a directory instead.")
}


#' Check whether a provided path is a file
#' @param path Path you are wanting to check
#' @importFrom rlang abort
#' @export
#' @family path type checks

check_is_file <- function(path) {
  if (fs::is_file(path)) {
    return(path)
  }

  rlang::abort(message = "The path you provided is NOT to a file.
               Please provide a path to a file instead.")
}


#' Utility function to check whether a project has been updated since last rendered
#' @param path Path to the project
#' @keywords internal
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom utils head

has_rendered <- function(path = ".") {
  Sys.setenv("FERTILE_RENDER_MODE" = TRUE)

  render_log <- path_log(path)

  # Delete the log if it's empty
  if (fs::file_exists(render_log) & file.size(render_log) == 0) {
    fs::file_delete(render_log)
  }

  if (!fs::file_exists(render_log)) {
    return(FALSE)
  }

  render_log <- log_report(path)

  ever_rendered <- nrow(render_log) > 0

  if (ever_rendered) {
    last_rendered <- render_log %>%
      dplyr::arrange(desc(timestamp)) %>%
      dplyr::pull(timestamp) %>%
      head(1)
  } else {
    last_rendered <- -Inf
  }

  rmd <- list.files(path, pattern = "\\.(r|R)md$")
  rscript <- list.files(path, pattern = "\\.R$")

  directory <- fs::dir_info(path)

  scripts <- directory %>%
    dplyr::filter(basename(path) %in% c(rmd, rscript))

  if (nrow(scripts) > 0) {
    last_modified <- scripts %>%
      dplyr::arrange(desc(modification_time)) %>%
      dplyr::pull(modification_time) %>%
      head(1)
  } else {
    last_modified <- 0
  }


  if (last_modified > last_rendered) {
    return(FALSE)
  }

  return(TRUE)

  Sys.setenv("FERTILE_RENDER_MODE" = FALSE)
}


#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom crayon white black

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

#' @importFrom utils packageVersion
#' @importFrom crayon red

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

#' Utility function to create a copy of a project in a temp directory
#' @import fs
#' @inheritParams fs::dir_exists
#' @export
#' @return A temp directory identical to your original directory.
#'
#' For example:
#'
#' \code{path <- "tests/testthat/project_noob"}
#' \code{temp_dir <- sandbox(path)}
#' \code{temp_dir}
#'
#' "/var/folders/v6/f62qz88s0sd5n3yqw9d8sb300000gn/T/RtmpwBp1PN/project_noob"

sandbox <- function(path) {
  test_dir <- path(tempdir(), path_file(path))
  if (!fs_path(path) == test_dir) {
    if (dir_exists(test_dir)) {
      dir_delete(test_dir)
    }
    dir_copy(path, test_dir)
  }
  # remove any logs present
  log_clear(test_dir)
  return(test_dir)
}

#' Find the project root, but always return something
#' @inheritParams fs::path_norm
#' @importFrom rprojroot find_root is_rstudio_project has_file is_git_root
#' @export

proj_root <- function(path = ".") {
  root <- tryCatch(
    rprojroot::find_root(
      rprojroot::has_file(".here") |
        rprojroot::is_rstudio_project |
        rprojroot::is_git_root,
      path
    ),
    error = function(e) {
      message(e)
      message(paste("Using working directory", getwd(), "instead"))
      return(getwd())
    }
  )
  return(path(root))
}


#' Override functions masked by fertile and run from the original packages.
#' @param expr Code to run as if \code{fertile} was not loaded
#' @export


danger <- function(expr) {
  detach("package:fertile", unload = TRUE)
  on.exit(require("fertile"), add = TRUE)
  invisible(expr)
}

flatten_lints <- lintr:::flatten_lints

#' Test projects in ZIP files
#' @inheritParams downloader::download
#' @export

check_from_zip <- function(url, ...) {
  lcl <- file_temp()
  downloader::download(url, destfile = lcl, ...)

  files <- utils::unzip(lcl, exdir = tempdir())
  path_dir <- path_common(files)

  sandbox(path_dir)
  x <- proj_check(path_dir)
}



# File type checks

#' Test whether a given path is to an image file
#' @param path Path to file you want to test
#' @export
#' @family file type checks

is_image_file <- function(path) {
  check_is_file(path)

  type <- mime::guess_type(path_abs(path))

  if (grepl("image", type)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# is_data_file

#' Test whether a given path is to a data file
#' @param path Path to file you want to test
#' @export
#' @family file type checks


is_data_file <- function(path) {
  check_is_file(path)

  data_extensions <- c(
    "data", "csv", "dat", "xml", "tsv", "json", "xls", "xlsx",
    "sav", "syd", "mtp", "sas7bdat"
  )

  # check if in extensions

  type <- tools::file_ext(path_abs(path))

  if (type %in% data_extensions) {
    return(TRUE)
  }

  size <- file_info(path)$size

  if (type == "txt" & size > "10K") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



#' Test whether a given path is to a text file
#' @param path Path to file you want to test
#' @export
#' @family file type checks

is_text_file <- function(path) {
  check_is_file(path)

  type <- mime::guess_type(path_abs(path))

  if (grepl("text", type)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Test whether a given path is to an R file
#' @param path Path to file you want to test
#' @importFrom tools file_ext
#' @export
#' @family file type checks

is_r_file <- function(path) {
  check_is_file(path)

  ext <- file_ext(path_abs(path))
  ext <- tolower(ext)

  if (ext %in% c(
    "rmd", "rproj", "r", "rscript", "rnw", "rda", "rdata"
  ) |
    grepl("README.md", path) == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Print a list of the available checks
#' @export
#' @rdname proj_check
#' @section list_checks:
#' Print a list of the available checks
#' provided by fertile for reference purposes

list_checks <- function() {
  msg("The available checks in `fertile` are as follows:")

  checks <- c(
    "has_tidy_media",
    "has_tidy_images",
    "has_tidy_code",
    "has_tidy_raw_data",
    "has_tidy_data",
    "has_tidy_scripts",
    "has_readme",
    "has_no_lint",
    "has_proj_root",
    "has_no_nested_proj_root",
    "has_only_used_files",
    "has_clear_build_chain",
    "has_no_absolute_paths",
    "has_only_portable_paths",
    "has_no_randomness",
    "has_well_commented_code"
  )

  print(checks)
}

#' Return name of a given function's file path-related argument
#' @param func name of function to check for path argument (e.g. "read_csv")
#' @param package name of package that provided function is from (e.g. "readr")
#' @export
#' @keywords internal

takes_path_arg <- function(func, package = "") {

  # See if a package name was provided
  pkg_name_provided <- ifelse(package == "", FALSE, TRUE)

  # If a package was NOT provided, check to see if more than 1 func loaded.
  # If more than 1 function loaded w/ same name, return message telling
  # user to specify package.
  pkgs_with_func <- grep("package:", utils::find(func), value = TRUE)
  pkgs_with_func <- gsub(".*:", "", pkgs_with_func)



  if (pkg_name_provided == FALSE & length(pkgs_with_func) > 1) {
    text_too_many <- paste0(
      "A function with the name '", func, "' exists in more than one loaded package. \n ",
      "Please specify which package's function you would like to use via 'package = _' \n"
    )


    rlang::abort(message = text_too_many)
  }


  # If package was not provided and no function was loaded with the given name
  # Return an error message asking for a package

  if (pkg_name_provided == FALSE & length(pkgs_with_func) == 0) {
    text_none_found <- paste0(
      "None of the loaded packages in your R environment contain a function called '", func, "'. \n",
      "To help find the correct function, please specify the name of the package you would like to search in via 'package = _'"
    )

    rlang::abort(message = text_none_found)
  }

  # If package was provided OR only 1 function was loaded with given name, return the name of the file path-related argument
  if (pkg_name_provided == TRUE | (pkg_name_provided == FALSE & length(pkgs_with_func) == 1)) {
    if (pkg_name_provided == TRUE) {
      to_eval <- paste0("formals(", package, "::", func, ")")
    } else {
      to_eval <- paste0("formals(", pkgs_with_func, "::", func, ")")
    }

    args <- eval(parse(text = to_eval))

    args_vector <- names(args)

    # return all arguments with names that seem related to paths

    path_args <- c()

    for (arg in args_vector) {
      if (arg %in% c("file", "path", "filepath")) {
        path_args <- path_args %>% append(arg)
      }
    }

    if (length(path_args) == 0) {
      return(FALSE)
    }


    return(path_args)
  }
}


#' Generate the code associated with writing a shim
#' @param func name of function you want to create a shim for (e.g. "read_excel")
#' @param package name of package that provided function is from (e.g. "readxl")
#' @param path_arg name of path-related argument in that function (if not specified, fertile will make an educated guess).
#' @return vector of lines making up code for shim
#' @export
#' @keywords internal

get_shim_code <- function(func, package = "", path_arg = "") {

  # Get name of path argument to provided function

  if (package == "") {
    pkg <- grep("package:", utils::find(func), value = TRUE)
    pkg <- gsub(".*:", "", pkg)
  } else {
    pkg <- package
  }

  # Check to see if user provided a path argument. If not, find that argument.
  if (path_arg == "") {
    path_arg <- takes_path_arg(func, pkg)
  }


  # Flag if there was more than one path argument
  if (length(path_arg) > 1) {
    rlang::abort(message = "The function you provided takes more than one path-related argument.
                 Please specify which one you would like fertile to track with 'path_arg = _'")
  }

  # Get list of required arguments

  to_eval <- paste0("formals(", pkg, "::", func, ")")
  args <- eval(parse(text = to_eval))

  required_args <- c()
  all_args <- c()

  for (arg in names(args)) {
    arg_to_eval <- paste0("args$", arg)
    arg_class <- class(eval(parse(text = arg_to_eval)))

    if (arg_class == "name" & arg != "...") {
      required_args <- required_args %>% append(arg)
    }

    all_args <- all_args %>% append(arg)
  }

  # Put required args and path args together

  required_arg_positions <- c()

  for (arg in required_args) {
    pos <- match(arg, all_args)
    required_arg_positions <- required_arg_positions %>% append(pos)
  }

  path_arg_position <- c()

  for (arg in path_arg) {
    pos <- match(arg, all_args)
    path_arg_position <- path_arg_position %>% append(pos)
  }

  args_to_include <- sort(unique(c(path_arg_position, required_arg_positions)))

  args_in_order <- all_args[args_to_include]



  # Write out function definition

  line1 <- paste0(func, " <- function(", paste(args_in_order, collapse = ", "), ", ...) {")
  line2 <- "   if (fertile::interactive_log_on()) {"
  line3 <- paste0("      fertile::log_push(", path_arg, ", '", pkg, "::", func, "')")
  line4 <- paste0("      fertile::check_path_safe(", path_arg, ", ... = '", pkg, "::", func, "')")
  line5 <- paste0("      ", pkg, "::", func, "(", paste(args_in_order, collapse = ", "), ", ...)")
  line6 <- "   }"
  line7 <- "}"


  func_lines <- c(
    line1,
    line2,
    line3,
    line4,
    line5,
    line6,
    line7
  )

  # Return function as vector of its lines
  return(func_lines)
}


#' Find the names of all functions that are potentially shimmable for a given package
#' @param package name of package to search through
#' @return vector containing the names of all the shimmable functions for the provided package
#' @export
#' @keywords internal

find_pkg_shimmable_functions <- function(package) {

  package_objects <- ls(paste0("package:", package))
  # if(package == "base"){
  #   package_objects <- package_objects[88:length(ls("package:base"))]
  # }

  shimmable_funcs <- c()
  for (obj in package_objects) {
    class_obj <- ""

    possible_error <- tryCatch(
      {
        class_obj <- class(utils::getFromNamespace(obj, package))[1]
      },
      error = function(e) {
        e
      }
    )

    if (!inherits(possible_error, "error", class_obj == "function")) {
      takes_path <- FALSE
      possible_error2 <- tryCatch(
        {
          takes_path <- takes_path_arg(obj, package)
        },
        error = function(e) {
          e
        }
      )

      if (!inherits(possible_error2, "error") & takes_path != FALSE) {
        shimmable_funcs <- shimmable_funcs %>% append(obj)
      }
    }
  }

  return(shimmable_funcs)
}



#' Find the names of all shimmable functions within the list of loaded packages
#' @return list containing the names of all the shimmable functions and their associated packages
#' @export
#' @keywords internal

find_all_shimmable_functions <- function() {
  search_path <- search()

  packages <- c()
  for (item in search_path) {
    if (grepl("package:", item) == TRUE & item != "package:datasets" & item != "package:fertile") {
      packages <- packages %>% append(substr(item, 9, nchar(item)))
    }
  }

  pkg_func_list <- list()
  for (pkg in packages) {

    suppressWarnings(shimmable_funcs <- find_pkg_shimmable_functions(pkg))
    pkg_func_list[[pkg]] <- shimmable_funcs
  }

  return(pkg_func_list)
}



#' Sub-function to help disable_added_shims operate: #1
#' @export
#' @keywords internal

is_function <- function(expr) {
  if (!is_assign(expr)) {
    return(FALSE)
  }
  value <- expr[[3]]
  is.call(value) && as.character(value[[1]]) == "function"
}


#' Sub-function to help disable_added_shims operate: #2
#' @export
#' @keywords internal

function_name <- function(expr) {
  as.character(expr[[2]])
}


#' Sub-function to help disable_added_shims operate: #3
#' @export
#' @keywords internal

is_assign <- function(expr) {
  is.call(expr) && as.character(expr[[1]]) %in% c("=", "<-", "assign")
}


#' Check that shims file exists and return path
#' @export
#' @rdname add_shim
#' @keywords internal

path_shims <- function() {

  # Get path to shim file
  x <- fs::path(Sys.getenv("HOME"), "fertile_shims.R")

  # If file exists, return the path
  # Otherwise create the file then return the path
  if (fs::file_exists(x)) {
    return(x)
  } else {
    fs::file_create(x)
    return(x)
  }
}

#' @rdname add_shim
#' @export


read_shims <- function() {
  x <- path_shims()

  # Get names of functions from inside the shims file
  file_code <- readLines(x)

  file_code %>%
    stringr::str_subset("fertile::log_push") %>%
    stringr::str_extract("'.+::.+'") %>%
    stringr::str_remove_all("'")
}

#' @rdname add_shim
#' @export

active_shims <- function() {

  text <- "get_active_shims_fertile <- function(){

  env_obj <- ls(.GlobalEnv)


  shims <- list()
  shims_names <- c()
  for (i in 1:length(env_obj)){

    # Pull object from environment
    object <- get(env_obj[i])

    # Check that it's a function
    obj_class <- class(object)[1]
    if (obj_class == 'function'){

      # Make sure that it came from fertile
      # If it did, add it to the shims list
      obj_attr <- attributes(object)
      if(!is.null(obj_attr$package) & !is.null(obj_attr$func_name)){
        if(obj_attr$package == 'fertile'){
          shims <- append(shims, object)
          shims_names <- append(shims_names, env_obj[i])
        }

      }
    }
  }


  shims_names

}"

  output_path <- fs::path(Sys.getenv("HOME"), "fertile_get_active_shims.R")
  base::writeLines(text, output_path)

  base::source(output_path)
  shims <- get_active_shims_fertile()
  rm("get_active_shims_fertile", envir = .GlobalEnv)

  shims

}
