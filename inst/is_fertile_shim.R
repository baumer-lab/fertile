# Code for is_fertile_shim(), used by active_shims()

is_fertile_shim <- function(i, env_obj = ls(.GlobalEnv)) {

  # pull object from environment
  object <- get(env_obj[i])

  # Check that it's a function
  obj_class <- class(object)[1]
  if (obj_class == "function") {

    # Make sure that it came from fertile
    # If it did, add it to the shims list
    obj_attr <- attributes(object)
    if (!is.null(obj_attr$package) & !is.null(obj_attr$func_name)) {
      if (obj_attr$package == "fertile") {
        env_obj[i]
      }
    }
  }
}
