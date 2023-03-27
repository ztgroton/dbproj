
#' S3 Validator for Class 'logical'
#'
#' @param obj S3 Object
#' @param allow_empty logical 
#' @param allow_na logical 
#' @param single logical 
#' @param check_names logical
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.logical(s3_obj)
#' }
validate.logical <- function(obj, 
                             allow_empty = FALSE, 
                             allow_na = FALSE, 
                             single = FALSE, 
                             check_names = FALSE, 
                             ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.logical`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'logical'))) {
    msg <- "`obj` must equal TRUE/FALSE in call to `validate.logical`"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `allow_empty`
  if (!isTRUE(identical(allow_empty, TRUE)) && !isTRUE(identical(allow_empty, FALSE))) {
    stop("`allow_empty` must be identical to TRUE/FALSE in call to `validate.logical`")
  }
  
  if (isTRUE(allow_empty)) {
    is_empty <- isTRUE(length(obj) > 0)
    if (!isTRUE(is_empty)) {
      msg <- "`obj` must have non-zero length in call to `validate.logical`"
      err <- c(msg, err)
    }
  }
  
  # * `allow_na`
  if (!isTRUE(identical(allow_na, TRUE)) && !isTRUE(identical(allow_na, FALSE))) {
    stop("`allow_na` must be identical to TRUE/FALSE in call to `validate.logical`")
  }
  
  if (!isTRUE(allow_na)) {
    is_allow_na <- !isTRUE(any(purrr::map_lgl(obj, ~ is.null(.) || is.na(.))))
    if (!isTRUE(is_allow_na)) {
      msg <- "`obj` must not contain any NA or NULL values in call to `validate.logical`"
      err <- c(msg, err)
    }
  } 
  
  # * `single`
  if (!isTRUE(identical(single, TRUE)) && !isTRUE(identical(single, FALSE))) {
    stop("`single` must be identical to TRUE/FALSE in call to `validate.logical`")
  }
  
  if (isTRUE(single)) {
    is_single <- isTRUE(length(obj) == 1)
    if (!isTRUE(is_single)) {
      msg <- "`obj` must be length 1 in call to `validate.logical`"
      err <- c(msg, err)
    }
  }
  
  # * `check_names`
  if (!isTRUE(identical(check_names, TRUE)) && !isTRUE(identical(check_names, FALSE))) {
    stop("`check_names` must be identical to TRUE/FALSE in call to `validate.logical`")
  }
  
  if (isTRUE(check_names)) {
    validate.character(
      obj = names(obj), 
      allow_empty = FALSE, 
      allow_na = FALSE, 
      single = FALSE, 
      check_names = FALSE
    )
  }
  
  # Final Output
  if (isTRUE(length(err) == 0)) {
    return(TRUE)
  } 
  else {
    warning("ERROR - `validate.logical`")
    return(err)
  }
  
}

#' S3 Validator for Class 'character'
#'
#' @param obj S3 Object
#' @param allow_empty logical 
#' @param allow_na logical 
#' @param single logical 
#' @param check_names logical 
#' @param ... R ellipsis 
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.character(s3_obj)
#' }
validate.character <- function(obj, 
                               allow_empty = FALSE, 
                               allow_na = FALSE, 
                               single = FALSE, 
                               check_names = FALSE, 
                               ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.logical`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'character'))) {
    msg <- "`obj` must inherit from 'character' in call to `validate.logical`"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `allow_empty`
  if (!isTRUE(validate.logical(allow_empty, single = TRUE))) {
    stop("`allow_empty` must be identical to TRUE/FALSE in call to `validate.character`")
  }
  
  if (isTRUE(allow_empty)) {
    is_empty <- isTRUE(length(obj) > 0)
    if (!isTRUE(is_empty)) {
      msg <- "`obj` must have non-zero length in call to `validate.character`"
      err <- c(msg, err)
    }
  }
  
  # * `allow_na`
  if (!isTRUE(validate.logical(allow_na, single = TRUE))) {
    stop("`allow_na` must be identical to TRUE/FALSE in call to `validate.character`")
  }
  
  if (!isTRUE(allow_na)) {
    is_allow_na <- !isTRUE(any(purrr::map_lgl(obj, ~ is.null(.) || is.na(.))))
    if (!isTRUE(is_allow_na)) {
      msg <- "`obj` must not contain any NA or NULL values in call to `validate.character`"
      err <- c(msg, err)
    }
  } 
  
  # * `single`
  if (!isTRUE(validate.logical(single, single = TRUE))) {
    stop("`single` must be identical to TRUE/FALSE in call to `validate.character`")
  }
  
  if (isTRUE(single)) {
    is_single <- isTRUE(length(obj) == 1)
    if (!isTRUE(is_single)) {
      msg <- "`obj` must be length 1 in call to `validate.character`"
      err <- c(msg, err)
    }
  }
  
  # * `check_names`
  if (!isTRUE(validate.logical(check_names, single = TRUE))) {
    stop("`check_names` must be identical to TRUE/FALSE in call to `validate.character`")
  }
  
  if (isTRUE(check_names)) {
    validate.character(
      obj = names(obj), 
      allow_empty = FALSE, 
      allow_na = FALSE, 
      single = FALSE, 
      check_names = FALSE
    )
  }
  
  # Final Output
  if (isTRUE(length(err) == 0)) {
    return(TRUE)
  } 
  else {
    warning("ERROR - `validate.character`")
    return(err)
  }
  
}

#' S3 Validator for Class 'numeric'
#'
#' @param obj S3 Object
#' @param allow_empty logical 
#' @param allow_na logical 
#' @param single logical 
#' @param check_names logical 
#' @param check_int logical
#' @param ... R ellipsis 
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.numeric(s3_obj)
#' }
validate.numeric <- function(obj, 
                             allow_empty = FALSE, 
                             allow_na = FALSE, 
                             single = FALSE, 
                             check_names = FALSE, 
                             check_int = FALSE,
                             ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.numeric`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'numeric'))) {
    msg <- "`obj` must inherit from 'numeric' in call to `validate.numeric`"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `allow_empty`
  if (!isTRUE(validate.logical(allow_empty, single = TRUE))) {
    stop("`allow_empty` must be identical to TRUE/FALSE in call to `validate.numeric`")
  }
  
  if (isTRUE(allow_empty)) {
    is_empty <- isTRUE(length(obj) > 0)
    if (!isTRUE(is_empty)) {
      msg <- "`obj` must have non-zero length in call to `validate.numeric`"
      err <- c(msg, err)
    }
  }
  
  # * `allow_na`
  if (!isTRUE(validate.logical(allow_na, single = TRUE))) {
    stop("`allow_na` must be identical to TRUE/FALSE in call to `validate.numeric`")
  }
  
  if (!isTRUE(allow_na)) {
    is_allow_na <- !isTRUE(any(purrr::map_lgl(obj, ~ is.null(.) || is.na(.))))
    if (!isTRUE(is_allow_na)) {
      msg <- "`obj` must not contain any NA or NULL values in call to `validate.numeric`"
      err <- c(msg, err)
    }
  } 
  
  # * `single`
  if (!isTRUE(validate.logical(single, single = TRUE))) {
    stop("`single` must be identical to TRUE/FALSE in call to `validate.numeric`")
  }
  
  if (isTRUE(single)) {
    is_single <- isTRUE(length(obj) == 1)
    if (!isTRUE(is_single)) {
      msg <- "`obj` must be length 1 in call to `validate.numeric`"
      err <- c(msg, err)
    }
  }
  
  # * `check_names`
  if (!isTRUE(validate.logical(check_names, single = TRUE))) {
    stop("`check_names` must be identical to TRUE/FALSE in call to `validate.numeric`")
  }
  
  if (isTRUE(check_names)) {
    validate.character(
      obj = names(obj), 
      allow_empty = FALSE, 
      allow_na = FALSE, 
      single = FALSE, 
      check_names = FALSE
    )
  }
  
  # * `check_int`
  if (!isTRUE(validate.logical(check_int, single = TRUE))) {
    stop("`check_int` must be identical to TRUE/FALSE in call to `validate.numeric`")
  }
  
  if (isTRUE(check_int)) {
    all_whole <- all(obj %% 1 == 0)
    if (!isTRUE(all_whole)) {
      msg <- "`obj` must only contain 'whole' integers in call to `validate.numeric`"
      err <- c(msg, err)
    }
  }
  
  # Final Output
  if (isTRUE(length(err) == 0)) {
    return(TRUE)
  } 
  else {
    warning("ERROR - `validate.numeric`")
    return(err)
  }
  
}

#' S3 Validator for Class 'integer'
#'
#' @param obj S3 Object
#' @param allow_empty logical 
#' @param allow_na logical 
#' @param single logical 
#' @param check_names logical 
#' @param ... R ellipsis 
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.integer(s3_obj)
#' }
validate.integer <- function(obj, 
                             allow_empty = FALSE, 
                             allow_na = FALSE, 
                             single = FALSE, 
                             check_names = FALSE, 
                             ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.integer`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'integer'))) {
    msg <- "`obj` must inherit from 'integer' in call to `validate.integer`"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `allow_empty`
  if (!isTRUE(validate.logical(allow_empty, single = TRUE))) {
    stop("`allow_empty` must be identical to TRUE/FALSE in call to `validate.integer`")
  }
  
  if (isTRUE(allow_empty)) {
    is_empty <- isTRUE(length(obj) > 0)
    if (!isTRUE(is_empty)) {
      msg <- "`obj` must have non-zero length in call to `validate.integer`"
      err <- c(msg, err)
    }
  }
  
  # * `allow_na`
  if (!isTRUE(validate.logical(allow_na, single = TRUE))) {
    stop("`allow_na` must be identical to TRUE/FALSE in call to `validate.integer`")
  }
  
  if (!isTRUE(allow_na)) {
    is_allow_na <- !isTRUE(any(purrr::map_lgl(obj, ~ is.null(.) || is.na(.))))
    if (!isTRUE(is_allow_na)) {
      msg <- "`obj` must not contain any NA or NULL values in call to `validate.integer`"
      err <- c(msg, err)
    }
  } 
  
  # * `single`
  if (!isTRUE(validate.logical(single, single = TRUE))) {
    stop("`single` must be identical to TRUE/FALSE in call to `validate.integer`")
  }
  
  if (isTRUE(single)) {
    is_single <- isTRUE(length(obj) == 1)
    if (!isTRUE(is_single)) {
      msg <- "`obj` must be length 1 in call to `validate.integer`"
      err <- c(msg, err)
    }
  }
  
  # * `check_names`
  if (!isTRUE(validate.logical(check_names, single = TRUE))) {
    stop("`check_names` must be identical to TRUE/FALSE in call to `validate.integer`")
  }
  
  if (isTRUE(check_names)) {
    validate.character(
      obj = names(obj), 
      allow_empty = FALSE, 
      allow_na = FALSE, 
      single = FALSE, 
      check_names = FALSE
    )
  }
  
  # Final Output
  if (isTRUE(length(err) == 0)) {
    return(TRUE)
  } 
  else {
    warning("ERROR - `validate.integer`")
    return(err)
  }
  
}

#' S3 Validator for Class 'list'
#'
#' @param obj S3 Object
#' @param allow_empty logical 
#' @param allow_na logical 
#' @param single logical 
#' @param check_names logical 
#' @param ... R ellipsis 
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.list(s3_obj)
#' }
validate.list <- function(obj, 
                          allow_empty = FALSE, 
                          allow_na = FALSE, 
                          single = FALSE, 
                          check_names = FALSE, 
                          ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.list`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list' in call to `validate.list`"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `allow_empty`
  if (!isTRUE(validate.logical(allow_empty, single = TRUE))) {
    stop("`allow_empty` must be identical to TRUE/FALSE in call to `validate.list`")
  }
  
  if (isTRUE(allow_empty)) {
    is_empty <- isTRUE(length(obj) > 0)
    if (!isTRUE(is_empty)) {
      msg <- "`obj` must have non-zero length in call to `validate.list`"
      err <- c(msg, err)
    }
  }
  
  # * `allow_na`
  if (!isTRUE(validate.logical(allow_na, single = TRUE))) {
    stop("`allow_na` must be identical to TRUE/FALSE in call to `validate.list`")
  }
  
  if (!isTRUE(allow_na)) {
    is_allow_na <- !isTRUE(any(purrr::map_lgl(obj, ~ is.null(.) || is.na(.))))
    if (!isTRUE(is_allow_na)) {
      msg <- "`obj` must not contain any NA or NULL values in call to `validate.list`"
      err <- c(msg, err)
    }
  } 
  
  # * `single`
  if (!isTRUE(validate.logical(single, single = TRUE))) {
    stop("`single` must be identical to TRUE/FALSE in call to `validate.list`")
  }
  
  if (isTRUE(single)) {
    is_single <- isTRUE(length(obj) == 1)
    if (!isTRUE(is_single)) {
      msg <- "`obj` must be length 1 in call to `validate.list`"
      err <- c(msg, err)
    }
  }
  
  # * `check_names`
  if (!isTRUE(validate.logical(check_names, single = TRUE))) {
    stop("`check_names` must be identical to TRUE/FALSE in call to `validate.list`")
  }
  
  if (isTRUE(check_names)) {
    validate.character(
      obj = names(obj), 
      allow_empty = FALSE, 
      allow_na = FALSE, 
      single = FALSE, 
      check_names = FALSE
    )
  }
  
  # Final Output
  if (isTRUE(length(err) == 0)) {
    return(TRUE)
  } 
  else {
    warning("ERROR - `validate.list`")
    return(err)
  }
  
}
