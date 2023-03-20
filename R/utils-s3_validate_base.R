
#' S3 Validator for Class 'character'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.character(s3_obj, bool = TRUE)
#' }
validate.character <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.character`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'character'))) {
    msg <- "`obj` must inherit from 'character'"
    err <- c(msg, err)
  }
  
  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `name`
  obj_name <- obj$name
  is_char <- isTRUE(is.character(obj_name))
  is_len1 <- isTRUE(length(obj_name) == 1)
  is_not_blank <- !isTRUE(any(is.na(obj_name))) && !isTRUE(any(is.null(obj_name)))
  
  if (!isTRUE(all(is_char, is_len1, is_not_blank))) {
    msg <- "`name` must be non-empty length 1 character string"
    err <- c(msg, err)
  }
  
  # * `type`
  is_valid <- isTRUE(obj$type %in% c('integer', 'numeric', 'text', 'logical', 'list'))
  if (!isTRUE(all(is_valid))) {
    msg <- "`type` must be valid value"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    if (isTRUE(length(err) == 0)) {return(obj)} 
    else {return(err)}
  } 
  else {
    if (isTRUE(length(err) == 0)) {return(TRUE)} 
    else {return(FALSE)}
  }
  
}

#' S3 Validator for Class 'numeric'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.numeric(s3_obj, bool = TRUE)
#' }
validate.numeric <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.numeric`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'numeric'))) {
    msg <- "`obj` must inherit from 'numeric'"
    err <- c(msg, err)
  }
  
  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `name`
  obj_name <- obj$name
  is_char <- isTRUE(is.character(obj_name))
  is_len1 <- isTRUE(length(obj_name) == 1)
  is_not_blank <- !isTRUE(any(is.na(obj_name))) && !isTRUE(any(is.null(obj_name)))
  
  if (!isTRUE(all(is_char, is_len1, is_not_blank))) {
    msg <- "`name` must be non-empty length 1 character string"
    err <- c(msg, err)
  }
  
  # * `type`
  is_valid <- isTRUE(obj$type %in% c('integer', 'numeric', 'text', 'logical', 'list'))
  if (!isTRUE(all(is_valid))) {
    msg <- "`type` must be valid value"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    if (isTRUE(length(err) == 0)) {return(obj)} 
    else {return(err)}
  } 
  else {
    if (isTRUE(length(err) == 0)) {return(TRUE)} 
    else {return(FALSE)}
  }
  
}

#' S3 Validator for Class 'integer'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.integer(s3_obj, bool = TRUE)
#' }
validate.integer <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.integer`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'integer'))) {
    msg <- "`obj` must inherit from 'integer'"
    err <- c(msg, err)
  }
  
  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `name`
  obj_name <- obj$name
  is_char <- isTRUE(is.character(obj_name))
  is_len1 <- isTRUE(length(obj_name) == 1)
  is_not_blank <- !isTRUE(any(is.na(obj_name))) && !isTRUE(any(is.null(obj_name)))
  
  if (!isTRUE(all(is_char, is_len1, is_not_blank))) {
    msg <- "`name` must be non-empty length 1 character string"
    err <- c(msg, err)
  }
  
  # * `type`
  is_valid <- isTRUE(obj$type %in% c('integer', 'numeric', 'text', 'logical', 'list'))
  if (!isTRUE(all(is_valid))) {
    msg <- "`type` must be valid value"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    if (isTRUE(length(err) == 0)) {return(obj)} 
    else {return(err)}
  } 
  else {
    if (isTRUE(length(err) == 0)) {return(TRUE)} 
    else {return(FALSE)}
  }
  
}

#' S3 Validator for Class 'logical'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.logical(s3_obj, bool = TRUE)
#' }
validate.logical <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.logical`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'logical'))) {
    msg <- "`obj` must inherit from 'logical'"
    err <- c(msg, err)
  }
  
  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `name`
  obj_name <- obj$name
  is_char <- isTRUE(is.character(obj_name))
  is_len1 <- isTRUE(length(obj_name) == 1)
  is_not_blank <- !isTRUE(any(is.na(obj_name))) && !isTRUE(any(is.null(obj_name)))
  
  if (!isTRUE(all(is_char, is_len1, is_not_blank))) {
    msg <- "`name` must be non-empty length 1 character string"
    err <- c(msg, err)
  }
  
  # * `type`
  is_valid <- isTRUE(obj$type %in% c('integer', 'numeric', 'text', 'logical', 'list'))
  if (!isTRUE(all(is_valid))) {
    msg <- "`type` must be valid value"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    if (isTRUE(length(err) == 0)) {return(obj)} 
    else {return(err)}
  } 
  else {
    if (isTRUE(length(err) == 0)) {return(TRUE)} 
    else {return(FALSE)}
  }
  
}

#' S3 Validator for Class 'list'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.list(s3_obj, bool = TRUE)
#' }
validate.list <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.list`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list'"
    err <- c(msg, err)
  }
  
  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }
  
  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}
  
  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)
  
  # * `name`
  obj_name <- obj$name
  is_char <- isTRUE(is.character(obj_name))
  is_len1 <- isTRUE(length(obj_name) == 1)
  is_not_blank <- !isTRUE(any(is.na(obj_name))) && !isTRUE(any(is.null(obj_name)))
  
  if (!isTRUE(all(is_char, is_len1, is_not_blank))) {
    msg <- "`name` must be non-empty length 1 character string"
    err <- c(msg, err)
  }
  
  # * `type`
  is_valid <- isTRUE(obj$type %in% c('integer', 'numeric', 'text', 'logical', 'list'))
  if (!isTRUE(all(is_valid))) {
    msg <- "`type` must be valid value"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    if (isTRUE(length(err) == 0)) {return(obj)} 
    else {return(err)}
  } 
  else {
    if (isTRUE(length(err) == 0)) {return(TRUE)} 
    else {return(FALSE)}
  }
  
}
