
#' S3 Validator for Class 'character'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.character(s3_obj)
#' }
validate.character <- function(obj, ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.character`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list'"
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
  if (isTRUE(length(err) == 0)) {return(TRUE)} 
  else {return(FALSE)}
  
}

#' S3 Validator for Class 'numeric'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.numeric(s3_obj)
#' }
validate.numeric <- function(obj, ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.numeric`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list'"
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
  if (isTRUE(length(err) == 0)) {return(TRUE)} 
  else {return(FALSE)}
  
}

#' S3 Validator for Class 'integer'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.integer(s3_obj)
#' }
validate.integer <- function(obj, ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.integer`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list'"
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
  if (isTRUE(length(err) == 0)) {return(TRUE)} 
  else {return(FALSE)}
  
}

#' S3 Validator for Class 'logical'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.logical(s3_obj)
#' }
validate.logical <- function(obj, ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.logical`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list'"
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
  if (isTRUE(length(err) == 0)) {return(TRUE)} 
  else {return(FALSE)}
  
}

#' S3 Validator for Class 'list'
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate.list(s3_obj)
#' }
validate.list <- function(obj, ...) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.list`")}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'list'))) {
    msg <- "`obj` must inherit from 'list'"
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
  if (isTRUE(length(err) == 0)) {return(TRUE)} 
  else {return(FALSE)}
  
}
