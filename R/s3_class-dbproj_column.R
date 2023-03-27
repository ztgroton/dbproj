
#' S3 Constructor for Class 'dbproj_column'
#'
#' @param name character
#' @param type character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_dbproj_column(raw_data_frame)
#' }
new_dbproj_column <- function(name, type) {
  
  # Validate Inputs
  if (missing(name)) {stop("`name` is missing in call to `new_dbproj_column`")}
  if (missing(type)) {stop("`type` is missing in call to `new_dbproj_column`")}
  
  # Initialize Empty S3 Object
  rs <- list(data = new.env())
  
  # Initialize `column`
  rs$data$name <- name
  rs$data$type <- type
  
  # Set Class
  class(rs) <- c('dbproj_column', class(rs))
  
  # Lock `rs$data`
  rlang::env_lock(rs$data)
  
  # Return S3 Object
  return(rs)
  
}

#' S3 Validator for Class 'dbproj_column'
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
#' test <- validate.dbproj_column(s3_obj, bool = TRUE)
#' }
validate.dbproj_column <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.dbproj_column`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_column'))) {
    msg <- "`obj` must inherit from 'dbproj_column'"
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
  if (!isTRUE(validate.character(obj = obj$data$name, single = TRUE))) {
    msg <- "`name` must be a length 1 character"
    err <- c(msg, err)
  }
  
  # * `type`
  if (!isTRUE(validate.character(obj = obj$data$type, single = TRUE))) {
    msg <- "`type` must be a length 1 character"
    err <- c(msg, err)
  }
  
  valid_types <- c('integer', 'numeric', 'character', 'logical', 'list', 'POSIXct', 'Date')
  is_valid <- isTRUE(obj$data$type %in% valid_types)
  if (!isTRUE(is_valid)) {
    msg <- "`type` must be valid value"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    
    if (isTRUE(length(err) == 0)) {
      return(obj)
    } else {
      warning("ERROR - `validate.numeric`")
      return(err)
    }
    
  } else {
    
    if (isTRUE(length(err) == 0)) {
      return(TRUE)
    } else {
      warning("ERROR - `validate.numeric`")
      return(FALSE)
    }
    
  }
  
}

#' S3 Helper Function for Class 'column'
#'
#' @param name character
#' @param type character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- column('testcol', 'character')
#' }
column <- function(name, type) {
  
  # Validate Inputs
  if (missing(name)) {stop("`name` is missing in call to `dbproj::column`")}
  if (missing(type)) {stop("`type` is missing in call to `dbproj::column`")}
  
  validate(new_dbproj_column(name, type))
  
} 
