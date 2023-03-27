
#' S3 Constructor for Class 'dbproj_char_tuple'
#'
#' @param x character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' output <- new_dbproj_char_tuple(x)
#' }
new_dbproj_char_tuple <- function(x) {
  
  # Validate Inputs
  if (missing(x)) {stop("`x` is missing in call to `new_dbproj_char_tuple`")}
  
  # Initialize Empty S3 Object
  rs <- list(data = new.env())
  
  # Initialize `column`
  rs$data$tuple <- x
  
  # Set Class
  class(rs) <- c('dbproj_char_tuple', class(rs))
  
  # Lock `rs$data`
  rlang::env_lock(rs$data)
  
  # Return S3 Object
  return(rs)
  
}

#' S3 Validator for Class 'dbproj_char_tuple'
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
#' test <- validate.dbproj_char_tuple(s3_obj, bool = TRUE)
#' }
validate.dbproj_char_tuple <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.dbproj_char_tuple`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_char_tuple'))) {
    msg <- "`obj` must inherit from 'dbproj_char_tuple'"
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
  
  # * `tuple`
  if (!isTRUE(validate.character(obj = obj$data$tuple, allow_empty = FALSE, allow_na = FALSE))) {
    msg <- "`obj$data$tuple` must be a non-empty character vector with no NA or NULL values"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    
    if (isTRUE(length(err) == 0)) {
      return(obj)
    } else {
      warning("ERROR - `validate.dbproj_char_tuple`")
      return(err)
    }
    
  } else {
    
    if (isTRUE(length(err) == 0)) {
      return(TRUE)
    } else {
      warning("ERROR - `validate.dbproj_char_tuple`")
      return(FALSE)
    }
    
  }
  
}

#' S3 Helper Function for Class 'char_tuple'
#'
#' @param x character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' output <- char_tuple(c('a', 'b', 'c'))
#' }
char_tuple <- function(x) {
  
  # Validate Inputs
  if (missing(x)) {stop("`x` is missing in call to `dbproj::char_tuple`")}
  
  validate(new_dbproj_char_tuple(x))
  
} 
