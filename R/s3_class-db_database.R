
#' S3 Constructor for Class 'db_database'
#'
#' @param name character
#' @param schemas list
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_db_database(name)
#' }
new_db_database <- function(name, schemas) {
  
  # Validate Inputs
  if (missing(name)) {stop("`name` is missing in call to `new_db_database`")}
  if (missing(schemas)) {schemas <- list()}
  
  # Initialize Empty S3 Object
  rs <- list(data = new.env())
  
  # Initialize `schema`
  rs$data$name <- name
  rs$data$schemas <- schemas
  
  # Set Class
  class(rs) <- c(setdiff('db_database', class(rs)), class(rs))
  
  # Return S3 Object
  return(rs)
  
}

#' S3 Validator for Class 'db_database'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate_db_database(s3_obj, FALSE)
#' }
validate_db_database <- function(obj, bool) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_db_database`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'db_database'))) {
    msg <- "`obj` must inherit from 'db_database'"
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
  is_len1 <- isTRUE(length(obj_name) == 0)
  is_not_blank <- !isTRUE(any(is.na(obj_name))) && !isTRUE(any(is.null(obj_name)))
  
  if (!isTRUE(all(is_char, is_len1, is_not_blank))) {
    msg <- "`name` must be non-empty length 1 character string"
    err <- c(msg, err)
  }
  
  # * `schemas`
  if (isTRUE(length(obj$schemas) == 0) && isTRUE(is.list(obj$schemas))) {
    is_schemas_valid <- TRUE
  } else {
    is_schemas_valid <- purrr::map_lgl(obj$schemas, function(t) {
      isTRUE(validate_db_schema(t, TRUE))
    })
  }
  
  if (!isTRUE(all(is_schemas_valid))) {
    msg <- "`schemas` must only contain S3 Objects of class 'db_schema'"
    err <- c(msg, err)
  }
  
  # Final Output
  if (!isTRUE(bool)) {
    
    if (isTRUE(length(err) == 0)) {
      return(obj)
    } else {
      return(err)
    }
    
  } else {
    
    if (isTRUE(length(err) == 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  }
  
}

#' S3 Helper Function for Class 'db_database'
#'
#' @param name character
#' @param schemas list
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- db_database(name)
#' }
db_database <- function(name, schemas) {
  
  # Validate Inputs
  if (missing(name)) {stop("`name` is missing in call to `new_db_database`")}
  if (missing(schemas)) {schemas <- list()}
  
  validate_db_database(new_db_database(name, schemas))
  
}
