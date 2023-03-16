
#' S3 Constructor for Class 'db_schema'
#'
#' @param x character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_db_schema(raw_data_frame)
#' }
new_db_schema <- function(x) {
  
  # Validate Inputs
  if (missing(x)) {stop("`x` is missing in call to `new_db_schema`")}
  
  # Initialize Empty S3 Object
  rs <- list(data = new.env())
  
  # Initialize `schema`
  rs$data$name <- x
  rs$data$tables <- list()
  
  # Set Class
  class(rs) <- c(setdiff('db_schema', class(rs)), class(rs))
  
  # Return S3 Object
  return(rs)
  
}

#' S3 Validator for Class 'db_schema'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate_db_schema(s3_obj, FALSE)
#' }
validate_db_schema <- function(obj, bool) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_db_schema`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'db_schema'))) {
    msg <- "`obj` must inherit from 'db_schema'"
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

#' S3 Helper Function for Class 'db_schema'
#'
#' @param x data.frame
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- db_schema(IMF_DATA)
#' }
db_schema <- function(x) {
  
  # Validate Inputs
  if (missing(x)) {stop("`x` is missing in call to `db_schema`")}
  
  validate_db_schema(new_db_schema(x))
  
}
