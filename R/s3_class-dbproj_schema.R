
#' S3 Constructor for Class 'dbproj_schema'
#'
#' @param name character
#' @param tables list
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_dbproj_schema(raw_data_frame)
#' }
new_dbproj_schema <- function(name, tables) {
  
  # Validate Inputs
  if (missing(name)) {stop("`name` is missing in call to `new_dbproj_schema`")}
  if (missing(tables)) {tables <- list()}
  
  # Initialize Empty S3 Object
  rs <- list(data = new.env())
  
  # Initialize `schema`
  rs$data$name <- name
  rs$data$tables <- tables
  
  # Set Class
  class(rs) <- c('dbproj_schema', class(rs))
  
  # Return S3 Object
  return(rs)
  
}

#' S3 Validator for Class 'dbproj_schema'
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
#' test <- validate.dbproj_schema(s3_obj, bool = TRUE)
#' }
validate.dbproj_schema <- function(obj, ..., bool = FALSE) {
  
  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate.dbproj_schema`")}
  if (missing(bool)) {bool <- FALSE}
  
  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_schema'))) {
    msg <- "`obj` must inherit from 'dbproj_schema'"
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
  
  # * `tables`
  if (isTRUE(length(obj$tables) == 0) && isTRUE(is.list(obj$tables))) {
    is_valid <- TRUE
  } else {
    is_valid <- purrr::map_lgl(obj$tables, function(t) {
      isTRUE(validate.dbproj_table(t, TRUE))
    })
  }
  
  if (!isTRUE(all(is_valid))) {
    msg <- "`tables` must only contain S3 Objects of class 'dbproj_table'"
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

#' S3 Helper Function for Class 'schema'
#'
#' @param name data.frame
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- schema(IMF_DATA)
#' }
schema <- function(name) {
  
  # Validate Inputs
  if (missing(name)) {stop("`name` is missing in call to `dbproj::schema`")}
  
  validate(new_dbproj_schema(name))
  
}
