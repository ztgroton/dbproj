
#' S3 Method - Test Equality of S3 'dbproj_column' Instances
#'
#' @param obj S3 Object
#' @param x S3 Object 
#' @param ... R ellipsis
#'
#' @return logical
#'
#' @examples
#' \dontrun{
#' test <- equals(s3_obj, s3_obj)
#' }
equals.dbproj_column <- function(obj, x, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `insert.dbproj_column`")}
  if (missing(x)) {stop("`x` is missing in call to `insert.dbproj_column`")}
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_column'))) {
    stop("`obj` must inherit from 'dbproj_column' in call to `insert.dbproj_column`")
  }
  
  # * `x`
  if (!isTRUE(inherits(x, 'dbproj_column'))) {
    stop("`x` must inherit from 'dbproj_column' in call to `insert.dbproj_column`")
  }
  
  # Test for Equality
  name_equal <- isTRUE(identical(sort(obj$data$name), sort(x$data$name)))
  type_equal <- isTRUE(identical(sort(obj$data$type), sort(x$data$type)))
  
  # Return Equality
  is_equal <- name_equal && type_equal
  return(is_equal)
  
}

#' S3 Method - CRUD 'Read' Operation
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- contents(obj, ...)
#' }
contents.dbproj_column <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `contents.dbproj_column`")}
  
  # Prepare Contents if not Empty
  obj_contents <- list(name = obj$data$name, type = obj$data$type)
  
  # Return Contents
  return(obj_contents)
  
}

#' S3 Method - CRUD 'Update' Operation
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update(obj, ...)
#' }
update.dbproj_column <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `update.dbproj_column`")}
  
  # Validate Input Expectations
  
  # * `...`
  dot_args <- packageHandleDots(...)
  
  if (!isTRUE(all(names(dot_args) %in% c('name', 'type')))) {
    stop("`names(dot_args)` must be subset of c('name', 'type') in call to `update.dbproj_column`")
  }
  
  # Update Column Details 
  if (isTRUE('name' %in% names(dot_args))) {
    obj$data$name <- dot_args[['name']]
  }
  
  if (isTRUE('type' %in% names(dot_args))) {
    obj$data$type <- dot_args[['type']]
  }
  
}
