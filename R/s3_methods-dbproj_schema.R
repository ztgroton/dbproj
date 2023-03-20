
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
contents.dbproj_schema <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `contents.dbproj_schema`")}
  
  # Get Table Count 
  table_cnt <- length(obj$tables)
  
  # Return Empty List if Empty 
  if (isTRUE(table_cnt == 0)) {return(list())}
  
  # Prepare Contents if not Empty
  obj_contents <- purrr::map(obj$tables, function(t){contents(t)})
  
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
update.dbproj_schema <- function(obj, ...) {}

#' S3 Method - CRUD 'Delete' Operation
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' delete(obj, ...)
#' }
delete.dbproj_schema <- function(obj, ...) {}

#' S3 Method - Alter Internal State of S3 Object Instances
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' alter(obj, ...)
#' }
alter.dbproj_schema <- function(obj, ...) {}
