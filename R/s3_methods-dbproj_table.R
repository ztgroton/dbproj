
#' S3 Method - Test Equality of S3 'dbproj_table' Instances
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#'
#' @examples
#' \dontrun{
#' test <- equals(s3_obj, FALSE)
#' }
equals.dbproj_table <- function(obj, ...) {}

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
contents.dbproj_table <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `contents.dbproj_table`")}
  
  # Get Column Count 
  column_cnt <- length(obj$columns)
  
  # Return Empty List if Empty 
  if (isTRUE(column_cnt == 0)) {return(list())}
  
  # Prepare Contents if not Empty
  obj_contents <- purrr::map(obj$columns, function(t){contents(t)})
  
  # Return Contents
  return(obj_contents)
  
}

#' S3 Method - CRUD 'Insert' Operation
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' insert(obj, ...)
#' }
insert.dbproj_table <- function(obj, ...) {}

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
update.dbproj_table <- function(obj, ...) {}

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
delete.dbproj_table <- function(obj, ...) {}

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
alter.dbproj_table <- function(obj, ...) {}
