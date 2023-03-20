
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
contents.dbproj_db <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `contents.dbproj_db`")}
  
  # Get Schema Count 
  schema_cnt <- length(obj$schema)
  
  # Return Empty List if Empty 
  if (isTRUE(schema_cnt == 0)) {return(list())}
  
  # Prepare Contents if not Empty
  obj_contents <- purrr::map(obj$schema, function(t){contents(t)})
  
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
insert.dbproj_db <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `insert.dbproj_db`")}
  
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
update.dbproj_db <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `update.dbproj_db`")}
  
}

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
delete.dbproj_db <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `delete.dbproj_db`")}
  
}

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
alter.dbproj_db <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `alter.dbproj_db`")}
  
}
