
#' S3 Generic - Validate Format of S3 Object Instances
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
#' test <- validate(s3_obj, FALSE)
#' }
validate <- function(obj, ..., bool) {UseMethod("validate", obj)}

#' S3 Generic - Alter Internal State of S3 Object Instances
#'
#' @param obj S3 Object
#' @param ... R ellipsis
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' alter(s3_obj)
#' }
alter <- function(obj, ...) {UseMethod("alter", obj)}

#' S3 Generic - CRUD 'Read' Operation
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
contents <- function(obj, ...) {UseMethod("contents", obj)}

#' S3 Generic - CRUD 'Insert' Operation
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
insert <- function(obj, ...) {UseMethod("insert", obj)}

#' S3 Generic - CRUD 'Update' Operation
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
update <- function(obj, ...) {UseMethod("update", obj)}

#' S3 Generic - CRUD 'Delete' Operation
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
delete <- function(obj, ...) {UseMethod("delete", obj)}