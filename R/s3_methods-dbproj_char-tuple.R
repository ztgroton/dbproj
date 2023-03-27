
#' S3 Method - Test Equality of S3 'dbproj_char_tuple' Instances
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
equals.dbproj_char_tuple <- function(obj, x, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `insert.dbproj_char_tuple`")}
  if (missing(x)) {stop("`x` is missing in call to `insert.dbproj_char_tuple`")}
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_char_tuple'))) {
    stop("`obj` must inherit from 'dbproj_char_tuple' in call to `insert.dbproj_char_tuple`")
  }
  
  # * `x`
  if (!isTRUE(inherits(x, 'dbproj_char_tuple'))) {
    stop("`x` must inherit from 'dbproj_char_tuple' in call to `insert.dbproj_char_tuple`")
  }
  
  # Test for Equality
  is_equal <- isTRUE(identical(sort(obj$data$tuple), sort(x$data$tuple)))
  
  # Return Equality
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
contents.dbproj_char_tuple <- function(obj, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `insert.dbproj_char_tuple`")}
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_char_tuple'))) {
    stop("`obj` must inherit from 'dbproj_char_tuple' in call to `insert.dbproj_char_tuple`")
  }
  
  # Return Contents
  return(obj$data$tuple)
  
}

#' S3 Method - CRUD 'Insert' Operation
#'
#' @param obj S3 Object
#' @param x character
#' @param in_place logical
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' insert(obj, x)
#' output <- insert(obj, x, in_place = FALSE)
#' }
insert.dbproj_char_tuple <- function(obj, x, in_place = TRUE, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `insert.dbproj_char_tuple`")}
  if (missing(in_place)) {in_place <- TRUE}
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_char_tuple'))) {
    stop("`obj` must inherit from 'dbproj_char_tuple' in call to `insert.dbproj_char_tuple`")
  }
  
  # * `x`
  if (!isTRUE(validate.character(obj = x, allow_empty = FALSE, allow_na = FALSE))) {
    stop("`x` must be non-empty character vector with no NA or NULL values in call to `insert.dbproj_char_tuple`")
  }
  
  # * `in_place`
  if (!isTRUE(validate.logical(obj = in_place, single = TRUE))) {
    stop("`in_place` must equal TRUE/FALSE in call to `insert.dbproj_char_tuple`")
  }
  
  # Insert Contents of `x` into `obj$data$tuple`
  obj$data$tuple <- unique(union(obj$data$tuple, x))
  
  # Conditionally Return Input `obj` (Standard R Style)
  if (isTRUE(in_place)) {
    invisible(TRUE)
  } else {
    invisible(obj)
  }
  
}

#' S3 Method - CRUD 'Update' Operation
#'
#' @param obj S3 Object
#' @param x character
#' @param in_place logical
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update(obj, x)
#' output <- update(obj, x, in_place = FALSE)
#' }
update.dbproj_char_tuple <- function(obj, x, in_place = FALSE, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `update.dbproj_char_tuple`")}
  if (missing(in_place)) {in_place <- TRUE}
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_char_tuple'))) {
    stop("`obj` must inherit from 'dbproj_char_tuple' in call to `update.dbproj_char_tuple`")
  }
  
  # * `x`
  if (!isTRUE(validate.character(obj = x, allow_empty = FALSE, allow_na = FALSE))) {
    stop("`x` must be non-empty character vector with no NA or NULL values in call to `update.dbproj_char_tuple`")
  }
  
  # * `in_place`
  if (!isTRUE(validate.logical(obj = in_place, single = TRUE))) {
    stop("`in_place` must equal TRUE/FALSE in call to `update.dbproj_char_tuple`")
  }
  
  # Update Contents of `obj$data$tuple`
  obj$data$tuple <- unique(x)
  
  # Conditionally Return Input `obj` (Standard R Style)
  if (isTRUE(in_place)) {
    invisible(TRUE)
  } else {
    invisible(obj)
  }
  
}

#' S3 Method - CRUD 'Delete' Operation
#'
#' @param obj S3 Object
#' @param x character
#' @param in_place logical
#' @param ... R ellipsis
#'
#' @export
#'
#' @examples
#' \dontrun{
#' delete(obj, x)
#' output <- delete(obj, x, in_place = FALSE)
#' }
delete.dbproj_char_tuple <- function(obj, x, in_place = FALSE, ...) {
  
  # Validate Inputs 
  if (missing(obj)) {stop("`obj` is missing in call to `delete.dbproj_char_tuple`")}
  if (missing(in_place)) {in_place <- TRUE}
  
  # Validate Input Expectations
  
  # * `obj`
  if (!isTRUE(inherits(obj, 'dbproj_char_tuple'))) {
    stop("`obj` must inherit from 'dbproj_char_tuple' in call to `delete.dbproj_char_tuple`")
  }
  
  # * `x`
  if (!isTRUE(validate.character(obj = x, allow_empty = FALSE, allow_na = FALSE))) {
    stop("`x` must be non-empty character vector with no NA or NULL values in call to `delete.dbproj_char_tuple`")
  }
  
  # * `in_place`
  if (!isTRUE(validate.logical(obj = in_place, single = TRUE))) {
    stop("`in_place` must equal TRUE/FALSE in call to `delete.dbproj_char_tuple`")
  }
  
  # Update Contents of `obj$data$tuple`
  obj$data$tuple <- unique(setdiff(obj$data$tuple, x))
  
  # Conditionally Return Input `obj` (Standard R Style)
  if (isTRUE(in_place)) {
    invisible(TRUE)
  } else {
    invisible(obj)
  }
  
}
