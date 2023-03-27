
#' Generate Dummy Character Vectors for Package Testing
#' 
#' @importFrom stats runif
#' 
#' @param n numeric
#' @param min numeric
#' @param max numeric
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' output <- gen_test_char(n = 20, min = 5, max = 20)
#' }
gen_test_char <- function(n = 10, min = 5, max = 20) {
  
  # Validate Inputs 
  if (missing(n)) {stop("`n` is missing in call to `gen_test_char`")}
  if (missing(min)) {min <- 5}
  if (missing(max)) {max <- 20}
  
  # Validate Input Expectations 
  
  # * `n`
  if (!isTRUE(validate.numeric(obj = n, single = TRUE, check_int = TRUE, check_neg = TRUE))) {
    stop("`n` must be a single whole number in call to `gen_test_char`")
  }
  
  # * `min`
  if (!isTRUE(validate.numeric(obj = min, single = TRUE, check_int = TRUE, check_neg = TRUE))) {
    stop("`min` must be a single whole number in call to `gen_test_char`")
  }
  
  # * `max`
  if (!isTRUE(validate.numeric(obj = max, single = TRUE, check_int = TRUE, check_neg = TRUE))) {
    stop("`max` must be a single whole number in call to `gen_test_char`")
  }
  
  # Generate `n` random strings, with lengths varying between `min` and `max`
  rand_len <- round(runif(n = 1, min = min, max = max), digits = 0)
  n_val <- random::randomStrings(n = n, len = rand_len)
  
  # Return Random Column Names 
  return(n_val)
  
}

#' Generate Dummy Numeric Vectors for Package Testing
#' 
#' @importFrom stats runif
#' 
#' @param n numeric
#' @param min numeric
#' @param max numeric
#' @param as_int logical
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' output <- gen_test_numeric(n = 20, min = 5, max = 19)
#' }
gen_test_numeric <- function(n = 10, min = 2, max = 100, as_int = FALSE) {
  
  # Validate Inputs 
  if (missing(n)) {stop("`n` is missing in call to `gen_test_numeric`")}
  if (missing(min)) {min <- 1}
  if (missing(max)) {max <- 100}
  if (missing(as_int)) {as_int <- FALSE}
  
  # Validate Input Expectations 
  
  # * `n`
  if (!isTRUE(validate.numeric(obj = n, single = TRUE, check_int = TRUE, check_neg = TRUE))) {
    stop("`n` must be a single whole number in call to `gen_test_numeric`")
  }
  
  # * `min`
  if (!isTRUE(validate.numeric(obj = min, single = TRUE, check_int = TRUE, check_neg = TRUE))) {
    stop("`min` must be a single whole number in call to `gen_test_numeric`")
  }
  
  # * `max`
  if (!isTRUE(validate.numeric(obj = max, single = TRUE, check_int = TRUE, check_neg = TRUE))) {
    stop("`max` must be a single whole number in call to `gen_test_numeric`")
  }
  
  # * `as_int` 
  if (!isTRUE(validate.logical(obj = as_int, single = TRUE))) {
    stop("`as_int` must equal TRUE/FALSE in call to `gen_test_numeric`")
  }
  
  # Generate `n` random numerics
  n_val <- runif(n = n, min = min, max = max)
  
  # Conditionally Round Values 
  if (isTRUE(as_int)) {
    n_val <- round(n_val, digits = 0)
  }
  
  # Return Random Column Names 
  return(n_val)
  
}
