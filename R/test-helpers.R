
expect_s3_semantics <- function(object, n) {
  
  # Use R Quasi-Quotation to capture Actual Value (act$val) and Label (act$lab)
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  
  act$n <- length(act$val)
  
  if (act$n == n) {
    succeed()
    return(invisible(act$val))
  }
  
  message <- sprintf("%s has length %i, not length %i.", act$lab, act$n, n)
  fail(message)
}
