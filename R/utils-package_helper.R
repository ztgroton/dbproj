
#' List Content Names and Types for Package 'dbproj' Namespace
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' output <- packageNameSpace()
#' }
packageNameSpace <- function() {
  
  # Get Names of Namespace Contents
  dbproj_names <- ls(getNamespace('dbproj'))
  
  # Get Types for Namespace Contents
  dbproj_types <- purrr::map_chr(
    dbproj_names, function(t){
      class(getNamespace('dbproj')[[t]])
    }
  )
  
  # Store Result as DataFrame
  result <- data.frame(
    name = dbproj_names, 
    type = dbproj_types, 
    stringsAsFactors = FALSE
  )
  
  # Return Result
  return(result)
  
} 

#' List Function Names and Parameters for Package 'dbproj'
#' 
#' @importFrom rlang .data
#'
#' @return character
#'
#' @examples
#' \dontrun{
#' output <- packageFunctions()
#' }
packageFunctions <- function() {
  
  # Get Names and Types of Package Namespace Contents
  package_name_space <- packageNameSpace()
  
  # Get Names of Functions as Character Vector
  function_names <- packageNameSpace() %>% 
    dplyr::filter(.data$type == 'function') %>% 
    dplyr::pull(.data$name)
  
  # Get Function Details
  result <- purrr::map(function_names, function(fname) {
    
    f <- getNamespace('dbproj')[[fname]]
    
    if (!isTRUE(is.function(f))) {
      stop("`f` must be function in call to `dbproj::packageFunctions`")
    }
    
    return(names(formals(f)))
    
  })
  names(result) <- function_names
  
  # Process Result
  
  # Return Result
  return(result)
  
}

#' List S3 Methods for Package 'dbproj'
#' 
#' @importFrom rlang .data
#' 
#' @return character
#'
#' @examples
#' \dontrun{
#' output <- packageS3Methods()
#' }
packageS3Methods <- function() {
  
  # Get Names of S3 Methods in Package Namespace
  s3_method_names <- ls(getNamespace('dbproj')[[".__S3MethodsTable__."]])
  
  # Get Package Function Details
  package_functions <- packageFunctions()
  
  # Validate that `s3_method_names` are all Function Names
  if (!isTRUE(all(s3_method_names %in% names(package_functions)))) {
    stop("`s3_method_names` must be subset of 'dbproj' function namespace in call to `dbproj::packageS3Methods`")
  }
  
  # Get Details of all S3 Methods 
  s3_methods <- package_functions[s3_method_names]
  
  # Return Result
  return(s3_methods)
  
}

#' List S3 Generics for Package 'dbproj'
#' 
#' @importFrom rlang .data
#' 
#' @return character
#'
#' @examples
#' \dontrun{
#' output <- packageS3Generics()
#' }
packageS3Generics <- function() {
  
  # Get Names of S3 Methods in Package Namespace
  s3_method_names <- ls(getNamespace('dbproj')[[".__S3MethodsTable__."]])
  
  # Get Package Function Details
  package_functions <- packageFunctions()
  
  # Validate that `s3_method_names` are all Function Names
  if (!isTRUE(all(s3_method_names %in% names(package_functions)))) {
    stop("`s3_method_names` must be subset of 'dbproj' function namespace in call to `dbproj::packageS3Generics`")
  }
  
  # Get Details of all S3 Methods 
  s3_methods <- package_functions[s3_method_names]
  
  # Calculate Expected S3 Generic Names 
  s3_generic_names <- unique(purrr::map_chr(names(s3_methods), function(t){strsplit(t, '.', TRUE)[[1]][1]}))
  
  if (!isTRUE(length(intersect(s3_method_names, s3_generic_names)) == 0)) {
    stop("`s3_method_names` must not overlap with `s3_generic_names` in call to `dbproj::packageS3Generics`")
  }
  
  if (!isTRUE(all(s3_generic_names %in% names(package_functions)))) {
    stop("`s3_generic_names` must be subset of 'dbproj' function namespace in call to `dbproj::packageS3Generics`")
  }
  
  # Get Details of all S3 Generics 
  s3_generics <- package_functions[s3_generic_names]
  
  # Return Result
  return(s3_generics)
  
}
