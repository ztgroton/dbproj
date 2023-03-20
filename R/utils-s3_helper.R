
#' List Content Names and Types for Package 'dbproj' Namespace
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' output <- dbprojPackageNameSpace()
#' }
dbprojPackageNameSpace <- function() {
  
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

#' List Function Names for Package 'dbproj'
#' 
#' @importFrom rlang .data
#'
#' @return character
#'
#' @examples
#' \dontrun{
#' output <- dbprojFunctionNameSpace()
#' }
dbprojFunctionNameSpace <- function() {
  
  # Get Names and Types of Package Namespace Contents
  package_name_space <- dbprojPackageNameSpace()
  
  # Get Names of Functions as Character Vector
  function_name_space <- package_name_space %>% 
    dplyr::filter(.data$type == 'function') %>% 
    dplyr::pull(.data$name)
  
  return(function_name_space)
  
}

#' List S3 Method Names for Package 'dbproj'
#'
#' @return character
#'
#' @examples
#' \dontrun{
#' output <- dbprojS3Methods()
#' }
dbprojS3Methods <- function() {
  
  # Get Names of S3 Methods in Package Namespace
  s3_methods <- ls(getNamespace('dbproj')[[".__S3MethodsTable__."]])
  
  # Validate that `s3_methods` are all Function Names
  if (!isTRUE(all(s3_methods %in% dbprojFunctionNameSpace()))) {
    stop("`s3_methods` must be subset of 'dbproj' function namespace in call to `dbproj::dbprojS3Methods`")
  }
  
  # Return Result
  return(s3_methods)
  
}
