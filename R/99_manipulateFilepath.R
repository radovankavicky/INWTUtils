#' Remove trailing "/" from path
#' @param path charachter: A filepath
rmBackslash <- function(path) {
  if (substr(path, nchar(path), nchar(path)) == "/") {
    substr(path, 1, nchar(path) - 1)
  } else {
    path
  }
}


#' Add trailing "/" to path
#' @param path charachter: A filepath
addBackslash <- function(path) {
  if (substr(path, nchar(path), nchar(path)) != "/") {
    paste0(path, "/")
  } else{
    path
  }
}
