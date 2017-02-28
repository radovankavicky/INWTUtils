#' INWT's own linters
#' @description Linters added by INWT. Can be used with
#' \code{\link[lintr]{lint}}.
#' @param source_file returned by \code{\link[lintr]{get_source_expressions}}
INWTlinters <- function() NULL


#' @describeIn INWTlinters Arguments without default values should come before
#' arguments with default values. (only for package functions)
#' @export
args_without_default_first_linter <- function(source_file) {
  ids <- grep("function\\(.*=.*,[\n]?[ ]*[A-z0-9_\\.]*[^...][\\),]",
              source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Arguments without default value should be listed before
         arguments with default value",
         linter = "args_without_default_first_linter")
  })
}


# nolint start
#' @describeIn INWTlinters Are there double whitespaces ("  ")?
#' @export
# nolint end
double_space_linter <- function(source_file) {
  ids <- grep("[^#']+[^ ]+ {2,}", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Double whitespace",
         linter = "double_space_linter")
  })
}


#' @describeIn INWTlinters Instead of using internal functions from an INWT
#' package in scripts with \code{:::}, they should be exported and documented.
#' (only for scripts)
#' @export
internal_INWT_function_linter <- function(source_file) {
  ids <- grep("INWT\\w*:::", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "If internal functions are used, the should rather be
         documented and exported",
         linter = "internal_INWT_function_linter")
  })
}


#' @describeIn INWTlinters Using library in package functions can have
#' unexpected side effects. (only for package functions)
#' @export
library_linter <- function(source_file) {
  ids <- grep("^[^#\'\"]*library\\(", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Don't use library in package functions",
         linter = "library_linter")
  })
}


#' @describeIn INWTlinters Changing the working directory in package functions
#' can have unexpected side effects. (only for package functions)
#' @export
setwd_linter <- function(source_file) {
  ids <- grep("^[^#\'\"]*setwd\\(", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Avoid side effects caused by setwd",
         linter = "setwd_linter")
  })
}


#' @describeIn INWTlinters Sourcing files in package functions can have
#' unexpected side effects. (only for package functions)
#' @export
source_linter <- function(source_file) {
  ids <- grep("([^.]|^)source\\(", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Don't use source in package functions",
         linter = "source_linter")
  })
}
