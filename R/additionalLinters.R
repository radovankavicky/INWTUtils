# Everywhere

#' @export
double_whitepace_linter <- function(source_file) {
  ids <- grep("[^#']+[^ ]+ {2,}", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Double whitespace",
         linter = "double_whitepace_linter")
  })
}


# Only in scripts

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


# Only in packages

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

#' @export
source_file_linter <- function(source_file) {
  ids <- grep("([^.]|^)source\\(", source_file$file_lines)
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Don't use source in package functions",
         linter = "source_file_linter")
  })
}

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
