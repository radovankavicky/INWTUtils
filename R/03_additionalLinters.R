#' @title INWT's own linters
#'
#' @name INWTLinters
#'
#' @description Linters added by INWT. Usually not called directly but used
#' with \code{\link[lintr]{lint}}.
#'
#' @param source_file returned by \code{\link[lintr]{get_source_expressions}}
#'
#' @examples \dontrun{
#' writeLines(con = "lintExample.txt",
#' # nolint start
#'            text = c("# Example script to demonstrate INWT's own linters",
#'                     "",
#'                     "source(anotherScript.R)",
#'                     "",
#'                     "foo <- function(x = 1, y) {",
#'                     "  2 * x + 1",
#'                     "}",
#'                     "",
#'                     "# This  line containts  double spaces",
#'                     "",
#'                     "print(INWTUtils:::scriptLinters())"))
#' # nolint end
#' lintr::lint("lintExample.txt",
#'      linters = list(argsWithoutDefault = args_without_default_first_linter,
#'                     doubeWhitespace = double_space_linter,
#'                     sourceLinter = source_linter))
#' }
#'
NULL


#' @describeIn INWTLinters Arguments without default values should come before
#' arguments with default values.
#' @export
args_without_default_first_linter <- function(source_file) {

  pattern <- paste0("function\\([^\\)]*[A-z0-9_\\. '\"]+=[A-z0-9_\\. '\"]+,",
                    "[ ]*",
                    "[A-z0-9_\\. '\"]+[^(\\.\\.\\.)][\\),]+")
  ## Argument with default value
  # String "function("
  # Any number of characters which are not ")"
  # At least one character of A-z, 0-9, "_", ".", space, and quotes (name of an
    # argument)
  # Exaclty one "="
  # Default value of the argument
  # Comma separating this argument from the next one
  ## Arbitrary number of spaces
  ## Argument without default value
  # Name of an argument
  # NOT "..." (since this would be allowed)
  # Closing bracket or comma (without having defined a default value)

  # Problems within lines
  idsWithin <- grep(pattern, source_file$file_lines)

  # Problems over two lines
  pastedText <- lapply(1:(length(source_file$file_lines) - 1),
                       function(x) paste(source_file$file_lines[x],
                                         source_file$file_lines[x + 1]))

  idsMult <- grep(pattern, pastedText)

  # Remove double-counted problems
  ids <- c(idsWithin, setdiff(idsMult, c(idsWithin - 1))) %>% unique

  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Arguments without default value should be listed before
         arguments with default value.",
         linter = "args_without_default_first_linter")
  })
}


#' @describeIn INWTLinters Are there double whitespaces?
#' @export
double_space_linter <- function(source_file) {

  ids <- grep("([^#' ]+.*[^ ]+ {2,}[^( +#$)])|(^# \\w {2,})",
              source_file$file_lines)
  # At least one character which is neither # nor '
  # Any characters
  # At least one character which is not space
  # At least two spaces
  # ...not directly followed by some spaces and a hash
  # OR: Starting with #, then space, then word character, then at least two
  # spaces
  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Double whitespace.",
         linter = "double_space_linter")
  })
}


#' @describeIn INWTLinters Instead of using internal functions from an INWT
#' package in scripts with \code{:::}, they should be exported and documented.
#' (only for scripts)
#' @export
internal_INWT_function_linter <- function(source_file) {

  ids <- grep("INWT\\w*:::", source_file$file_lines)
  # String INWT
  # Arbitrary number of word characters completing the package name
  # "..." to call internal function

  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "If internal functions are used, they should rather be
         documented and exported.",
         linter = "internal_INWT_function_linter")
  })
}


#' @describeIn INWTLinters Changing the working directory in package functions
#' can have unexpected side effects. (only for package functions)
#' @export
setwd_linter <- function(source_file) {

  ids <- grep("^[^#\'\"]*setwd\\(", source_file$file_lines)
  # Starting with arbitrary number of characters which are NOT # or quotes (to
    # exclude comments and quoted text)
  # String "setwd("

  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Avoid side effects caused by setwd.",
         linter = "setwd_linter")
  })
}


#' @describeIn INWTLinters Sourcing files in package functions can have
#' unexpected side effects. (only for package functions)
#' @export
source_linter <- function(source_file) {

  ids <- grep("^[^#\'\"]*source\\(", source_file$file_lines)
  # Starting with arbitrary number of characters which are NOT # or quotes (to
    # exclude comments and quoted text)
  # String "source("

  lapply(ids, function(id) {
    Lint(filename = source_file$filename,
         line_number = id,
         column_number = NULL,
         type = "style",
         message = "Don't use source in package functions.",
         linter = "source_linter")
  })
}
