#' Check code style
#'
#' @description A file is checked for violations of the INWT style conventions.
#' All types of files are checked using \code{\link[lintr]{lint}}. For details
#' about the tested linters see \code{\link{linterList}}. Further checks are
#' conducted if the type of the file is specified ("script" or "pkgFuns"). Those
#' checks are applied only to uncommented lines.
#'
#' @param file character: File to check
#' @param type character: Type of the file (script or pkgFuns)
#' 
#' @examples \dontrun{
#' # Write example file:
#' writeLines(c(
#'   "# This is an example document violating style conventions",
#'   "foo <- function() {",
#'   "library(INWTUtils)",
#'   "c(1+1 ,1)  ",
#'   paste("print('A very long text which is nevertheless written into a",
#'   "single line such that the line exceeds 100 character by far'))}"
#' ),
#' con = "example.R")
#' 
#' # Check file:
#' checkStyle(file = "example.R", type = "pkgFuns")
#' }
#'
#' @export
#'
checkStyle <- function(file, type = c("script", "pkgFuns")) {

  customTests <- lapply(if (all(type == "script")) {
    forbiddenTextScript()
  } else if (all(type == "pkgFuns")) {
    forbiddenTextPkgFuns()
  } else {
    NULL
  },
  function(text) checkText(file, text))
  
  lint(file,
       linters = linterList())
  
}


#' List of linters tested to ensure INWT style conventions
#'
#' @description Used in \code{\link{checkStyle}}. Included linters are:
#' \code{\link[lintr]{assignment_linter}},
#' \code{\link[lintr]{closed_curly_linter}},
#' \code{\link[lintr]{commas_linter}},
#' \code{\link[lintr]{infix_spaces_linter}},
#' \code{\link[lintr]{line_length_linter}},
#' \code{\link[lintr]{multiple_dots_linter}},
#' \code{\link[lintr]{no_tab_linter}},
#' \code{\link[lintr]{object_length_linter}},
#' \code{\link[lintr]{object_usage_linter}},
#' \code{\link[lintr]{open_curly_linter}},
#' \code{\link[lintr]{spaces_inside_linter}},
#' \code{\link[lintr]{spaces_left_parentheses_linter}},
#' \code{\link[lintr]{trailing_blank_lines_linter}},
#' \code{\link[lintr]{trailing_whitespace_linter}}
#'
#' @examples # Code that lists all tested linters:
#' cat(paste0("\\code{\\link[lintr]{", sort(names(linterList())), "}}",
#'   collapse = ",\n#' "))
#'
#' @export
#'
linterList <- function() {
  list(assignment_linter = assignment_linter,
       closed_curly_linter = closed_curly_linter,
       commas_linter = commas_linter,
       infix_spaces_linter = infix_spaces_linter,
       line_length_linter = line_length_linter(100),
       multiple_dots_linter = multiple_dots_linter,
       no_tab_linter = no_tab_linter,
       object_usage_linter = object_usage_linter,
       object_length_linter = object_length_linter,
       open_curly_linter = open_curly_linter,
       spaces_inside_linter = spaces_inside_linter,
       spaces_left_parentheses_linter = spaces_left_parentheses_linter,
       trailing_blank_lines_linter = trailing_blank_lines_linter,
       trailing_whitespace_linter = trailing_whitespace_linter)
}


#' Check text file for a patterm
#'
#' Checks a text file for a specific pattern. Regular expressions are allowed.
#' Only uncommented lines (i.e., lines which do not start with \code{#}) are
#' examined.
#'
#' @param file character: Path to a file
#' @param pattern character: Pattern to look for
#'
#' @return If the text contains the pattern, a warning is returned, if not,
#' nothing is returned.
#' 
#' @examples \dontrun{
#' # Write example file:
#' writeLines(c(
#'   "# This is an example document to demonstrate the function checkTest",
#'   "x <- 1:3",
#'   "cat(INWTUtils:::forbiddenTextScript())"),
#'   con = "example.R")
#'   
#' # Check file:
#' checkText("example.R", ":::")
#' }
#'
#' @export
#'
checkText <- function(file, pattern) {
  fileContent <- readLines(file)
  if (length(grep(pattern, fileContent[!startsWith(fileContent, "#")])) > 0)
    warning(paste0("Pattern '", pattern, "' appears in ", file, ".\n"))
}


# Patterns that are not allowed in scripts
forbiddenTextScript <- function() c("INWT\\w*:::")


# Patterns that are not allowed in the function code in R packages
forbiddenTextPkgFuns <- function() c("setwd", "library", "source")
