#' Check code style
#'
#' @description A file is checked for violations of the INWT style conventions
#' using \code{\link[lintr]{lint}}. In addition to the
#' \code{\link[lintr]{linters}} provided by the package \code{lintr}, some
#' custom linters are tested. For details about the tested linters see
#' \code{\link{linterList}}. The set of used linters depends on the type of the
#' file. If the type is specified ("script" or "pkgFuns"), some additional
#' linters are tested.
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
#'   "single line such that the line exceeds 100 character by far')}"
#' )),
#' con = "example.R")
#'
#' # Check file:
#' checkStyle(file = "example.R", type = "pkgFuns")
#' }
#'
#' @export
#'
checkStyle <- function(file, type = c("script", "pkgFuns")) {
  lint(file, linters = linterList(type))
}


#' List of linters tested to ensure INWT style conventions
#'
#' @description Used in \code{\link{checkStyle}}. The set of included linters
#' depends on the type of the file. The following linters are always included:
#' \code{\link[lintr]{assignment_linter}},
#' \code{\link[lintr]{closed_curly_linter}},
#' \code{\link[lintr]{commas_linter}},
#' \code{\link{double_whitepace_linter}},
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
linterList <- function(type = c("script", "pkgFuns")) {

  linters <- list(assignment_linter = assignment_linter,
                  closed_curly_linter = closed_curly_linter,
                  commas_linter = commas_linter,
                  double_whitepace_linter = double_whitepace_linter,
                  infix_spaces_linter = infix_spaces_linter,
                  line_length_linter = line_length_linter(100),
                  multiple_dots_linter = multiple_dots_linter,
                  no_tab_linter = no_tab_linter,
                  object_usage_linter = object_usage_linter,
                  object_length_linter = object_length_linter(30L),
                  open_curly_linter = open_curly_linter,
                  spaces_inside_linter = spaces_inside_linter,
                  spaces_left_parentheses_linter = spaces_left_parentheses_linter,
                  trailing_blank_lines_linter = trailing_blank_lines_linter,
                  trailing_whitespace_linter = trailing_whitespace_linter)

  if (all(type == "script")) {
    linters <- c(linters,
                 internal_INWT_function_linter = internal_INWT_function_linter)
  }

  if (all(type == "pkgFuns")) {
    linters <- c(linters,
                 args_without_default_first_linter = args_without_default_first_linter,
                 library_linter = library_linter,
                 setwd_linter = setwd_linter)
  }

  return(linters)
}
