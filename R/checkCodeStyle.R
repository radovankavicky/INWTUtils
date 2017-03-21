#' Check code style
#'
#' @description Checks a for violations of the INWT style conventions using
#' \code{\link[lintr]{lint}}. In addition to the \code{\link[lintr]{linters}}
#' provided by the package \code{lintr}, some custom linters are tested. The set
#' of used linters depends on the type of the file. If the type is specified
#' ("script" or "pkgFuns"), some additional linters are tested. For details
#' about the tested linters see \code{\link{selectLntrs}}.
#'
#' @param file character: File to check
#' @inheritParams selectLntrs
#'
#' @examples \dontrun{
#' writeLines(con = "lintExample.txt",
#'            text = c("# Example script to demonstrate INWT's own linters",
#'            # nolint start
#'                     "",
#'                     "foo <- function(x = 1, y) {",
#'                     "  2*x + 1",
#'                     "}",
#'                     "",
#'                     paste0("# This  line containts  double spaces and is ",
#'                            "very long. The following lines will use = ",
#'                            "instead of <- and access an internal INWT ",
#'                            "function."),
#'                     "z = 1",
#'                     "print(INWTUtils:::scriptLntrs())",
#'                     ""))
#'            # nolint end
#' checkStyle("lintExample.txt", type = "script")
#'
#' }
#'
#' @export
#'
checkStyle <- function(file, type = c("script", "pkgFuns")) {
  lint(file, linters = selectLntrs(type))
}


#' List of linters to check INWT style conventions
#'
#' @description Used in \code{\link{checkStyle}}. The set of included linters
#' depends on the type of the file. The following linters are always included:
#' \itemize{
#'   \item\code{\link[INWTUtils]{args_without_default_first_linter}},
#'   \item\code{\link[lintr]{assignment_linter}},
#'   \item\code{\link[lintr]{closed_curly_linter}},
#'   \item\code{\link[lintr]{commas_linter}},
#'   \item\code{\link[INWTUtils]{double_space_linter}},
#'   \item\code{\link[lintr]{infix_spaces_linter}},
#'   \item\code{\link[lintr]{line_length_linter}},
#'   \item\code{\link[lintr]{multiple_dots_linter}},
#'   \item\code{\link[lintr]{no_tab_linter}},
#'   \item\code{\link[lintr]{object_length_linter}},
#'   \item\code{\link[lintr]{object_usage_linter}},
#'   \item\code{\link[lintr]{open_curly_linter}},
#'   \item\code{\link[lintr]{spaces_inside_linter}},
#'   \item\code{\link[lintr]{spaces_left_parentheses_linter}},
#'   \item\code{\link[lintr]{trailing_blank_lines_linter}},
#'   \item\code{\link[lintr]{trailing_whitespace_linter}}
#' }
#'
#' The following linters are only included for \code{type = pkgFuns}:
#' \itemize{
#'   \item\code{\link{setwd_linter}},
#'   \item\code{\link{source_linter}}
#' }
#'
#' The following linters are only included for \code{type = script}:
#' \itemize{
#'   \item\code{\link{internal_INWT_function_linter}}
#' }
#'
#' @param type character: Type of the file
#'
#' @examples
#' # Code listing tested linters:
#' linterNames <- sort(names(selectLntrs()))
#' packages <- unlist(lapply(linterNames,
#'                           function(name) {
#'                             erg <- help.search(name)
#'                             erg$matches$Package
#'                           }))
#' # nolint start
#' cat("#' \\itemize{\n#'",
#'     paste0("  \\item\\code{\\link[", packages, "]{", linterNames, "}}",
#'            collapse = ",\n#' "), "\n#' }")
#' # nolint end
#'
#' @export
#'
selectLntrs <- function(type = c("script", "pkgFuns")) {

  linters <- list(args_without_default_first_linter =
                    args_without_default_first_linter,
                  assignment_linter = assignment_linter,
                  closed_curly_linter = closed_curly_linter,
                  commas_linter = commas_linter,
                  double_space_linter = double_space_linter,
                  infix_spaces_linter = infix_spaces_linter,
                  line_length_linter = line_length_linter(100),
                  multiple_dots_linter = multiple_dots_linter,
                  no_tab_linter = no_tab_linter,
                  object_usage_linter = object_usage_linter,
                  object_length_linter = object_length_linter(30L),
                  open_curly_linter = open_curly_linter,
                  spaces_inside_linter = spaces_inside_linter,
                  spaces_left_parentheses_linter =
                    spaces_left_parentheses_linter,
                  trailing_blank_lines_linter = trailing_blank_lines_linter,
                  trailing_whitespace_linter = trailing_whitespace_linter)

  if (all(type == "script")) linters <- c(linters, scriptLntrs())
  if (all(type == "pkgFuns")) linters <- c(linters, pkgFunLntrs())

  return(linters)
}


pkgFunLntrs <- function() {
  list(setwd_linter = setwd_linter,
       source_linter = source_linter)
}


scriptLntrs <- function() {
  list(internal_INWT_function_linter = internal_INWT_function_linter)
}
