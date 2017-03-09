#' Remove non-standard packages
#'
#' @description Removes all non-standard packages from the searchpath. Only
#' packages loaded when starting R remain in the searchpath. This leads to a
#' clean searchpath without unwanted masking effects.
#' @export
#'
#' @examples
#' \dontrun{
#' library(INWTutils)
#' search()
#' rmPkgs()
#' search()
#' # INWTutils is not in the searchpath anymore
#' }
rmPkgs <- function() {
  if (!is.null(packages <- names(sessionInfo()$otherPkgs))) {
    pkgs <- paste0('package:', packages)
    suppressWarnings(
      invisible(
        lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)
      )
    )
  }
}
