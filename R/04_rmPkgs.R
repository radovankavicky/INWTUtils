#' Remove non-standard packages from searchpath
#'
#' @description Removes all non-standard packages from the searchpath. Only
#' packages loaded when starting R remain in the searchpath. This leads to a
#' clean searchpath without unwanted masking effects.
#' @export
#'
#' @examples
#' \dontrun{
#' library(INWUtils)
#' search()
#' rmPkgs()
#' search()
#' # INWUtils is not in the searchpath anymore
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


#' Uninstall package from all libraries
#'
#' @description Uninstalls one or more packages from all libraries with write
#' rights
#' @param packages character vector of packages to remove
#' @export
deletePkgs <- function(packages) {
  invisible(lapply(packages, function(p) {
    cat(paste0("Libraries where ", p, " is installed:\n"))
    cat(showLibs(p)[[p]])
    cat("\n\nUninstalling...\n\n")
    remove.packages(p, lib = .libPaths())
    cat(paste0("Libraries where ", p, " is installed:\n"))
    cat(showLibs(p)[[p]])
  }))
}


#' List libraries where package is installed
#'
#' @description List all libraries where packages are installed
#' @param packages character: vector package names
#' @return Named list with one entry for each package. The entries list all
#' libraries of the search path where the package is installed.
#' @export
#'
#' @examples showLibs(c("lintr", "INWTUtils", "nonexistentPackage"))
showLibs <- function(packages = NULL) {
  lapply(packages, function(package) {
    paths <- .libPaths()[lapply(.libPaths(), function(path) {
      package %in% (installed.packages(lib.loc = path) %>% row.names)
    }) %>% unlist]
    if (length(paths) == 0) paths <- "Package not installed"
    paths
  }) %>% setNames(packages)
}
