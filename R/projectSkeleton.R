#' Create file structure
#'
#' @description Creates a useful file and folder structure. This includes:
#' \itemize{
#'   \item data (folder)
#'   \item reports (folder)
#'   \item rScripts (folder)
#'   \item libWin (folder) containing .gitginore
#'   \item libLinux (folder) containing .gitginore
#'   \item .Rprofile
#'   \item .Rproj (optional)
#' }
#' The infrastructure for a package can be added.
#'
#' @param dir character: Directory where the file structure is created, relative
#' to the current working directory. The current working directory by default.
#' @param pkgName character: If \code{pkgName} is specified, a package with this
#' name is created.
#' @param pkgOnToplevel logical: Should the package live in the main directory
#' (default) or in a subfolder called package?
#' @param rProject logical: Create an R Project?
#' @param exampleScript logical: Create example script? (not yet used)
#' @param ... Further arguments passed to \code{\link[devtools]{create}} resp.
#' \code{\link[devtools]{setup}} if creating a package
#'
#' @examples createProjectSkeleton("tmp", rProject = TRUE)
#'
#' @export
createProjectSkeleton <- function(dir = ".",
                                  pkgName = NULL,
                                  pkgOnToplevel = TRUE,
                                  rProject = FALSE,
                                  exampleScript = TRUE,
                                  ...) {

  if (dir != ".") {
    message(paste0("Creating '", dir, "/'"))
    dir.create(dir)
  }
  if (substr(dir, nchar(dir), nchar(dir)) != "/") dir <- paste0(dir, "/")

  folders <- c("data", "libLinux", "libWin", "reports", "RScripts")
  message(paste0("Creating directories: ", paste0(folders, collapse = ", ")))
  lapply(paste0(dir, folders), dir.create)

  message("Writing .gitignore")
  copyFile(dir, "gitignore", "libLinux/.gitignore")
  copyFile(dir, "gitignore", "libWin/.gitignore")

  message("Writing .Rprofile")
  copyFile(dir, "Rprofile", ".Rprofile")

  if (exampleScript) {
    message("Writing example script")
    copyFile(dir, "exampleScript.R", "RScripts")
  }

  if (!is.null(pkgName)) {
    createPackage(dir, pkgName, pkgOnToplevel, ...)
  }

  if (rProject) {
    message("Creating R Project")
    createProject(!is.null(pkgName), pkgOnToplevel, dir)
  }

}


# Copy a file from inst to the - per default - respective directory relative to
# the project root
# dir: project root
# origin: name/path of file relative to inst
# dest: new path (path only or path with filename) relative to project root
copyFile <- function(dir, origin, dest = origin, ...) {
  file.copy(from = system.file(origin, package = "INWTUtils"),
            to = paste0(dir, dest), ...)
}


#' Create package
#'
#' @description Creates a package, either directly in the directory specified in
#' \code{dir} or in a subfolder called "package". Also creates an infrastructure
#' for testthat, a test for the package style and an .Rbuildignore. An R project
#' has to be created separately.
#' Used by \code{\link{createProjectSkeleton}}.
#'
#' @param dir character: Directory
#' @param pkgName character: Package name
#' @param pkgOnToplevel logical: Should the package live in the main
#' directory or in a subfolder called package?
#' @param ... Further arguments passed to \code{\link[devtools]{create}} resp.
#' \code{\link[devtools]{setup}}
#'
#' @examples
#' \dontrun{
#' createPackage(dir = "./", pkgName = "aTestPackage", pkgOnToplevel = FALSE)
#' dir.create("tmp")
#' createPackage(dir = "tmp/", pkgName = "aTestPackage", pkgOnToplevel = TRUE)
#' }
#'
#' @export
#'
createPackage <- function(dir, pkgName, pkgOnToplevel, ...) {

  if (substr(dir, nchar(dir), nchar(dir)) != "/") dir <- paste0(dir, "/")
  packageDir <- if (pkgOnToplevel) dir else paste0(dir, "package/")

  do.call(if (pkgOnToplevel) setup else create,
          args = list(path = packageDir,
                      description = list(Package = pkgName,
                                         Imports = "lintr, INWTUtils"),
                      rstudio = FALSE,
                      ... = ...))

  use_testthat(pkg = packageDir)
  writeLines(c('context("Code Style")',
               '# nolint start',
               'test_that("Code style is in line with INWT style conventions", {',
               '  lintr::expect_lint_free(linters = INWTUtils::selectLntrs())',
               '}',
               '# nolint end',
               ')'),
             con = paste0(packageDir, "tests/testthat/test-codeStyle.R"))

  copyFile(dir, ".Rbuildignore", ifelse(pkgOnToplevel, "", "package"))
}


#' Create R project
#'
#' @description Creates an R project with useful configuration. If the project
#' contains a package, the respective options are set. The project is named
#' after the folder which contains it.
#' Internally used by \code{\link{crateProjectSkeleton}}.

#' @param pkg logical: Does the project contain a package?
#' @param pkgOnToplevel logical: Does the package live in the project directory
#' (default) or a subfolder "package"?
#' @param dir character: Directory where the R project is created; current
#' working directory by default
#'
#' @examples
#' \dontrun{
#' createProject(pkg = FALSE, dir = "./")
#' dir.create("tmp")
#' createProject(pkg = TRUE, pkgOnToplevel = FALSE, dir = "tmp")
#' }
#'
#' @export
#'
createProject <- function(pkg, pkgOnToplevel = TRUE, dir = "./") {
  prefs <- c("Version: 1.0",
             "",
             "RestoreWorkspace: No",
             "SaveWorkspace: No",
             "AlwaysSaveHistory: No",
             "",
             "EnableCodeIndexing: Yes",
             "UseSpacesForTab: Yes",
             "NumSpacesForTab: 2",
             "Encoding: UTF-8",
             "",
             "RnwWeave: Sweave",
             "LaTeX: pdfLaTeX")
  if (pkg) prefs <- c(prefs,
                      "",
                      "BuildType: Package",
                      "PackageUseDevtools: Yes",
                      if (!pkgOnToplevel) "PackagePath: package",
                      "PackageInstallArgs: --no-multiarch --with-keep.source",
                      "PackageRoxygenize: rd,collate,namespace,vignette")

  if (substr(dir, nchar(dir), nchar(dir)) != "/") dir <- paste0(dir, "/")
  projName <- (if (dir == "./") getwd() else dir) %>%
    strsplit("/") %>% unlist %>% `[`(length(.))

  writeLines(text = prefs,
             con = paste0(dir, projName, ".Rproj"))
}
