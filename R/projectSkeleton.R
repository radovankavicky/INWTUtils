#' Create file structure
#'
#' @description Creates a file structure with a useful folder structure. This
#' includes the folders data, reports and rScripts to store files. Furthermore,
#' the folders libWin and libLinux are created, together with an .Rprofile file.
#' Newly installed packages are then stored in the respective lib folder to
#' enable work in a sandbox. The infrastructure for a package and an RStudio
#' project can be added.
#'
#' @param dir Directory where the file structure is created, relative to the
#' current working directory. The current working directory by default.
#' @param pkgName character: If \code{pkgName} is specified, a package with this
#' name is created.
#' @param pkgOnToplevel logical: Should the package live in the main directory
#' (default) or in a subfolder called package?
#' @param rProject logical: Create an R Project?
#' @param ... Further arguments passed to \code{\link[devtools]{create}} resp.
#' \code{\link[devtools]{setup}} if creating a package
#'
#' @examples projectSkeleton("tmp")
#'
#' @export
projectSkeleton <- function(dir = ".",
                            pkgName = NULL,
                            pkgOnToplevel = TRUE,
                            rProject = FALSE,
                            ...) {

  if (dir != ".") {
    message(paste0("Creating '", dir, "/'"))
    dir.create(dir)
  }
  dir <- paste0(dir, "/")

  folders <- c("data", "libLinux", "libWin", "reports", "rScripts")
  message(paste0("Creating directories: ", paste0(folders, collapse = ", ")))
  lapply(paste0(dir, folders), dir.create)

  message("Writing .gitignore")
  writeGitignore(paste0(dir, "libLinux"))
  writeGitignore(paste0(dir, "libWin"))

  message("Writing .Rprofile")
  writeLines(c('.First <- function() {',
               '  if (grepl("Windows", Sys.getenv("OS"))) {',
               paste0('    .libPaths(new = c(paste(getwd(), "libWin",',
                      ' sep = "/"), .libPaths()))'),
               '  } else {',
               paste0('    .libPaths(new = c(paste(getwd(), "libLinux",',
                      ' sep = "/"), .libPaths()))'),
               '  }',
               '}',
               '',
               '.First()'),
             con = paste0(dir, ".Rprofile"))

  if (!is.null(pkgName)) {
    createPackage(pkgName, pkgOnToplevel, dir, ...)
  }

  if(rProject) {
    message("Creating R Project")
    createProject(dir, !is.null(pkgName), pkgOnToplevel)
  }

}


# Write empty .gitignore to push lib folders to github
writeGitignore <- function(dir) {
  writeLines(c("# Ignore everything in this directory",
               "*", "# Except this file", "!.gitignore"),
             con = paste0(dir, "/.gitignore"))
}


#' Create package
#'
#' @description Creates a package, either directly in the passed directory or
#' in a subfolder called package. Also creates infrastructure for testthat.
#' Internally used by \code{\link{projectSkeleton}}.
#'
#' @param pkgName character: Package name
#' @param pkgOnToplevel logical: Should the package live in the main
#' directory or in a subfolder called package?
#' @param dir character: Directory
#' @param ... Further arguments passed to \code{\link[devtools]{create}} resp.
#' \code{\link[devtools]{setup}}
#'
#' @export
#'
createPackage <- function(pkgName, pkgOnToplevel, dir, ...) {

  packageDir <- if (pkgOnToplevel) dir else paste0(dir, "package/")

  do.call(if (pkgOnToplevel) setup else create,
          args = list(path = packageDir,
                      description = list(Package = pkgName),
                      rstudio = FALSE,
                      ... = ...))

  use_testthat(pkg = packageDir)
  # writeLines(c('context("Code Style")',
  #              '# nolint start',
  #              'test_that("Code style is in line with INWT style conventions", {',
  #              '  lintr::expect_lint_free(linters = INWTUtils::linterList())',
  #              '}',
  #              '# nolint end',
  #              ')'),
  #            con = paste0(packageDir, "tests/testthat/test-codeStyle.R"))
}


#' Create R project
#'
#' @description Creates an R project with useful configuration. If the project
#' contains a package, the respective options are set. Internally used by
#' \code{\link{projectSkeleton}}.
#'
#' @param dir character: Directory where the R project is created
#' @param pkg logical: Does the project contain a package?
#' @param pkgOnToplevel logical: Does the package live in the project directory
#' or a subfolder "package"?
#'
#' @export
#'
createProject <- function(dir, pkg, pkgOnToplevel) {
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
  projName <- paste0(getwd(), "/", dir) %>%
    strsplit("/") %>% unlist %>% `[`(length(.))
  writeLines(text = prefs,
             con = paste0(dir, projName, ".Rproj"))
}
