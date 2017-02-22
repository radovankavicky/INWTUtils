context("Package code style")

pkgPath <- if (interactive()) getwd() else paste0(getwd(), "/../../../INWTUtils")

writeLines(pkgPath,
           con = "~/Netzfreigaben/Git_TEX/logdatei.txt")

scripts <- paste0(pkgPath, "/RScripts/",
                  list.files(paste0(pkgPath, "/RScripts")))
pkgFuns <- paste0(pkgPath, "/R/",
                  setdiff(list.files(paste0(pkgPath, "/R")), "checkCodeStyle.R"))

test_that("Code in line with INWT style conventions", {
  lintr::expect_lint_free(linters = INWTUtils::linterList())
  testthat::expect_warning(lapply(pkgFuns,
                                  function(file) {
                                    lapply(INWTUtils:::forbiddenTextPkgFuns(),
                                           function(text) {
                                             INWTUtils:::checkText(file, text)
                                           })}),
                           NA)
  testthat::expect_warning(lapply(scripts,
                                  function(file) {
                                    lapply(INWTUtils:::forbiddenTextScript(),
                                           function(text) {
                                             INWTUtils:::checkText(file, text)
                                           })}),
                           NA)
  testthat::expect_warning(lapply(scripts,
                                  function(file) checkStyle(file,
                                                            type = "script")),
                           NA)
  testthat::expect_warning(lapply(pkgFuns,
                                  function(file) checkStyle(file,
                                                            type = "pkgFuns")),
                           NA)
})
