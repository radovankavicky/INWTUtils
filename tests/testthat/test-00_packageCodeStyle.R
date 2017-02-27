context("Package code style")

pkgPath <- if (interactive()) getwd() else paste0(getwd(), "/../../../INWTUtils")

writeLines(pkgPath,
           con = "~/Netzfreigaben/Git_TEX/logdatei.txt")

scripts <- paste0(pkgPath, "/RScripts/",
                  list.files(paste0(pkgPath, "/RScripts")))
pkgFuns <- paste0(pkgPath, "/R/",
                  setdiff(list.files(paste0(pkgPath, "/R")),
                          "forbiddenPatterns.R"))


test_that("Code is lint-free", {
  lintr::expect_lint_free(linters = INWTUtils::linterList())
})


test_that("Code in line with INWT style conventions", {
  expect_warning(lapply(pkgFuns,
                        function(file) {
                          lapply(INWTUtils:::forbiddenTextPkgFuns(),
                                 function(text) {
                                   INWTUtils:::checkText(file, text)
                                 })}),
                 NA)
  expect_warning(lapply(scripts,
                        function(file) {
                          lapply(INWTUtils:::forbiddenTextScript(),
                                 function(text) {
                                   INWTUtils:::checkText(file, text)
                                 })}),
                 NA)
  expect_warning(lapply(scripts,
                        function(file) checkStyle(file,
                                                  type = "script")),
                 NA)
  expect_warning(lapply(pkgFuns,
                        function(file) checkStyle(file,
                                                  type = "pkgFuns")),
                 NA)
})
