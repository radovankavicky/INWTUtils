context("Code Style")

# pathPrefix <- if (interactive()) "" else "../../"
# scriptPath <- paste0(pathPrefix, "RScripts")
# pkgFunPath <- paste0(pathPrefix, "R")
pkgPath <- normalizePath(file.path(lintr:::find_package()))


# nolint start
test_that("Code in line with INWT style conventions", {
  lintr::expect_lint_free(linters = INWTUtils::linterList())
  # nolint end
  # testthat::expect_warning(tmp <- lapply(paste0(pkgFunPath, "/", setdiff(list.files(pkgFunPath),
  #                                                                        "checkCodeStyle.R")),
  #                                        function(file) {
  #                                          customTests <- lapply(INWTUtils:::forbiddenTextPkgFuns(),
  #                                                                function(text) INWTUtils:::checkText(file, text))
  #                                        }),
  #                          NA)
  # testthat::expect_warning(lapply(paste0(pkgPath, "/", "RScripts/", list.files(scriptPath)),
  #                                 function(file) {
  #                                   lapply(INWTUtils:::forbiddenTextScript(),
  #                                          function(text) INWTUtils:::checkText(file, text))
  #                                 }),
  #                          NA)
  # testthat::expect_warning(tmp <- lapply(paste0(scriptPath, "/", list.files(scriptPath)),
  #                                        function(file) checkStyle(file, type = "script")),
  #                          NA)
  # testthat::expect_warning(tmp <- lapply(paste0(pkgFunPath, "/", setdiff(list.files(pkgFunPath),
  #                                                                        "checkCodeStyle.R")),
  #                                        function(file) checkStyle(file, type = "pkgFuns")),
  #                          NA)
})
