context("Style checking functions")

# nolint start

# pathPrefix <- if (interactive()) "tests/testthat/" else ""
#
# test_that("Regular expressions in forbiddenTextScript", {
#   expect_equal(grep(INWTUtils:::forbiddenTextScript()["accessInternalFunction"],
#                     "INWTCLV:::functionname"), 1)
#   expect_length(grep(INWTUtils:::forbiddenTextScript()["accessInternalFunction"],
#                      "INWTCLV::functionname"), 0)
#   expect_length(grep(INWTUtils:::forbiddenTextScript()["accessInternalFunction"],
#                      "INWT dplyr:::functionname"), 0)
# })
#
# test_that("Regular expressions in forbiddenTextPkgFuns: setwd", {
#   expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["setwd"],
#                     "setwd"), 1)
# })
#
# test_that("Regular expressions in forbiddenTextPkgFuns: library", {
#   expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["library"],
#                     "library"), 1)
# })
#
# test_that("Regular expressions in forbiddenTextPkgFuns: sourcedFiles", {
#   expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["sourcedFiles"],
#                     "source"), 1)
#   expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["sourcedFiles"],
#                     " source"), 1)
#   expect_length(grep(INWTUtils:::forbiddenTextPkgFuns()["sourcedFiles"],
#                      "--with-keep.source"), 0)
# })
#
#
# test_that("Regular expressions in forbiddenTextPkgFuns: double whitespace", {
#   expect_equal(grep(INWTUtils:::forbiddenTextAll()["doubleWhitespace"],
#                     c("text",
#                       "  indented text",
#                       "text with  double whitespace",
#                       "  indented text  with double whitespace")),
#                c(3, 4))
# })
#
# pattern <- INWTUtils:::forbiddenTextPkgFuns()["argsWithoutDefaultFirst"]
# test_that("Regex in forbiddenTextPkgFuns: args without default come first", {
#
#   expect_length(grep(pattern, "function(arg1 = TRUE,
#                      arg2)"), 1)
#   expect_length(grep(pattern, "function(arg1 = TRUE,
#                         arg2,
#                         arg3)"), 1)
#   expect_length(grep(pattern, "function(arg1,
#                         arg2 = TRUE,
#                         arg3)"), 1)
#   expect_length(grep(pattern, "function(arg1 = 1,
#                         arg2,
#                         arg3 = 'astring')"), 1)
#   expect_length(grep(pattern, "function(arg1 = TRUE,
#                         arg.2 = 123,
#                         arg3)"), 1)
#   expect_length(grep(pattern, "function(arg1 = 2"), 0)
#   expect_length(grep(pattern, "function(arg1)"), 0)
#   expect_length(grep(pattern, "function(arg1 = 2,
#                         arg_2 = 'string')"), 0)
#   expect_length(grep(pattern, "function(arg_1,
#                         arg2 = TRUE)"), 0)
#   expect_length(grep(pattern, "function(arg1, arg2 = TRUE)"), 0)
#   expect_length(grep(pattern, "function(arg1,
#                         arg2)"), 0)
#   expect_length(grep(pattern, "function(arg1,
#                         arg2 = 1, ...)"), 0)
#   expect_length(grep(pattern, "function(arg1 = TRUE, ...)"), 0)
# })
#
# test_that("checkText ignores commented lines", {
#   expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "first"), NA)
#   expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "roxygen"), NA)
#   expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "Normal"))
#   expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "later"))
# })
#
# test_that("linterList returns a list of functions", {
#   expect_type(linterList(), "list")
#   expect_true(lapply(linterList(), function(x) class(x) == "function") %>%
#                 unlist %>%
#                 all)
# })
#
# test_that("forbidden text functions return character vector", {
#   expect_type(INWTUtils:::forbiddenTextScript(), "character")
#   expect_type(INWTUtils:::forbiddenTextPkgFuns(), "character")
#   expect_type(INWTUtils:::forbiddenTextAll(), "character")
# })

# nolint end
