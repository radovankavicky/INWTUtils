context("Style checking functions")

pathPrefix <- if (interactive()) "tests/testthat/" else ""

test_that("Regular expressions in forbiddenTextScript", {
  expect_equal(grep(INWTUtils:::forbiddenTextScript()["accessInternalFunction"],
                    "INWTCLV:::functionname"), 1)
  expect_length(grep(INWTUtils:::forbiddenTextScript()["accessInternalFunction"],
                     "INWTCLV::functionname"), 0)
  expect_length(grep(INWTUtils:::forbiddenTextScript()["accessInternalFunction"],
                     "INWT dplyr:::functionname"), 0)
})

test_that("Regular expressions in forbiddenTextPkgFuns", {
  expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["setwd"],
                    "setwd"), 1)
  expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["library"],
                    "library"), 1)
  expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["sourcedFiles"],
                    "source"), 1)
  expect_equal(grep(INWTUtils:::forbiddenTextPkgFuns()["sourcedFiles"],
                    " source"), 1)
  expect_length(grep(INWTUtils:::forbiddenTextPkgFuns()["sourcedFiles"],
                     "--with-keep.source"), 0)
  expect_equal(grep(INWTUtils:::forbiddenTextAll()["doubleWhitespace"],
                    c("text",
                      "  indented text",
                      "text with  double whitespace",
                      "  indented text  with double whitespace")),
               c(3, 4))
})

test_that("checkText ignores commented lines", {
  expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "first"), NA)
  expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "roxygen"), NA)
  expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "Normal"))
  expect_warning(checkText(paste0(pathPrefix, "checkText.txt"), "later"))
})
