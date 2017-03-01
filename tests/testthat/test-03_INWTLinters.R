context("INWT's own linter functions")


test_that("args_without_default_first_linter (within line)", {
  # nolint start
  inputWrong <- list(filename = "An example object",
                     file_lines = c("function(arg1 = TRUE, arg2)",
                                    "function(arg1 = TRUE, arg2, arg3)",
                                    "function(arg1, arg2 = TRUE, arg3)",
                                    "function(arg1 = 1, arg2, arg3 = 'astring')",
                                    "function(arg1 = TRUE, arg.2 = 123, arg3)"))
  # nolint end
  inputCorrect <- list(filename = "An example object",
                       file_lines = c("function(arg1 = 2)",
                                      "function(arg1)",
                                      "function(arg1 = 2, arg_2 = 'string')",
                                      "function(arg_1, arg2 = TRUE)",
                                      "function(arg1, arg2 = TRUE)",
                                      "function(arg1, arg2)",
                                      "function(arg1, arg2 = 1, ...)",
                                      "function(arg1 = TRUE, ...)"))
  expect_true(lapply(args_without_default_first_linter(inputWrong),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(args_without_default_first_linter(inputWrong) %>% length,
               inputWrong$file_lines %>% length)
  expect_true(lapply(args_without_default_first_linter(inputCorrect),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(args_without_default_first_linter(inputCorrect) %>% length, 0)
})


test_that("double_space_linter", {
  # nolint start
  inputWrong <- list(filename = "An example object",
                     file_lines = c("Simple text with  double whitespace",
                                    "  indented text  with double whitespace",
                                    "  # Commented  text",
                                    "  # Indented  line  commented out"))
  inputCorrect <- list(filename = "An example object",
                       file_lines = c("Normal text",
                                      "  indented text",
                                      "#'   An indented roxygen comment",
                                      "  # Indented line commented out"))
  # nolint end
  expect_true(lapply(double_space_linter(inputWrong),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(double_space_linter(inputWrong) %>% length,
               inputWrong$file_lines %>% length)
  expect_true(lapply(double_space_linter(inputCorrect),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(double_space_linter(inputCorrect) %>% length, 0)
})


test_that("internal_INWT_function_linter", {
  inputWrong <- list(filename = "An example object",
                     file_lines = c("INWTpackage:::foo",
                                    "anINWTpkg:::foo",
                                    "some code anINWTpkg:::foo",
                                    "INWTpkg:::foo("))
  inputCorrect <- list(filename = "An example object",
                       file_lines = c("INWTpkg::foo",
                                      "INWT some test dplyr:::foo"))
  expect_true(lapply(internal_INWT_function_linter(inputWrong),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(internal_INWT_function_linter(inputWrong) %>% length,
               inputWrong$file_lines %>% length)
  expect_true(lapply(internal_INWT_function_linter(inputCorrect),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(internal_INWT_function_linter(inputCorrect) %>% length, 0)
})


test_that("library_linter", {
  # nolint start
  inputWrong <- list(filename = "An example object",
                     file_lines = c("library(pkg)",
                                    'library("pkg"',
                                    "  library(pkg)"))
  # nolint end
  inputCorrect <- list(filename = "An example object",
                       file_lines = c("# Comment with the word library",
                                      "#' Roxygen comment containing library",
                                      "some code 123 # comment with library"))
  expect_true(lapply(library_linter(inputWrong),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(library_linter(inputWrong) %>% length,
               inputWrong$file_lines %>% length)
  expect_true(lapply(library_linter(inputCorrect),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(library_linter(inputCorrect) %>% length, 0)
})


test_that("setwd_linter", {
  # nolint start
  inputWrong <- list(filename = "An example object",
                     file_lines = c("setwd(path)",
                                    'setwd("path"',
                                    "  setwd(path)"))
  # nolint end
  inputCorrect <- list(filename = "An example object",
                       file_lines = c("# Comment with the word setwd",
                                      "#' Roxygen comment containing setwd",
                                      "some code 123 # comment with setwd("))
  expect_true(lapply(setwd_linter(inputWrong),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(setwd_linter(inputWrong) %>% length,
               inputWrong$file_lines %>% length)
  expect_true(lapply(setwd_linter(inputCorrect),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(setwd_linter(inputCorrect) %>% length, 0)
})


test_that("source_linter", {
  # nolint start
  inputWrong <- list(filename = "An example object",
                     file_lines = c("source(file)",
                                    'source("file"',
                                    "  source(file)"))
  # nolint end
  inputCorrect <- list(filename = "An example object",
                       file_lines = c("# Comment with the word source",
                                      "#' Roxygen comment containing source",
                                      "some code 123 # comment with source(",
                                      paste0("PackageInstallArgs: --no-multiarch",
                                             "--with-keep.source")))
  expect_true(lapply(source_linter(inputWrong),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(source_linter(inputWrong) %>% length,
               inputWrong$file_lines %>% length)
  expect_true(lapply(source_linter(inputCorrect),
                     function(x) class(x) == "lint") %>% unlist %>% all)
  expect_equal(source_linter(inputCorrect) %>% length, 0)
})
