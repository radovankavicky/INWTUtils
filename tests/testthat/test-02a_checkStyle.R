context("Function checkStyle()")

test_that("checkStyle returns correct type", {
  testFile <- system.file("testScript_checkStyle.R", package = "INWTUtils")
  expect_true(file.exists(testFile))
  erg <- checkStyle(testFile, type = "script")
  expect_s3_class(erg, "lints")
  expect_true(lapply(erg, class) %>% unlist %>% `==`("lint") %>% all)
})


test_that("checkStyle returns correct content", {
  erg <- checkStyle(system.file("testScript_checkStyle.R",
                                package = "INWTUtils"),
                    type = "script")
  expect_true(erg[[1]]$message == paste("Arguments without default value should",
                                        "be listed before\n         arguments",
                                        "with default value."))
  expect_true(erg[[2]]$message == "Use <-, not =, for assignment.")
  expect_true(erg[[3]]$message == "Commas should always have a space after.")
  expect_true(erg[[4]]$message == "Double whitespace.")
  expect_true(erg[[5]]$message == "Double whitespace.")
  expect_true(erg[[6]]$message == "Put spaces around all infix operators.")
  expect_true(erg[[7]]$message ==
                "lines should not be more than 100 characters.")
  expect_true(erg[[8]]$message == paste("Variable and function names should",
                                        "not be longer than 30 characters."))
  expect_true(erg[[9]]$message == paste("If internal functions are used, they",
                                        "should rather be\n         documented",
                                        "and exported."))
  expect_true(erg[[10]]$message == paste("Place a space before left",
                                         "parenthesis, except in a function",
                                         "call."))
  expect_true(erg[[length(erg)]]$message == "Trailing blank lines are superfluous.")
})
