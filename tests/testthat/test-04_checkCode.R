context("Function checkStyle()")

test_that("checkStyle returns correct type", {
  erg <- checkStyle(system.file("testScript_checkStyle.R",
                                package = "INWTUtils"),
                    type = "script")
  expect_s3_class(erg, "lints")
  expect_true(lapply(erg, class) %>% unlist %>% `==`("lint") %>% all)
})


test_that("checkStyle returns correct content", {
  erg <- checkStyle(system.file("testScript_checkStyle.R",
                                package = "INWTUtils"), type = "script")
  expect_true(erg[[1]]$message == "Double whitespace.")
  expect_true(erg[[2]]$message == "Put spaces around all infix operators.")
})
