context("Function checkStyle()")

# nolint start
writeLines(text = c("# This is an example file  for the checkStyle function",
                    "x <- 1:5",
                    "makeDouble <- function(vec) 2*vec",
                    "",
                    "y <- makeDouble(x)"),
           # nolint end
           con = paste0(tempdir(), "/checkCodeExample.txt"))


test_that("checkStyle returns correct type", {
  erg <- checkStyle(paste0(tempdir(), "/checkCodeExample.txt"), type = "script")
  expect_s3_class(erg, "lints")
  expect_true(lapply(erg, class) %>% unlist %>% `==`("lint") %>% all)
})


test_that("checkStyle returns correct content", {
  erg <- checkStyle(paste0(tempdir(), "/checkCodeExample.txt"), type = "script")
  expect_true(erg[[1]]$message == "Double whitespace.")
  expect_true(erg[[2]]$message == "Put spaces around all infix operators.")
})
