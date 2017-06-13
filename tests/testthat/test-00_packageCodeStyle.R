context("Package code style")

if (interactive()) {
  library(testthat)
  library(lintr)
  library(devtools)
}

test_that("Code is lint-free", {

  # Files to exclude from linter check
  excludedFiles <- c("testScript_args_without_default.R",
                     "testScript_checkStyle.R",
                     "doc/createProjectSkeleton.R",
                     "doc/checkCodeStyle.R")

  if (interactive()) load_all()

  exclusions <- lapply(excludedFiles, function(file) {
    system.file(file, package = "INWTUtils") %>%
      gsub(paste0(getwd(), "/"), "", .)
  })

  writeLines(c('linters: with_defaults()',
               paste0('exclusions: list("',
                      paste0(exclusions, collapse = '", "'),
                      '")'),
               'exclude: "# nolint"',
               'exclude_start: "# nolint start"',
               'exclude_end: "# nolint end"'),
             ".lintr")

  lintr:::read_settings(".lintr")

  expect_lint_free(linters = selectLinters())

  unlink(".lintr")
})


test_that("Example script is lint-free", {
  exampleScript <- system.file("exampleScript.R", package = "INWTUtils")
  expect_length(checkStyle(exampleScript, type = "script"), 0)
})
