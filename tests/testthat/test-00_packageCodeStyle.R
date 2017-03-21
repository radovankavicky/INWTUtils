context("Package code style")

test_that("Code is lint-free", {

  # Files to exclude from linter check
  excludedFiles <- c("testScript_args_without_default_first_linter.R",
                     "exampleScript.R",
                     "testScript_checkStyle.R")

  exclusions <- as.list(paste0(if (interactive()) "inst/" else "",
                               excludedFiles))

  writeLines(c('linters: with_defaults()',
               paste0('exclusions: list("',
                      paste0(exclusions, collapse = '", "'),
                      '")'),
               'exclude: "# nolint"',
               'exclude_start: "# nolint start"',
               'exclude_end: "# nolint end"'),
             ".lintr")

  lintr:::read_settings(".lintr")

  expect_lint_free(linters = selectLntrs())

  unlink(".lintr")
})
