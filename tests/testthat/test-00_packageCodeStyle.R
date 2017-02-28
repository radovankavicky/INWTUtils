context("Package code style")

test_that("Code is lint-free", {
  expect_lint_free(linters = selectLntrs())
})
