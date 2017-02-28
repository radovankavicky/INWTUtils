context("Package code style")

test_that("Code is lint-free", {
  lintr::expect_lint_free(linters = INWTUtils::selectLntrs())
})
