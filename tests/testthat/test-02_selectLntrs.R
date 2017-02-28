context("Function returning a list of linters")

test_that("selectLntrs returns a list of functions", {
  expect_type(selectLntrs(), "list")
  expect_true(lapply(selectLntrs(), function(x) class(x) == "function") %>%
                unlist %>%
                all)
  expect_type(selectLntrs("pkgFuns"), "list")
  expect_true(lapply(selectLntrs("pkgFuns"),
                     function(x) class(x) == "function") %>%
                unlist %>%
                all)
  expect_type(selectLntrs("script"), "list")
  expect_true(lapply(selectLntrs("script"),
                     function(x) class(x) == "function") %>%
                unlist %>%
                all)
})

test_that("selectLntrs returns correct linters for doc type", {
  expect_length(intersect(names(selectLntrs()),
                          names(INWTUtils:::pkgFunLntrs())),
                0)
  expect_length(intersect(names(selectLntrs()),
                          names(INWTUtils:::scriptLntrs())),
                0)
  expect_true(all(names(INWTUtils:::pkgFunLntrs()) %in%
                    names(selectLntrs("pkgFuns"))))
  expect_true(all(names(INWTUtils:::scriptLntrs()) %in%
                    names(selectLntrs("script"))))
  expect_equal(selectLntrs("pkgFuns") %>% names %>% sort,
               union(selectLntrs() %>% names,
                     INWTUtils:::pkgFunLntrs() %>% names) %>% sort)
  expect_equal(selectLntrs("script") %>% names %>% sort,
               union(selectLntrs() %>% names,
                     INWTUtils:::scriptLntrs() %>% names) %>% sort)
})
