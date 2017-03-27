context("Function returning a list of linters")

test_that("selectLntrs returns a list of functions", {

  erg <- selectLntrs()
  expect_type(erg, "list")
  expect_true(lapply(erg, function(x) class(x) == "function") %>%
                unlist %>% all)
  rm(erg)

  erg <- selectLntrs("pkgFuns")
  expect_type(erg, "list")
  expect_true(lapply(erg,
                     function(x) class(x) == "function") %>%
                unlist %>% all)
  rm(erg)

  erg <- selectLntrs("script")
  expect_type(erg, "list")
  expect_true(lapply(erg,
                     function(x) class(x) == "function") %>%
                unlist %>% all)
  rm(erg)

})

test_that("selectLntrs returns correct linters for doc type", {
  expect_length(intersect(names(selectLntrs()),
                          names(INWTUtils:::pkgFunLntrs())),
                0)
  expect_length(intersect(names(selectLntrs()),
                          names(INWTUtils:::scriptLntrs())),
                0)
  expect_length(intersect(names(INWTUtils:::pkgFunLntrs()),
                          names(INWTUtils:::scriptLntrs())),
                0)
  expect_true(all(names(INWTUtils:::pkgFunLntrs()) %in%
                    names(selectLntrs("pkgFuns"))))
  expect_true(all(names(INWTUtils:::scriptLntrs()) %in%
                    names(selectLntrs("script"))))
  expect_equal(selectLntrs("pkgFuns") %>% names %>% sort,
               union(INWTUtils:::generalLntrs() %>% names,
                     INWTUtils:::pkgFunLntrs() %>% names) %>% sort)
  expect_equal(selectLntrs("script") %>% names %>% sort,
               union(INWTUtils:::generalLntrs() %>% names,
                     INWTUtils:::scriptLntrs() %>% names) %>% sort)
})


test_that("selectLntrs: arguments addLinters and exludeLinters", {

  # selectLntrs with default args
  expect_equal(selectLntrs() %>% names %>% sort,
               names(INWTUtils:::generalLntrs()) %>% sort)

  # Exclude linters
  excl <- c("assignment_linter", "line_length_linter")
  erg <- selectLntrs(excludeLinters = excl)
  expect_equal(names(erg) %>% sort,
               setdiff(names(INWTUtils:::generalLntrs()), excl) %>% sort)
  rm(erg, excl)

  # Add linters
  add <- c(absolute_paths_linter = absolute_paths_linter,
           object_usage_linter = object_usage_linter)
  erg <- selectLntrs(addLinters = add)
  expect_equal(names(erg) %>% sort,
               union(names(INWTUtils:::generalLntrs()), names(add)) %>% sort)
  rm(erg)

  # Combination: exclude and add linters
  excl <- c("assignment_linter", "absolute_paths_linter")
  erg <- selectLntrs(excludeLinters = excl, addLinters = add)
  expect_equal(names(erg) %>% sort,
               c(names(INWTUtils:::generalLntrs()), names(add)) %>%
                 setdiff(excl) %>%
                 sort)
  rm(erg, excl)

  # Combination: type, add, exclude
  excl <- c("object_usage_linter", "setwd_linter")
  erg <- selectLntrs(type = "pkgFuns", addLinters = add, excludeLinters = excl)
  expect_equal(names(erg) %>% sort,
               c(names(INWTUtils:::generalLntrs()),
                 names(INWTUtils:::pkgFunLntrs()),
                 names(add)) %>%
                 setdiff(excl) %>% unique %>% sort)

})


test_that("selectLntrs returns every linter only once", {
  add <- list(setwd_linter = setwd_linter,
              assignment_linter = assignment_linter)
  expect_equal(selectLntrs(type = "pkgFuns",
                           addLinters = add) %>%
                 names %>% sort,
               c(names(INWTUtils:::generalLntrs()),
                 names(INWTUtils:::pkgFunLntrs()),
                 names(add)) %>% unique %>% sort)
})
