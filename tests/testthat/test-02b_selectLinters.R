context("Function returning a list of linters")

test_that("selectLinters returns a list of functions", {

  erg <- selectLinters()
  expect_type(erg, "list")
  expect_true(lapply(erg, function(x) class(x) == "function") %>%
                unlist %>% all)
  rm(erg)

  erg <- selectLinters("pkgFuns")
  expect_type(erg, "list")
  expect_true(lapply(erg,
                     function(x) class(x) == "function") %>%
                unlist %>% all)
  rm(erg)

  erg <- selectLinters("script")
  expect_type(erg, "list")
  expect_true(lapply(erg,
                     function(x) class(x) == "function") %>%
                unlist %>% all)
  rm(erg)

})

test_that("selectLinters returns correct linters for doc type", {
  expect_length(intersect(names(selectLinters()),
                          names(INWTUtils:::pkgFunLinters())),
                0)
  expect_length(intersect(names(selectLinters()),
                          names(INWTUtils:::scriptLinters())),
                0)
  expect_length(intersect(names(INWTUtils:::pkgFunLinters()),
                          names(INWTUtils:::scriptLinters())),
                0)
  expect_true(all(names(INWTUtils:::pkgFunLinters()) %in%
                    names(selectLinters("pkgFuns"))))
  expect_true(all(names(INWTUtils:::scriptLinters()) %in%
                    names(selectLinters("script"))))
  expect_equal(selectLinters("pkgFuns") %>% names %>% sort,
               union(INWTUtils:::generalLinters() %>% names,
                     INWTUtils:::pkgFunLinters() %>% names) %>% sort)
  expect_equal(selectLinters("script") %>% names %>% sort,
               union(INWTUtils:::generalLinters() %>% names,
                     INWTUtils:::scriptLinters() %>% names) %>% sort)
})


test_that("selectLinters: arguments addLinters and exludeLinters", {

  # selectLinters with default args
  expect_equal(selectLinters() %>% names %>% sort,
               names(INWTUtils:::generalLinters()) %>% sort)

  # Exclude linters
  excl <- c("assignment_linter", "line_length_linter")
  erg <- selectLinters(excludeLinters = excl)
  expect_equal(names(erg) %>% sort,
               setdiff(names(INWTUtils:::generalLinters()), excl) %>% sort)
  rm(erg, excl)

  # Add linters
  add <- c(absolute_paths_linter = absolute_paths_linter,
           object_usage_linter = object_usage_linter)
  erg <- selectLinters(addLinters = add)
  expect_equal(names(erg) %>% sort,
               union(names(INWTUtils:::generalLinters()), names(add)) %>% sort)
  rm(erg)

  # Combination: exclude and add linters
  excl <- c("assignment_linter", "absolute_paths_linter")
  erg <- selectLinters(excludeLinters = excl, addLinters = add)
  expect_equal(names(erg) %>% sort,
               c(names(INWTUtils:::generalLinters()), names(add)) %>%
                 setdiff(excl) %>%
                 sort)
  rm(erg, excl)

  # Combination: type, add, exclude
  excl <- c("object_usage_linter", "setwd_linter")
  erg <- selectLinters(type = "pkgFuns", addLinters = add, excludeLinters = excl)
  expect_equal(names(erg) %>% sort,
               c(names(INWTUtils:::generalLinters()),
                 names(INWTUtils:::pkgFunLinters()),
                 names(add)) %>%
                 setdiff(excl) %>% unique %>% sort)

})


test_that("selectLinters returns every linter only once", {
  add <- list(setwd_linter = setwd_linter,
              assignment_linter = assignment_linter)
  expect_equal(selectLinters(type = "pkgFuns",
                           addLinters = add) %>%
                 names %>% sort,
               c(names(INWTUtils:::generalLinters()),
                 names(INWTUtils:::pkgFunLinters()),
                 names(add)) %>% unique %>% sort)
})
