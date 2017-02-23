context("Project Skeleton")

test_that("projectSkeleton does not produce any errors", {
  expect_error(projectSkeleton("../tmp1"), NA)
  expect_error(projectSkeleton("../tmp2/", pkgName = "aTestPackage",
                               pkgOnToplevel = TRUE),
               NA)
  expect_error(projectSkeleton("../tmp3", pkgName = "aTestPackage",
                               pkgOnToplevel = FALSE),
               NA)
  expect_error(projectSkeleton("../tmp4/", rProject = TRUE),
               NA)
  expect_error(projectSkeleton("../tmp5",
                               pkgName = "aTestPackage",
                               pkgOnToplevel = TRUE,
                               rProject = TRUE),
               NA)
  expect_error(projectSkeleton("../tmp6/",
                               pkgName = "aTestPackage",
                               pkgOnToplevel = FALSE,
                               rProject = TRUE),
               NA)
})

lapply(paste0("../tmp", 1:6), function(folder) unlink(folder, TRUE, TRUE))



projectSkeleton("../tmp",
                pkgName = "aTestPackage",
                pkgOnToplevel = FALSE,
                rProject = TRUE)

test_that("projectSkeleton creates correct files", {
  expect_true(file.exists("../tmp"))
  expect_true(file.exists("../tmp/data"))
  expect_true(file.exists("../tmp/libLinux/.gitignore"))
  expect_true(file.exists("../tmp/libWin/.gitignore"))
  expect_true(file.exists("../tmp/package/DESCRIPTION"))
  expect_true(file.exists("../tmp/package/tests/testthat.R"))
  expect_true(file.exists("../tmp/reports"))
  expect_true(file.exists("../tmp/rScripts"))
  expect_true(file.exists("../tmp/tmp.Rproj"))
})

unlink("../tmp", TRUE, TRUE)
