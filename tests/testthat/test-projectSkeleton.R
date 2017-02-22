context("Project Skeleton")



test_that("projectSkeleton does not produce any errors", {
  testthat::expect_error(projectSkeleton("../tmp1"), NA)
  testthat::expect_error(projectSkeleton("../tmp2/", pkgName = "aTestPackage",
                                         pkgOnToplevel = TRUE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp3", pkgName = "aTestPackage",
                                         pkgOnToplevel = FALSE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp4/", rProject = TRUE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp5",
                                         pkgName = "aTestPackage",
                                         pkgOnToplevel = TRUE,
                                         rProject = TRUE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp6/",
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
  testthat::expect_true(file.exists("../tmp"))
  testthat::expect_true(file.exists("../tmp/data"))
  testthat::expect_true(file.exists("../tmp/libLinux/.gitignore"))
  testthat::expect_true(file.exists("../tmp/libWin/.gitignore"))
  testthat::expect_true(file.exists("../tmp/package/DESCRIPTION"))
  testthat::expect_true(file.exists("../tmp/package/tests/testthat.R"))
  testthat::expect_true(file.exists("../tmp/reports"))
  testthat::expect_true(file.exists("../tmp/rScripts"))
  testthat::expect_true(file.exists("../tmp/tmp.Rproj"))
})

unlink("../tmp", TRUE, TRUE)
