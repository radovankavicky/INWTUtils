context("Project Skeleton")

test_that("projectSkeleton does not produce any errors", {
  testthat::expect_error(projectSkeleton("../tmp1"), NA)
  testthat::expect_error(projectSkeleton("../tmp2", pkgName = "aTestPackage",
                                         pkgOnToplevel = TRUE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp3", pkgName = "aTestPackage",
                                         pkgOnToplevel = FALSE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp4", rProject = TRUE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp5",
                                         pkgName = "aTestPackage",
                                         pkgOnToplevel = TRUE,
                                         rProject = TRUE),
                         NA)
  testthat::expect_error(projectSkeleton("../tmp6",
                                         pkgName = "aTestPackage",
                                         pkgOnToplevel = FALSE,
                                         rProject = TRUE),
                         NA)
})

lapply(paste0("../tmp", 1:6), function(folder) unlink(folder, TRUE, TRUE))
