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


dir.create("../tmp")
test_that("createProject creates correct files", {

  createProject(pkg = FALSE, dir = "../tmp")
  expect_true(file.exists("../tmp/tmp.Rproj"))
  expect_length(readLines("../tmp/tmp.Rproj"), 13)
  unlink("../tmp/tmp.Rproj", TRUE, TRUE)

  createProject(pkg = TRUE, pkgOnToplevel = TRUE, "../tmp")
  expect_true(file.exists("../tmp/tmp.Rproj"))
  expect_length(readLines("../tmp/tmp.Rproj"), 18)
  expect_equal(readLines("../tmp/tmp.Rproj")[15], "BuildType: Package")
  unlink("../tmp/tmp.Rproj", TRUE, TRUE)

  createProject(pkg = TRUE, pkgOnToplevel = FALSE, "../tmp")
  expect_true(file.exists("../tmp/tmp.Rproj"))
  expect_length(readLines("../tmp/tmp.Rproj"), 19)
  expect_equal(readLines("../tmp/tmp.Rproj")[15], "BuildType: Package")
  expect_equal(readLines("../tmp/tmp.Rproj")[17], "PackagePath: package")
  unlink("../tmp/tmp.Rproj", TRUE, TRUE)

})
unlink("../tmp", TRUE, TRUE)


dir.create("../tmp")
test_that("createPackage creates correct files - pkg on top level", {
  createPackage(dir = "../tmp", pkgName = "testPackage", pkgOnToplevel = TRUE)
  expect_true(file.exists("../tmp/.Rbuildignore"))
  expect_equal(readLines("../tmp/.Rbuildignore")[3:4],
               c("lib*", "RScripts"))
  expect_true(file.exists("../tmp/DESCRIPTION"))
  expect_equal(readLines("../tmp/DESCRIPTION")[1], "Package: testPackage")
  expect_true(file.exists("../tmp/NAMESPACE"))
  expect_true(file.exists("../tmp/R"))
  expect_true(file.exists("../tmp/tests/testthat.R"))
})
unlink("../tmp", TRUE, TRUE)


dir.create("../tmp")
test_that("createPackage creates correct files - pkg in own folder", {
  createPackage(dir = "../tmp", pkgName = "testPackage", pkgOnToplevel = FALSE)
  expect_true(file.exists("../tmp/package/.Rbuildignore"))
  expect_equal(readLines("../tmp/package/.Rbuildignore")[3:4],
               c("lib*", "RScripts"))
  expect_true(file.exists("../tmp/package/DESCRIPTION"))
  expect_equal(readLines("../tmp/package/DESCRIPTION")[1], "Package: testPackage")
  expect_true(file.exists("../tmp/package/NAMESPACE"))
  expect_true(file.exists("../tmp/package/R"))
  expect_true(file.exists("../tmp/package/tests/testthat.R"))
})
unlink("../tmp", TRUE, TRUE)
