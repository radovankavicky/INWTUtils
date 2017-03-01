context("Project Skeleton")

tmpdir <- tempdir()

test_that("projectSkeleton does not produce any errors", {
  expect_error(projectSkeleton(paste0(tmpdir, "/tmp1")), NA)
  expect_error(projectSkeleton(paste0(tmpdir, "/tmp2/"),
                               pkgName = "aTestPackage",
                               pkgOnToplevel = TRUE),
               NA)
  expect_error(projectSkeleton(paste0(tmpdir, "/tmp3"),
                               pkgName = "aTestPackage",
                               pkgOnToplevel = FALSE),
               NA)
  expect_error(projectSkeleton(paste0(tmpdir, "/tmp4/"),
                               rProject = TRUE),
               NA)
  expect_error(projectSkeleton(paste0(tmpdir, "/tmp5"),
                               pkgName = "aTestPackage",
                               pkgOnToplevel = TRUE,
                               rProject = TRUE),
               NA)
  expect_error(projectSkeleton(paste0(tmpdir, "/tmp6/"),
                               pkgName = "aTestPackage",
                               pkgOnToplevel = FALSE,
                               rProject = TRUE),
               NA)
})


test_that("projectSkeleton creates correct files", {
  projectSkeleton(paste0(tmpdir, "/tmp7"),
                  pkgName = "aTestPackage",
                  pkgOnToplevel = FALSE,
                  rProject = TRUE)
  expect_true(file.exists(paste0(tmpdir, "/tmp7")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/data")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/libLinux/.gitignore")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/libWin/.gitignore")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/package/DESCRIPTION")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/package/tests/testthat.R")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/reports")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/rScripts")))
  expect_true(file.exists(paste0(tmpdir, "/tmp7/tmp7.Rproj")))
})


test_that("createProject creates correct files", {
  dir.create(paste0(tmpdir, "/tmp8"))

  createProject(pkg = FALSE, dir = paste0(tmpdir, "/tmp8"))
  expect_true(file.exists(paste0(tmpdir, "/tmp8/tmp8.Rproj")))
  expect_length(readLines(paste0(tmpdir, "/tmp8/tmp8.Rproj")), 13)
  unlink(paste0(tmpdir, "/tmp8/tmp8.Rproj"), TRUE, TRUE)

  createProject(pkg = TRUE, pkgOnToplevel = TRUE, paste0(tmpdir, "/tmp8"))
  expect_true(file.exists(paste0(tmpdir, "/tmp8/tmp8.Rproj")))
  expect_length(readLines(paste0(tmpdir, "/tmp8/tmp8.Rproj")), 18)
  expect_equal(readLines(paste0(tmpdir, "/tmp8/tmp8.Rproj"))[15],
               "BuildType: Package")
  unlink(paste0(tmpdir, "/tmp8/tmp8.Rproj", TRUE, TRUE))

  createProject(pkg = TRUE, pkgOnToplevel = FALSE, paste0(tmpdir, "/tmp8"))
  expect_true(file.exists(paste0(tmpdir, "/tmp8/tmp8.Rproj")))
  expect_length(readLines(paste0(tmpdir, "/tmp8/tmp8.Rproj")), 19)
  expect_equal(readLines(paste0(tmpdir, "/tmp8/tmp8.Rproj"))[15],
               "BuildType: Package")
  expect_equal(readLines(paste0(tmpdir, "/tmp8/tmp8.Rproj"))[17],
               "PackagePath: package")
})


test_that("createPackage creates correct files - pkg on top level", {
  dir.create(paste0(tmpdir, "/tmp9"))
  createPackage(dir = paste0(tmpdir, "/tmp9"), pkgName = "testPackage",
                pkgOnToplevel = TRUE)
  expect_true(file.exists(paste0(tmpdir, "/tmp9/.Rbuildignore")))
  expect_equal(readLines(paste0(tmpdir, "/tmp9/.Rbuildignore"))[3:4],
               c("lib*", "RScripts"))
  expect_true(file.exists(paste0(tmpdir, "/tmp9/DESCRIPTION")))
  expect_equal(readLines(paste0(tmpdir, "/tmp9/DESCRIPTION"))[1],
               "Package: testPackage")
  expect_true(file.exists(paste0(tmpdir, "/tmp9/NAMESPACE")))
  expect_true(file.exists(paste0(tmpdir, "/tmp9/R")))
  expect_true(file.exists(paste0(tmpdir, "/tmp9/tests/testthat.R")))
})


test_that("createPackage creates correct files - pkg in own folder", {
  dir.create(paste0(tmpdir, "/tmp0"))
  createPackage(dir = paste0(tmpdir, "/tmp0"), pkgName = "testPackage",
                pkgOnToplevel = FALSE)
  expect_true(file.exists(paste0(tmpdir, "/tmp0/package/.Rbuildignore")))
  expect_equal(readLines(paste0(tmpdir, "/tmp0/package/.Rbuildignore"))[3:4],
               c("lib*", "RScripts"))
  expect_true(file.exists(paste0(tmpdir, "/tmp0/package/DESCRIPTION")))
  expect_equal(readLines(paste0(tmpdir, "/tmp0/package/DESCRIPTION"))[1],
               "Package: testPackage")
  expect_true(file.exists(paste0(tmpdir, "/tmp0/package/NAMESPACE")))
  expect_true(file.exists(paste0(tmpdir, "/tmp0/package/R")))
  expect_true(file.exists(paste0(tmpdir, "/tmp0/package/tests/testthat.R")))
})


test_that("packages created with projectSkeleton can be built", {
  projectSkeleton(paste0(tmpdir, "/tmp10/"),
                  pkgName = "aTestPackage",
                  pkgOnToplevel = TRUE)
  expect_error(build(pkg = paste0(tmpdir, "/tmp10"),
                     path = paste0(tmpdir, "/tmp10")),
               NA)
  expect_warning(build(pkg = paste0(tmpdir, "/tmp10"),
                       path = paste0(tmpdir, "/tmp10")),
                 NA)
  projectSkeleton(paste0(tmpdir, "/tmp11"),
                  pkgName = "aTestPackage",
                  pkgOnToplevel = FALSE)
  expect_error(build(pkg = paste0(tmpdir, "/tmp11/package"),
                     path = paste0(tmpdir, "/tmp11")),
               NA)
  expect_warning(build(pkg = paste0(tmpdir, "/tmp11/package"),
                       path = paste0(tmpdir, "/tmp11")),
                 NA)
  projectSkeleton(paste0(tmpdir, "/tmp12"),
                  pkgName = "aTestPackage",
                  pkgOnToplevel = TRUE,
                  rProject = TRUE)
  expect_error(build(pkg = paste0(tmpdir, "/tmp12"),
                     path = paste0(tmpdir, "/tmp12")),
               NA)
  expect_warning(build(pkg = paste0(tmpdir, "/tmp12"),
                       path = paste0(tmpdir, "/tmp12")),
                 NA)
  projectSkeleton(paste0(tmpdir, "/tmp13/"),
                  pkgName = "aTestPackage",
                  pkgOnToplevel = FALSE,
                  rProject = TRUE)
  expect_error(build(pkg = paste0(tmpdir, "/tmp13/package"),
                     path = paste0(tmpdir, "/tmp13")),
               NA)
  expect_warning(build(pkg = paste0(tmpdir, "/tmp13/package"),
                       path = paste0(tmpdir, "/tmp13")),
                 NA)
})
