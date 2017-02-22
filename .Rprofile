.First <- function() {
  if (grepl("Windows", Sys.getenv("OS"))) {
    .libPaths(new = c(paste(getwd(), "libWin", sep = "/"), .libPaths()))
  } else {
    .libPaths(new = c(paste(getwd(), "libLinux", sep = "/"), .libPaths()))
  }
}

.First()
