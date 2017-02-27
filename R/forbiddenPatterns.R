# Patterns that are never allowed
forbiddenTextAll <- function() {
  c(doubleWhitespace = "[^( | ')]  ")
}

# Patterns that are not allowed in scripts
forbiddenTextScript <- function() {
  c(accessInternalFunction = "INWT\\w*:::",
    forbiddenTextAll())
}


# Patterns that are not allowed in the function code in R packages
forbiddenTextPkgFuns <- function() {
  c(setwd = "setwd",
    library = "library",
    sourcedFiles = "([^.]|^)source",
    argsWithoutDefaultFirst =
      "function\\(.*=.*,[\n]?[ ]*[A-z0-9_\\.]*[^...][\\),]",
    forbiddenTextAll())
}
