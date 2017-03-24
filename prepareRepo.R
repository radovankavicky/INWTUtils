devtools::build_vignettes()

for (thema in c("createProjectSkeleton", "checkCodeStyle")) {

  knitr::knit(paste0("inst/doc/", thema, ".Rmd"),
              paste0("inst/", thema, ".md"))
  text <- readLines(paste0("inst/", thema, ".md"))

  # Add "vignette" to image paths
  picLines <- grep("\\(\\w+\\.PNG", text, value = FALSE)
  text[picLines] <- gsub("\\(", "(vignettes/", x = text[picLines])

  # Remove YAML header, add title
  title <- text[2] %>%
    gsub("title: \"", "", .) %>%
    gsub("\"", "", .) %>%
    paste("#", .)
  text <- c("", "", title, text[-(1:10)])

  assign(thema, text)

}

text <- c("# INWTUtils",
          "This repository contains a package which provides utility functions",
          "used by the INWT Statistics GmbH. This includes amongst others",
          "functions to create a file structure for new projects, to check",
          "code for violations of style conventions and to keep the searchpath",
          "clean. In addition, an example R script is included.",
          createProjectSkeleton,
          checkCodeStyle)

writeLines(text, "README.md")
