################################################################################
# This script writes an introduction text and selected vignettes the README.md #
# file.                                                                        #
#                                                                              #
# Author: Mira Céline Klein                                                    #
# E-mail: mira.klein@inwt-statistic.de                                         #
################################################################################

library(dplyr)

devtools::build_vignettes()

themas <- c("createProjectSkeleton", "checkCodeStyle")

for (thema in themas) {

  # Create md files of vignettes
  knitr::knit(paste0("inst/doc/", thema, ".Rmd"),
              paste0("inst/", thema, ".md"))
  text <- readLines(paste0("inst/", thema, ".md"))

  # Add folder "vignette" to image paths
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
          # Introduction
          "This repository contains a package which provides utility functions",
          "used by the INWT Statistics GmbH. This includes amongst others",
          "functions to create a file structure for new projects, to check",
          "code for violations of style conventions and to keep the searchpath",
          "clean. In addition, an example R script is included.",
          createProjectSkeleton,
          checkCodeStyle)

writeLines(text, "README.md")

unlink(c(paste0("inst/", themas, ".md"), "inst/doc"), recursive = TRUE)
