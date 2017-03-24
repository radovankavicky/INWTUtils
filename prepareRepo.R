devtools::build_vignettes()

for (thema in c("createProjectSkeleton", "checkCodeStyle")) {
  knitr::knit(paste0("inst/doc/", thema, ".Rmd"),
              paste0("inst/", thema, ".md"))
  assign(thema, readLines(paste0("inst/", thema, ".md")))
}

getTitle <- function(lines) {
  lines[2] %>% gsub("title: \"", "", .) %>% gsub("\"", "", .) %>% paste("#", .)
}

text <- c("# INWTUtils",
          paste("This repository contains a package which provides utility",
                "functions used by INWT. This includes amongst others",
                "functions to create a file structure for new projects, to",
                "check code for violations of style conventions and to keep",
                "the searchpath clean. In addition, an example R script is",
                "included."),
          "",
          "",
          getTitle(createProjectSkeleton),
          createProjectSkeleton[-(1:10)],
          "",
          "",
          getTitle(checkCodeStyle),
          checkCodeStyle[-(1:10)])

writeLines(text, "README.md")
