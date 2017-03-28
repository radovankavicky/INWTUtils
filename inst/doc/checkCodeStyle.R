## ---- echo = FALSE-------------------------------------------------------
library(INWTUtils)

## ----badStyleScript, eval = FALSE----------------------------------------
#  writeLines(c("# This is an example for bad style",
#               "x = 1+2",
#               "# A comment with  double  spaces",
#               "foo <- function( y) print(paste('You entered', y))",
#               ""),
#             con = "badStyle1.R")
#  writeLines(c("# This is a second example   ",
#               "z<-c(1,2)"),
#             con = "badStyle2.R")

## ----checkStyle, results = "hide", eval = FALSE--------------------------
#  checkStyle(files = c("badStyle1.R", "badStyle2.R"),
#             type = "script")

## ----lintersAlways, results = 'asis', echo = FALSE-----------------------
cat("-", paste0(names(INWTUtils:::generalLinters()), collapse = "\n\n- "))

## ----lintersScript, results = 'asis', echo = FALSE-----------------------
cat("-", paste0(names(INWTUtils:::scriptLinters()), collapse = "\n\n- "))

## ----lintersFuns, results = 'asis', echo = FALSE-------------------------
cat("-", paste0(names(INWTUtils:::pkgFunLinters()), collapse = "\n\n- "))

## ----nolint, eval = FALSE------------------------------------------------
#  # nolint start
#  x <- c(1,2) # This line will be excluded from the checks
#  # nolint end
#  y <- c(3, 4) # This line won't be excluded anymore.

## ----rmFiles, echo = FALSE, results = 'hide', eval = FALSE---------------
#  lapply(c("badStyle1.R", "badStyle2.R"), unlink)

