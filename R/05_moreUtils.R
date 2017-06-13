#' Read csv file checking for correct row number
#'
#' @description Sometimes \code{\link[readr]{read_csv}} stops reading in a csv
#' file after several lines without any message. This function is a wrapper
#' around \code{\link[readr]{read_csv}} that counts the number of rows in the
#' original file and compares it with the resulting data.frame. If the numbers
#' of rows differ, it throws a warning.
#'
#' @inheritParams readr::read_csv
#' @param ... arguments passed to \code{\link[readr]{read_csv}}
#'
#' @examples
#' write.csv(x = iris, file = "iris.csv")
#' read_csv(file = "iris.csv")
#' read_csv(file = "iris.csv", skip = 3, col_names = FALSE, n_max = 10)
#'
#' @export
read_csv <- function(file, col_names = TRUE, skip = 0, n_max = Inf, ...) {

  col_names <- if (!exists("col_names")) TRUE else col_names
  skip <- if (!exists("skip")) 0 else skip
  n_max <- if (!exists("n_max")) Inf else n_max

  df <- readr::read_csv(file = file,
                        col_names = col_names,
                        skip = skip,
                        n_max = n_max,
                        ...)

  nRowsInFile <- system(paste0("wc -l ", file), intern = TRUE) %>%
    strsplit(split = " ") %>% unlist %>% `[[`(1) %>% as.integer
  target <- min(n_max, nRowsInFile - (col_names != FALSE) - skip)
  actual <- nrow(df)

  if (target != actual) {
    warning(paste0("Wrong number of rows may have been imported by read_csv.",
                   " Should be ", target,
                   " rows, but resulting data.frame has ", actual, ". ",
                   "Please use fread from the data.table package instead."))
  } else {
    message("Correct number of lines has been imported (", actual, ").")
    return(df)
  }
}

# Ausweitung auf read_csv2, ...
# Was ist mit Win?
