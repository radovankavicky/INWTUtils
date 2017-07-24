#' Format Big Numbers
#' @description Wrapper around \code{\link[base]{formatC}}
#'
#' @param x numeric
#' @param digits integer
#' @param bigMark character: thousand separator
#'
#' @export
#'
#' @examples
#' formatBigNumbers(1234567.123, digits = 2)
#' formatBigNumbers(1234567, bigMark = " ")
formatBigNumbers <- function(x, bigMark = "\\\\,", digits = 0) {
  formatC(x,
          big.mark = bigMark,
          format = "f",
          digits = digits)
}


#' Create percentage from share
#'
#' @description Make a nice-looking percentage out of a share.
#'
#' @param x numeric: a share
#' @param digits integer: number of decimal places for the percentage
#' @param percSymbol logical: return the "\%" symbol?
#' @param keepTrailing0 logical: keep trailing zeros (decimals) to have the same
#' length for all numbers?
#'
#' @return Character
#'
#' @export
#'
#' @examples makePct(0.25)
#' makePct(1.251, percSymbol = FALSE)
#' makePct(0.005, digits = 2)
#' makePct(0.26, digits = 2, keepTrailing0 = FALSE)
#' makePct(0.26, digits = 2, keepTrailing0 = TRUE)
makePct <- function(x, digits = 0, percSymbol = TRUE, keepTrailing0 = TRUE) {
  res <- (100 * x) %>%
    round(digits = digits)
  if (keepTrailing0) res <- formatC(res, digits = digits, format = "f")
  if (percSymbol) res <- paste0(res, "%")
  return(res)
}
