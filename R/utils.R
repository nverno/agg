##' @name nonEmpty
##' @param x 
nonEmpty <- function(x) x[nzchar(x)]

## From Hadleys book somewhere
##' @name compact
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}
