#' Random sample of rows from a data.table
#'
#' @param DT A data.table
#' @param n  Number of rows to sample (will never exceed DT size)
#' @return A data.table with up to n randomly sampled rows
#'
rsample  <- function(DT, n) {
  stopifnot(data.table::is.data.table(DT))
  DT[sample(.N, min(.N, n))]
}
