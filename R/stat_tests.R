#' Vectorize T-test for the Property data
#'
#' @param x features of the Property dataframe
#'
#' @return vectorized t tests
#' @export
vectorize_ttest_for_property <- function(x) {
  t.test(unlist(property[, x]) ~ property$treat)
}