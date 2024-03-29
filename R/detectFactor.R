#' @title detectFacotr
#' @export
#' @return A vector including whether column is Discrete or Continuous variable.

detectFactor <- function(data) {
  return(
    sapply(1:ncol(data), function(x) {
      if (length(unique(data[, x])) > 7) {
        return("Continuous")
      } else {
        return("Factor")
      }
    }) %>%
      as.factor()
  )
}
