#' @export

source("R/detectFactor.R")

convertFactor <- function(data, order = NULL) {
  if(is.null(order)) order <- detectFactor(data)
  for(col in 1:(length(order))) if(order[col] == "Factor") data[, col] <- as.factor(data[, col])

  return(data)
}
