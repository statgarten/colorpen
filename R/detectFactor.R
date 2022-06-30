#' @export

detectFactor <- function(data){
  return(
    sapply(1:ncol(data), function(x){
      if(length(unique(data[, x])) > 4){
        return("Continuous")
      } else {
        return("Factor")
      }
    }) %>%
      as.factor()
  )
}
