#' @export

univariate <- function(type){
  switch(
    type,
    "histogram" = {
      return(geom_histogram())
    }
  )
}
