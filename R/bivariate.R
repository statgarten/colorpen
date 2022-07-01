#' @export

bivariate <- function(type){
  switch(
    type,
    "scatter" = {
      return(geom_point())
    },
    "line" = {
      return(geom_line())
    },
    "jitter" = {
      return(geom_jitter())
    }
  )
}
