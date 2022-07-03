#' @export

bivariate <- function(type, vartypeX, vartypeY) {
  switch(type,
    "scatter" = {
      return(geom_point())
    },
    "line" = {
      return(geom_line())
    },
    "jitter" = {
      return(geom_jitter())
    },
  )
}
