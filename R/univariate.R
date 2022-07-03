#' @export

univariate <- function(type, vartypeX) {
  switch(type,
    "histogram" = {
      return(geom_histogram()) # binwidth 넣을 필요 있을수도
    },
    "area" = {
      return(geom_area(stat = "bin"))
    },
    "dot" = {
      return(geom_dotplot())
    },
    "freqpoly" = {
      return(geom_freqpoly())
    },
    "density" = {
      return(geom_density())
    }
  )
}
