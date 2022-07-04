#' @export

univariate <- function(type, vartypeX) {
  if (vartypeX == "Factor") {
    switch(type,
      "bar" = {
        return(geom_bar())
      },
      "histogram" = {
        return(geom_histogram()) # binwidth 넣을 필요 있을수도
      },
      "dot" = {
        return(geom_dotplot())
      },
      "pie" = {
        return(
          geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0)
        )
      }
    )
  } else if (vartypeX == "Continuous") {
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
}
