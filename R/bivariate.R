#' @export

bivariate <- function(type, vartypeX, vartypeY) {
  if (vartypeX == "Factor" & vartypeY == "Factor") {
    switch(type,
      "count" = {
        return(geom_count())
      },
      "jitter" = {
        return(geom_jitter())
      },
    )
  } else if (vartypeX == "Factor" & vartypeY == "Continuous") {
    switch(type,
      "col" = {
        return(geom_col())
      },
      "box" = {
        return(geom_boxplot())
      },
      "dot" = {
        return(geom_dotplot())
      },
      "violin" = {
        return(geom_violin())
      },
    )
  } else if (vartypeX == "Continuous" & vartypeY == "Factor") {

  } else {
    switch(type,
      "scatter" = {
        return(geom_point())
      },
      "line" = {
        return(geom_line())
      },
      "quantile" = {
        return(geom_quantile())
      },
      "lm" = {
        return(geom_smooth(method = lm))
      },
      "area" = {
        return(geom_area())
      },
      "line" = {
        return(geom_line())
      },
      "step" = {
        return(geom_step(direction = "hv")) # direction of stairs: 'vh' for vertical then horizontal, 'hv' for horizontal then vertical, or 'mid' for step half-way between adjacent x-values.
      }
    )
  }
}
