#' @export
#' @import cli
bivariate <- function(type, vartypeX, vartypeY) {
  cli_alert_success("bivariate type: {type} vartypeX: {vartypeX} vartypeY: {vartypeY}")
  if (vartypeX == "Factor" & vartypeY == "Factor") {
    cli_alert_success("X: Factor Y: Factor")
    switch(type,
      "count" = {
        return(geom_count())
      },
      "jitter" = {
        return(geom_jitter())
      },
      "heatmap" = {
        return(geom_tile())
      }
    )
  } else if (vartypeX == "Factor" & vartypeY == "Continuous") {
    cli_alert_success("X: Factor Y: Continuous")
    switch(type,
      "col" = {
        return(geom_col())
      },
      "box" = {
        return(geom_boxplot())
      },
      "dot" = { # Error in `check_required_aesthetics()`:! geom_dotplot requires the following missing aesthetics: y
        return(geom_dotplot())
      },
      "violin" = {
        return(geom_violin())
      },
    )
  } else if (vartypeX == "Continuous" & vartypeY == "Factor") {
    cli_alert_success("X: Continuous Y: Factor")
  } else { # 모두 Continuous #success
    cli_alert_success("X: Continuous Y: Continuous")
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
