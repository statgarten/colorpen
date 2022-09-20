#' @title plotGen
#' @export
#' @import shiny
#' @import dplyr
#' @import tidyverse
#' @import plotly
#'
#' @param data Data to describe in graph
#' @param type Type of graph
#' @param criteria X-axis variable in data
#' @param describe Y-axis variable in data
#' @param options Additional Options
#' @return A `plotly` graph.
#'

plotGen <- function(data, type, criteria, describe = NULL, options = NULL, wraptype = NULL, wrapcols = NULL, wraprows = NULL) {
  data <- data %>%
    as.data.frame() %>%
    convertFactor() # Dataframe 형식으로 변환; Factor 형식으로 변환

  exception <- c("pie", "donut")

  if (type %in% exception) {
    return(f_plot_ly(data, type, criteria, describe, options, wraptype, wrapcols, wraprows))
  }

  f_aes <- function(x, y = NULL) {
    if (type == "pie") {
      count <-
        return(aes_string(x = "", y = criteria, fill = criteria))
    }
    if (is.null(y)) {
      return(
        aes_string(x = x)
      )
    }
    return(
      aes_string(x = x, y = y)
    )
  }

  f_geom <- function(type) {
    indexX <- which(data %>% colnames() == criteria)
    indexY <- which(data %>% colnames() == describe)
    vartypeX <- detectFactor(data)[indexX]
    vartypeY <- detectFactor(data)[indexY]

    if (is.null(describe)) {
      return(univariate(type, vartypeX))
    } else {
      return(bivariate(type, vartypeX, vartypeY))
    }
  }

  g <- ggplotly((data %>%
    ggplot(f_aes(x = criteria, y = describe)) +
    f_geom(type)) %>%
    f_options(options) + f_wrap(wraptype, wrapcols, wraprows))

  return(g)
}
