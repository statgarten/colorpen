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
#'

plotGen <- function(data, type, criteria, describe = NULL, options = NULL) {
  data <- data %>%
    as.data.frame() %>%
    convertFactor() # Dataframe 형식으로 변환; Factor 형식으로 변환

  f_aes <- function(x, y = NULL) {
    colour <- NULL
    if (is.null(y)) {
      return(
        aes_string(x = x)
      )
    }
    if (x != "NA") {
      colour <- x
    }
    # if("single_variable" %in% input$dev){
    #   return(
    #     aes_string(x = x, colour = colour)
    #   )
    # } else {
    #   return(
    #     aes_string(x = x, y = y, colour = colour)
    #   )
    # }
    return(
      aes_string(x = x, y = y)
    )
  }

  f_geom <- function(type) {
    univariate <- c("histogram")
    bivariate <- c("scatter", "line", "jitter")
    indexX <- which(data %>% colnames() == criteria)
    indexY <- which(data %>% colnames() == describe)
    vartypeX <- detectFactor(data)[indexX]
    vartypeY <- detectFactor(data)[indexY]

    if (is.na(describe) & type %in% univariate) {
      return(univariate(type, vartypeX))
    } else if (type %in% bivariate) {
      return(bivariate(type, vartypeX, vartypeY))
    }
  }

  g <- (data %>%
    ggplot(f_aes(x = criteria, y = describe)) +
    f_geom(type)) %>%
    f_options(options)

  return(g)
}
