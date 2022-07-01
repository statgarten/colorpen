#' @export
#' @import shiny
#' @import dplyr
#' @import tidyverse
#' @import plotly
#'
#'@param data Data to describe in graph
#'@param type Type of graph
#'@param criteria X-axis variable in data
#'@param describe Y-axis variable in data
#'@param options Additional Options

plotGen <- function(data, type, criteria, describe = NULL, options){
  data <- data %>% as.data.frame()

  f_aes <- function(x, y){
    colour <- NULL
    if(x != 'NA') {
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

  f_geom <- function(){

  }

  g <- data %>%
    ggplot(f_aes(x = criteria, y = describe)) +
    geom_point()


  return(g)
}
