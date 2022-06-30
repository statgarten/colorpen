#' @export
#' @import shiny
#' @import dplyr
#' @import tidyverse
#' @import plotly
#'

plotGen <- function(data, criteria, description = NULL){
  f_aes <- function(x, y){
    colour <- NULL
    if(criteria != 'NA') {
      colour <- criteria
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

  g <-data %>%
        ggplot(f_aes(x = criteria, y = description)) +
        geom_point()

  return(g)
}
