#' @export
#' @import plotly
#' @import dplyr

# For exceptions
f_plot_ly <- function(data, type, criteria, describe = NULL, options = NULL, wraptype = NULL, wrapcols = NULL, wraprows = NULL) {
  switch(type,
    "pie" = {
      return(plot_ly(
        data,
        type = type,
        labels = ~ get(criteria),
        values = ~ get(criteria)
      ))
    }
  )
}
