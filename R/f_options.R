#' @export

f_options <- function(g, options){
  if("scaleToLog10" %in% options){
    g <- g + scale_x_log10()
  }

  return(g)
}
