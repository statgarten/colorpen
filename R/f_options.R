#' @export

f_options <- function(g, options) {
  # Scale options
  if ("XScaleToLog10" %in% options) {
    g <- g + scale_x_log10()
  }
  if ("XScaleToSqrt" %in% options) {
    g <- g + scale_x_sqrt()
  }
  if ("XScaleReverse" %in% options) {
    g <- g + scale_x_reverse()
  }
  if ("YScaleToLog10" %in% options) {
    g <- g + scale_y_log10()
  }
  if ("YScaleToSqrt" %in% options) {
    g <- g + scale_y_sqrt()
  }
  if ("YScaleReverse" %in% options) {
    g <- g + scale_y_reverse()
  }

  # Theme options
  if ("ThemeBW" %in% options) {
    g <- g + theme_bw()
  }
  if ("ThemeGray" %in% options) {
    g <- g + theme_gray()
  }
  if ("ThemeDark" %in% options) {
    g <- g + theme_dark()
  }
  if ("ThemeClassic" %in% options) {
    g <- g + theme_classic()
  }
  if ("ThemeLight" %in% options) {
    g <- g + theme_light()
  }

  return(g)
}
