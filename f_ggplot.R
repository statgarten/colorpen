dummy <- function(){
  return(NULL)
}

viewFactor <- function() {
  if(input$factor == "NA"){
    return(NULL)
  } else {
    return(
      input$factor
    )
  }
}

f_geom <- function(mapping, stat, position){
  if("single_variable" %in% input$dev){
    switch(input$plottype,
           "area_single" = {
             return(
               geom_area(stat = "bin")
             )
           },
           "density_single" = {
             return(
               geom_density(kernel = "gaussian")
             )
           },
           "dot_single" = {
             return(
               geom_dotplot()
             )
           },
           "histogram_single" = {
             return(
               geom_histogram()
             )
           },
           "qq" = {
             return(
               geom_qq(aes_string(sample = input$criteria))
             )
           }
    )
  } else if("describe_is_factor" %in% input$dev){
    if("criteria_is_factor" %in% input$dev){ #Both Discrete
      switch(
        input$plottype,
        "count" = { #Single Discrete
          return(
            geom_count()
          )
        },
        "jitter" = {
          return(
            geom_jitter()
          )
        }
      )
    } else { #Describe Discrete Criteria Continuous
      switch(
        input$plottype,

      )
    }
  } else {
    if("criteria_is_factor" %in% input$dev){ #Describe Continuous Criteria Descrete
      switch(
        input$plottype,

      )
    } else { #Both Continuous
      switch(
        input$plottype,
        "point" = {
          return(
            geom_point()
          )
        },
        "quantile" = {
          return(
            geom_quantile()
          )
        },
        "lm" = {
          return(
            geom_smooth(method = lm)
          )
        }
      )
    }
  }
}

f_coordinate <- function(){
  return(
    dummy()
  )
}

f_facet <- function(){
  return(
    dummy()
  )
}

f_scale <- function(){
  scale <- NULL
  if("xlog10" %in% input$options){
    scale <- scale + scale_x_log10()
  }
  if("xsqrt" %in% input$options){
    scale <- scale + scale_x_sqrt()
  }
  return(
    scale
  )
}

f_theme <- function(){
  return(
    dummy()
  )
}

f_aes <- function(x, y){
  if("single_variable" %in% input$dev){
    return(
      aes_string(x = x)
    )
  } else {
    return(
      aes_string(x = x, y = y)
    )
  }
}

f_position <- function(){

}
