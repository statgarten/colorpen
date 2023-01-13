#' @title Shiny Module for mosaicplot
#'
#' @description mosaicModule UI Function
#'
#' @param id id of module
#'
#' @import shiny
#' @import ggmosaic
#' @import ggplot2
#' @importFrom phosphoricons ph
#'
#' @export
mod_mosaicModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column( # Result Area
        h4("Mosaic Plot"),
        width = 8,
        style = "border-right: dotted 1px black",
        plotOutput(outputId = ns("plot"))
      ),
      column( # Options
        width = 4,
        style = "border-left: dotted 1px black",
        h4("Options"),
        fluidRow(
          style = "margin:auto",
          uiOutput(outputId = ns("colsUI")),
          actionButton( # Main action
            ns("draw"),
            label = tagList(
              phosphoricons::ph("arrow-circle-right"),
              label = "Draw",
            ),
            class = "myButton",
            width = "100%"
          )
        )
      )
    )
  )
}

#' @title Shiny Module for mosaicplot
#' @description mosaicModule Server Functions
#'
#' @param id if of module
#' @param inputData "reactive" data
#'
#'
#' @export
mod_mosaicModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    # update UI
    observeEvent(inputData(), {
      data <- inputData()
      output$colsUI <- renderUI({
        tagList(
          column(
            width = 6,
            selectInput(
              inputId = ns("x"),
              label = "X (Factor)",
              choices = names(Filter(is.factor, data))
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = ns("fill"),
              label = "Color (Factor)",
              choices = names(Filter(is.factor, data))
            )
          )
        )
      })
    })

    observeEvent(input$draw, {
      plot.variables <- c(x = input$x, fill = input$fill)

      data <- inputData()
      output$plot <- renderPlot({
        eval(parse(text = paste0(
          'ggplot(data = data) + ',
            'geom_mosaic(',
            'aes(x = product(', input$x, '), fill = ', input$fill, ')) + ',
          'theme_mosaic() + theme(legend.position = ', "'",'none',"'", ')')
        ))
      })
    })
  })
}
