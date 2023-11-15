#' @title Shiny Module for pairplot
#'
#' @description PairModule UI Function
#'
#' @param id id of module
#'
#' @import shiny
#' @importFrom phosphoricons ph
#'
#' @export
mod_pairModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column( # Result Area
        width = 8,
        style = "border-right: dotted 1px black",
        plotOutput(outputId = ns("plot"))
      ),
      column( # Options
        width = 4,
        style = "border-left: dotted 1px black",
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

#' @title Shiny Module for pair plot
#' @description PairModule Server Functions
#'
#' @param id if of module
#' @param inputData "reactive" data
#'
#' @import magrittr
#' @importFrom dplyr select
#' @importFrom GGally ggpairs
#'
#' @export
mod_pairModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    # update UI
    observeEvent(inputData(), {
      data <- inputData()
      output$colsUI <- renderUI({
        selectInput(
          inputId = ns("columns"),
          label = "Select Columns",
          choices = names(Filter(is.numeric, data)),
          multiple = TRUE,
        )
      })
    })

    observeEvent(input$draw, {
      data <- inputData()
      output$plot <- renderPlot({
        GGally::ggpairs(data %>% dplyr::select(input$columns))
      })
    })
  })
}
