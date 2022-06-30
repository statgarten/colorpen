plotGenUI <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(
      inputId = ns("loadSubsetColumn"),
      label = "Load Variables",
      icon = icon("check")
    ),
    selectInput(
      inputId = ns("subsetColumn"),
      label = "subsetSelectLabel",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    ),
    actionButton(
      inputId = ns("subsetButton"),
      label = "subset",
      icon = icon("angle-down")
    )
  )
}

plotGenServer <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadSubsetColumn, {
      updateSelectizeInput(
        session,
        inputId = "subsetColumn",
        label = "subsetSelectLabel",
        choices = colnames(inputData()),
        server = TRUE
      )
    })

    observeEvent(input$subsetButton, {
      eval(parse(
        text =
          paste0(
            "inputData( inputData() %>% ",
            "select(-", input$subsetColumn, "))"
          )
      ))

      output$DT <- renderDT(
        getDT(inputData())
      )
    })
  })
}
