#' @title SHiny Module for map visualization.
#'
#' @description mapVisModule UI Function
#'
#' @param id id of module
#' @param i18n shiny.i18n object, default value is NULL
#'
#' @import shiny
#' @import leaflet
#' @import shiny.i18n
#' @importFrom phosphoricons ph
#'
#' @export
mod_mapVisModule_ui <- function(id, i18n = NULL) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column( # Result Area
        width = 8,
        style = "border-right: dotted 1px black",
        leafletOutput(outputId = ns("mymap"))
      ),
      column( # Options
        width = 4,
        style = "border-left: dotted 1px black",
        fluidRow(
          style = "margin:auto",
          selectInput(ns("x"), label = "", choices = NULL, width = "100%"),
          selectInput(ns("y"), label = "", choices = NULL, width = "100%"),
          sliderInput(
            ns("radius"),
            label = ifelse(is.null(i18n), "marker size", i18n$t("marker size")),
            min = 1, max = 10, value = 5, step = 1, ticks = FALSE,
            width = "100%"
          ),
          checkboxInput(
            ns("cluster"),
            label = ifelse(is.null(i18n), "Group marker", i18n$t("Group marker")),
            width = "100%"
          ),
          div(
            id = ns("div"),
            selectInput(ns("color"), "", choices = NULL, width = "100%"),
            sliderInput(
              ns("opacity"),
              label = ifelse(is.null(i18n), "alpha", i18n$t("alpha")),
              min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE,
              width = "100%"
            )
          ),
          actionButton( # Main action
            ns("draw"),
            label = tagList(
              phosphoricons::ph("arrow-circle-right"),
              label = ifelse(is.null(i18n), "Draw", i18n$t("Draw")),
            ),
            class = "myButton",
            width = "100%"
          )
        )
      )
    )
  )
}

#' @title SHiny Module for map visualization.
#' @description mapVisModule Server Functions
#'
#' @param id if of module
#' @param inputData "reactive" data
#' @param i18n shiny.i18n object, default value is NULL
#' @param lang "reactive" shiny.i18n option like 'en', default value is NULL
#' @import shiny.i18n
#' @importFrom shinyjs hide show
#' @import leaflet
#'
#' @export
mod_mapVisModule_server <- function(id, inputData, i18n = NULL, lang = reactive({
                                      NULL
                                    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(input$cluster, {
      if (input$cluster) {
        shinyjs::hide(id = "div")
      } else {
        shinyjs::show(id = "div")
      }
    })



    observeEvent(list(inputData(), lang()), {
      data <- inputData()

      updateSelectizeInput(
        session,
        inputId = "x",
        label = ifelse(is.null(i18n), "longitude column (X)", i18n()$t("longitude column (X)")),
        choices = names(Filter(is.numeric, data)), # Numeric only
        server = TRUE,
        selected = NULL
      )

      updateSelectizeInput(
        session,
        inputId = "y",
        label = ifelse(is.null(i18n), "latitude column (Y)", i18n()$t("latitude column (Y)")),
        choices = names(Filter(is.numeric, data)), # Numeric only
        server = TRUE,
        selected = NULL
      )

      updateSelectizeInput(
        session,
        inputId = "color",
        label = ifelse(is.null(i18n), "color column", i18n()$t("color column")),
        choices = union(names(Filter(is.numeric, data)), names(Filter(is.factor, data))),
        server = TRUE,
        selected = NULL
      )
    })

    observeEvent(input$draw, {
      data <- inputData()
      m <- generateMap(
        x <- data[[input$x]],
        y <- data[[input$y]],
        colorVariable = data[[input$color]],
        fillOpacity = input$opacity,
        radius = input$radius,
        cluster = input$cluster
      )
      output$mymap <- renderLeaflet(m)
    })
  })
}

generateMap <- function(x, y, colorVariable = NULL, radius = 10, fillOpacity = 0.5, cluster = FALSE) {
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  if (is.factor(colorVariable)) { # factor
    palettes <- gg_color_hue(length(unique(colorVariable)))
    pal <- colorFactor(palettes, domain = unique(colorVariable))
  } else {
    if (is.numeric(colorVariable)) { # numeric
      pal <- colorNumeric("RdYlBu", domain = NULL)
    }
  }

  if (cluster) {
    return(
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          lng = x,
          lat = y,
          color = pal(colorVariable),
          stroke = FALSE,
          radius = radius,
          fillOpacity = fillOpacity,
          clusterOptions = markerClusterOptions()
        )
    )
  }

  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lng = x,
      lat = y,
      color = pal(colorVariable),
      stroke = FALSE,
      radius = radius,
      fillOpacity = fillOpacity
    ) %>%
    addLegend(
      pal = pal,
      values = colorVariable
    )
}
