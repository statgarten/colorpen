library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(echarts4r)
library(tidyverse) # include ggplot2
library(plotly)
library(dplyr)
library(DT)
library(tibble)

source(c("f_ggplot.R"))

originData <- mtcars

data <- originData %>% as.data.frame()

ui <- fluidPage(
  titlePanel("plotGen - Plot Genesis"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "dev",
        label = "Option for Developing",
        choices = c(
          "Criteria is Factor" = "criteria_is_factor",
          "Describe is Factor" = "describe_is_factor",
          "Single Variable" = "single_variable"
        ),
        selected = c("criteria_is_factor", "describe_is_factor"),
      ),
      selectizeInput(
        inputId = "plottype",
        label = "Select Type of Plot",
        choices = c("Area Plot(Single)" = "area_single",
                    "Density Plot(Single)" = "density_single",
                    "Dot Plot(Single)" = "dot_single",
                    "Histogram(Single)" = "histogram_single",
                    "QQ Plot(Single)" = "qq", #이상 Single Variable
                    "Scatter Plot" = "scatter",
                    "Point Plot" = "point",
                    "Quantile Plot" = "quantile",
                    "Logistic Regression" = "lm", #이상 Both Continuous
                    "Box Plot" = "box",
                    "Dot Plot" = "dot",
                    "Violin Plot" = "violin", #이상 One Continuous One Discrete
                    "Area Plot" = "area",
                    "Line Plot" = "line", #이상 Both Continuous
                    "Count Plot" = "count",
                    "Jitter Plot" = "jitter" #이상 Both Discrete
        ),
        selected = "scatter"
      ),
      conditionalPanel( # Scatter Plot일때
        condition = "input.plottype == 'scatter'",
        radioButtons(
          inputId="regression",
          label="Method for Regression",
          choices = c("None", "lm", "loess"),
          selected = "None",
          inline = TRUE,
        ),
        conditionalPanel(
          condition = "input.factor != 'NA'",
          checkboxInput(
            inputId = "overall_regression",
            label = "Show Overall Line Only",
            value = FALSE,
          )
        ),
      ),
      conditionalPanel(
        condition = "input.criteria == input.describe && input.regression != 'None'",
        tags$p(
          "Criteria and Describe cannot be same.",
          style = "color: red;"
        )
      ),
      #selectInput("smoothMethod", "Method",
      #            list("lm", "glm", "gam", "loess", "rlm"))
      selectizeInput(
        inputId = "criteria",
        label = "Select Variable for Criteria",
        choices = data %>% colnames(),
        selected = NULL
      ),
      selectizeInput(
        inputId = "position",
        label = "Select Position for Plot",
        choices = c("dodge", "identity", "jitterdodge", "jitter", "nudge", "stack"),
        selected = NULL
      ),
      checkboxGroupInput(
        inputId = "options",
        label = "Option for Plot",
        choices = c(
          "Use Row Name" = "userowname",
          "Rotate X-Axis Label" = "xlabel",
          "Show Legends" = "legend",
          "X scale to log10" = "xlog10",
          "X scale to SQRT" = "xsqrt"
        ),
        selected = NULL,
      ),
      conditionalPanel(
        # condition = "input.plottype == ('scatter' || 'line')",
        condition = TRUE,
        selectizeInput(
          inputId = "describe",
          label = "Select Variable to Describe",
          choices = data %>% colnames(),
          selected = NULL,
          multiple = FALSE,
          # 를 넣으면 여러개가 다 그래프에 나와야 하는데, input$describe로는 오류가 남.
          # input$describe의 type은 character인데, 이걸 list나 vector로 변환해도 한번에 처리 는 어려운 것 같음.
          # 그런데 mode()로 볼때는 character였는데, input$describe[1] 이런식의 호출도 되는 것으로 봐서 또 다른것인지 의문이 듦.
        ),
      ),
      selectizeInput(
        inputId = "factor",
        label = "Select Variable for Factor",
        choices = c("NA", data %>% colnames()),
        selected = NULL,
        multiple = FALSE,
        # 를 넣으면 여러개가 다 그래프에 나와야 하는데, input$describe로는 오류가 남.
        # input$describe의 type은 character인데, 이걸 list나 vector로 변환해도 한번에 처리 는 어려운 것 같음.
        # 그런데 mode()로 볼때는 character였는데, input$describe[1] 이런식의 호출도 되는 것으로 봐서 또 다른것인지 의문이 듦.
      ),
    ),
    mainPanel(
      textOutput("test"),
      # 처음에 plotOutput으로 충분한지 알았으나, reference 찾고 난 후 eharts4r에 맞는 output이 있다는 것을 알았음.
      # 패키지별로 output이 따로 존재한다고 생각해야 할 것 같음.
      plotlyOutput("plot"),
    )

  )
)

server <- function(input, output) {

  updateTrigger <- reactive({ # Plot이 Update되는 조건 지정
    list(input$criteria, input$describe, input$options, input$plottype)
  })

  output$test <- renderText({
    "xsqrt" %in% input$options
  })

  observeEvent(
    updateTrigger(),
    {
      output$plot <- renderPlotly({

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

        plot <- function(){
          ggplotly(
            data %>%
              ggplot(f_aes(x = input$criteria, y = input$describe)) +
              f_geom(mapping = f_aes(), stat = f_stat(), position = f_position()) +
              f_coordinate() +
              f_facet() +
              f_scale() +
              f_theme()
          )
        }

        plot()
      })
    }
  )
}

shinyApp(ui = ui, server = server)
