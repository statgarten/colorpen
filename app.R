library(shiny)
library(echarts4r)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinyjs)
library(tibble)

originData <- mtcars

data <- originData %>% as.data.frame()

ui <- fluidPage(
  titlePanel("plotGen - Plot Genesis"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "plottype",
        label = "Select Type of Plot",
        choices = c("Scatter Plot" = "scatter", 
                    "Bar Plot" = "bar", 
                    "Line Plot" = "line"),
        selected = "scatter"
      ),
      conditionalPanel( # Scatter Plot일때
        condition = "input.plottype == 'scatter'",
        radioButtons(
          inputId="regression",
          label="Method for Regression",
          choices = c("None", "lm", "glm"),
          selected = "None",
          inline = TRUE,
        ),
        #selectInput("smoothMethod", "Method",
        #            list("lm", "glm", "gam", "loess", "rlm"))
      ),
      selectizeInput(
        inputId = "criteria",
        label = "Select Variable for Criteria",
        choices = data %>% colnames(),
        selected = NULL
      ),
      checkboxGroupInput(
        inputId = "options",
        label = "Option for Plot",
        choices = c("Show X-Axis" = "xaxis", "Show Y-Axis" = "yaxis", "Use Row Name" = "userowname"),
        selected = c("xaxis", "yaxis"),
      ),
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
      echarts4rOutput("plot"),
    )
  )
)

server <- function(input, output) {
  
  updateTrigger <- reactive({ # Plot이 Update되는 조건 지정
    list(input$criteria, input$describe, input$options, input$plottype)
  })

  output$test <- renderText({ # 테스트용입니다.
    paste(input$describe, "~", input$criteria)
  })

  
  observeEvent(
    updateTrigger(),
    {
      output$plot <- renderEcharts4r({
      viewAxis <- function(e, axis){ # X, Y축에 관한 설정을 하는 함수
        func <- e
        
        func <- func %>% 
          e_axis_(label = "asdf", axis = c(substr(axis,1,1)), 
                  show = (FALSE || (axis %in% input$options)))
        
        return(func)
      }
      viewScatterDescribe <- function(e) { # 1개 이상의 Describe Variable을 사용하여 Plot 그리는 함수
        func <- e
        #for(describe in input$describe){
        #  func <- func %>% e_scatter_(describe)
        #}
        
        func <- func %>% e_scatter_(input$describe)

        return(func)
      }
      viewBarDescribe <- function(e) {
        func <- e

        func <- func %>% e_bar_(input$describe)
        
        return(func)
      }
      viewLineDescribe <- function(e) {
        func <- e
        
        func <- func %>% e_line_(input$describe)
        
        return(func)
      }
      viewRegression <- function(e, method){
        func <- e
        
        if(method == "lm"){
          func <- func %>% e_lm(formula = paste(input$describe, "~", input$criteria), 
                                name=paste(input$describe, "~", input$criteria, "w/ lm"))
        }
        
        return(func)
      }
      plot <- function(){ # Plot을 직접 그리는 함수
        switch(
          input$plottype,
          "scatter" = {
            return(
              data %>%
                group_by_(input$factor) %>% 
                e_charts_(input$criteria) %>%
                e_toolbox_feature (
                  feature = c("saveAsImage")
                ) %>% 
                e_axis_labels(x=input$criteria, y=input$describe) %>% 
                viewAxis("xaxis") %>% 
                viewAxis("yaxis") %>% 
                viewScatterDescribe %>% 
                viewRegression(input$regression)
            )
          },
          "bar" = {
            return(
              data %>%
                group_by_(input$factor) %>% 
                e_charts_(input$criteria) %>% 
                e_toolbox_feature (
                  feature = c("saveAsImage")
                ) %>% 
                e_axis_labels(x=input$criteria, y=input$describe) %>% 
                viewAxis("xaxis") %>% 
                viewAxis("yaxis") %>% 
                viewBarDescribe
            )
          },
          "line" = {
            return(
              data %>%
                group_by_(input$factor) %>% 
                e_charts_(input$criteria) %>% 
                e_toolbox_feature (
                  feature = c("saveAsImage")
                ) %>% 
                e_axis_labels(x=input$criteria, y=input$describe) %>% 
                viewAxis("xaxis") %>% 
                viewAxis("yaxis") %>% 
                viewLineDescribe
            )
          }
        )
      }
      plot()
        # e_charts(input$criteria)로는 오류 발생. e_charts_로만 가능한데 이 이유를 찾아보아야 함.
        # e_charts 랑 e_charts_ 는 파라미터를 names로 보느냐 character로 받느냐의 차이.
        # 현재 두개 입력시 그래프가 나타나지 않는데, 두개 입력되었을 경우 %>% e_charts(input$describe[2])를 추가할 수 있는 방법이 있을까요?
    })
    }
  )
}

shinyApp(ui = ui, server = server)
