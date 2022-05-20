library(shiny)
library(echarts4r)
library(dplyr)
library(stringi)

data <- mtcars %>% as.data.frame()

ui <- fluidPage(
  titlePanel("plotGen - Plot Genesis"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "criteria",
        label = "criteria",
        choices = mtcars %>% colnames(),
        selected = NULL
      ),
      checkboxGroupInput(
        inputId = "options",
        label = "Option for Plot",
        choices = c("Show X-Axis" = "xaxis", "Show Y-Axis" = "yaxis"),
        selected = c("xaxis", "yaxis"),
      ),
      selectizeInput(
        inputId = "describe",
        label = "Select Variable to Describe (Maximum: 2)",
        choices = mtcars %>% colnames(),
        selected = NULL,
        multiple = TRUE,
        options = list(maxItems = 2),
        # 를 넣으면 여러개가 다 그래프에 나와야 하는데, input$describe로는 오류가 남.
        # input$describe의 type은 character인데, 이걸 list나 vector로 변환해도 한번에 처리 는 어려운 것 같음.
        # 그런데 mode()로 볼때는 character였는데, input$describe[1] 이런식의 호출도 되는 것으로 봐서 또 다른것인지 의문이 듦.
      ),
    ),
    mainPanel(
      textOutput("test"),
      # 처음에 plotOutput으로 충분한지 알았으나, reference 찾고 난 후 eharts4r에 맞는 output이 있다는 것을 알았음.
      # 패키지별로 output이 따로 존재한다고 생각해야 할 것 같음.
      echarts4rOutput("scatterPlot"),
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderEcharts4r({
    
    axis <- function(e, axis){
     # if(axis %in% input$options){
     #    return(
     #      e %>% 
     #      e_x_axis(index=0, show=FALSE, axisLabel=axis)
     #    )
     #  } else {
     #    return(e)
     #  }
      return(
        e %>% 
          e_axis_(axis=c(substr(axis,1,1)), show=(FALSE || (axis %in% input$options)))
      )
    }
    
    plot <- function(){
      return(
        data %>%
        e_charts_(input$criteria) %>%
        e_scatter_(input$describe) %>% 
        e_toolbox_feature (
          feature = c("saveAsImage")
        ) %>% 
        axis("xaxis") %>% 
        axis("yaxis")
      )
    }
    
    if(!is.null(input$describe)) { # Describe에 아무것도 입력되지 않은 경우.
      plot()
      # e_charts(input$criteria)로는 오류 발생. e_charts_로만 가능한데 이 이유를 찾아보아야 함.
      # e_charts 랑 e_charts_ 는 파라미터를 names로 보느냐 character로 받느냐의 차이.
      
      # 현재 두개 입력시 그래프가 나타나지 않는데, 두개 입력되었을 경우 %>% e_charts(input$describe[2])를 추가할 수 있는 방법이 있을까요?
    } 
      
  })
  
  output$test <- renderText({ # 테스트용입니다.
    (function (x) substr(x,1,1)) (as.list(input$describe))
  })
}

shinyApp(ui = ui, server = server)
