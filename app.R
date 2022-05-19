#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(echarts4r)
library(dplyr)
library(stringi)

data <- mtcars %>% as.data.frame()

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Echars4r - Scatter Plot w/ mtcars"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId="criteria", label="criteria",
                           choices=mtcars %>% colnames(), selected=NULL),
            selectizeInput(inputId="describe", label="describe",
                           choices=mtcars %>% colnames(), selected=NULL,
                           multiple=TRUE#를 넣으면 여러개가 다 그래프에 나와야 하는데, input$describe로는 오류가 남.
                           #input$describe의 type은 character인데, 이걸 list나 vector로 변환해도 한번에 처리 는 어려운 것 같음.
                           #그런데 mode()로 볼때는 character였는데, input$describe[1] 이런식의 호출도 되는 것으로 봐서 또 다른것인지 의문이 듦.
                           ),
        ),

        mainPanel(
          #처음에 plotOutput으로 충분한지 알았으나, reference 찾고 난 후 eharts4r에 맞는 output이 있다는 것을 알았음.
          #패키지별로 output이 따로 존재한다고 생각해야 할 것 같음.
           echarts4rOutput("scatterPlot"),
           textOutput("test"),
        )
    )
)

server <- function(input, output) {


    output$scatterPlot <- renderEcharts4r({
      #e_charts(input$criteria)로는 오류 발생. e_charts_로만 가능한데 이 이유를 찾아보아야 함.
        data %>% e_charts_(input$criteria) %>% e_scatter_(input$describe)
    })

    output$test <- renderText(stri_split_fixed(input$describe," ", omit_empty = TRUE))

}

shinyApp(ui = ui, server = server)
