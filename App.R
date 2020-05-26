library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
# install.packages("DT")
library(DT)


ui <- shinyUI(fluidPage(
  headerPanel(title = "Bank Loan Prediction"),
  sidebarLayout(
    sidebarPanel( 
      selectInput("Prediction","Select a feild to histogram", choice = names(test)) 
    ),
    
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Data",DT::dataTableOutput("test"), downloadButton("downloadData","Download Data")),
                  tabPanel("Summary",verbatimTextOutput("summ")),
                  tabPanel("Plot",plotOutput("plot"))
      )
    )
  )
)
)



server <- shinyServer(function(input,output,session){


    output$test <- DT::renderDataTable({
      test
})
    output$summ <- renderPrint({
      summary(test)
})
    output$plot <- renderPlot({
      hist(test[,input$Prediction],col="red")
      
      predictionFilter <- subset(test, test$pred_MIS_Status == input$instate)
      
    })

    output$downloadData <- downloadHandler(
      
      
      filename = function(){
      paste("test","csv",sep = ".")
        },
      
      content = function(file){
       write.csv(test[,16],file)
        }
      
    )        
    
    
})    

      


shinyApp(ui=ui, server=server)

