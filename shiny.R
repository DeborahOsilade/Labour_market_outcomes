
library(shiny)

ui <-  fluidPage(
              titlePanel(
                h1("Labour Rates")),
              
sidebarLayout(
              sidebarPanel(
                            textInput("name","Enter Name") , 
                            textOutput("greeting")
   ),
              
              mainPanel(
                        plotOutput('trend')
                            
   )
)
)

server <- function(input,output){
                                 output$greeting <- renderText({paste("Welcome Back",input$name)})
                                 output$trend <- renderPlot({ ggplot()})
                                
}

shinyApp(ui,server)