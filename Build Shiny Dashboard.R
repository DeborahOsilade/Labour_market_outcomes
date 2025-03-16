library(shiny)
library(shinydashboard)

# Define UI for application
ui <- fluidPage(
  # Dashboard title
  titlePanel(h1("Comparing Labour Rates of Countries for Foreign Born and Native Born People")),
  
  # Add custom CSS to style valueBox colors
  tags$head(
    tags$style(HTML("
      .small-box.bg-blue { background-color: #b699b6 !important; }
      .small-box.bg-green { background-color: #2d4739 !important; }
      .small-box { color: white; }  /* Ensures text is white */
                    
        * Adjusts the width of the menu items */
      .nav-pills > li > a {
        white-space: nowrap;  /* Prevents text wrapping */
      }
      
      /* Reduces the width of the entire navlistPanel */
      .col-sm-3 {
        width: 250px !important;  /* Adjust this value to make it narrower */
        max-width: 250px !important;  /* Ensures it doesn't expand */
      }
      /* Added a clean background for the entire page */
      body {
        background-color: #F4F6F9;
      }
      
      /* Stylish borders for boxes and text */
      .box-header {
        background-color: #5D8C94;
        color: white;
      }" 
       ))),
    navbarPage(
    tabsetPanel( #2tabs here
      tabPanel("Introduction",textInput("name","Enter Name")),
      tabPanel("See Story","Analyse with filters", "Controls",
  column(12,
      sidebarLayout(
    sidebarPanel(
  # Adding Sidebar with controls
      #choose a gender button
     selectInput("Gender", "Select Gender:", choices = sex_of_people), 
      sliderInput("Year", "Slide a Year:",  min = min(time_of_year),  max = max(time_of_year), 
                  value = max(time_of_year),  step = 1,sep = ""),
      selectInput("TopN", "Select Number of Top Countries:", choices = num_choices, selected = 20)
    )
      ,
    #added main panel
    mainPanel(
       h1(textOutput("greeting")),
        textOutput("selected_text"),
        #adding a fluid row in the main panel
        fluidRow(
          # Summary Cards
          valueBoxOutput("avg_rate",width = 6),
          valueBoxOutput("median_rate",width = 6)
        ),
        # Adding  a title after the summary cards
        tags$h3("Labour Rate Analysis by Country"),
        
        tabsetPanel(id = "StatusTab",
          tabPanel("Foreign-born",
                   # Output plot and details for Foreign-born
                   fluidRow(
                     column(6, plotOutput("labour_rate_plot1")),
                     column(6, plotOutput("labour_rate_plot2"))
                   )
          ),
          tabPanel("Native-born",
                   # Output plot and details for Native-born
                   fluidRow(
                     column(6, plotOutput("labour_rate_plot3")),
                     column(6, plotOutput("labour_rate_plot4"))
                   )
          ))
          
  )))))))

# Define server logic_________________________________________________________________________________________

server <- function(input, output) {
 #output for greeting
  output$greeting <- renderText({paste("Welcome to the Story",input$name)})
   #output for select button
  output$selected_text <- renderText({paste("Viewing Labour Rates for:", input$Gender, "in",input$Year )})
  
# Render Summary Cards_____________________________________________________________________________________
#Extra data
  filtered_data <- reactive({ 
    status_filter <- input$StatusTab 
    
    labour_rates%>%
      filter(labour_rates$sex == input$Gender, labour_rates$time_period == input$Year, labour_rates$place_of_birth == status_filter)})
  
  #first card
  output$avg_rate <- renderValueBox({
    avg_rate <- mean(filtered_data()$rates, na.rm = TRUE)
    valueBox(
      round(avg_rate, 2),
      "Average Employment Rate",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  #second card
  output$median_rate <- renderValueBox({
    median_rate <- median(filtered_data()$rates, na.rm = TRUE)
    valueBox(
      round(median_rate, 2),
      "Median Employment Rate",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
#output for first plot___________________________________________________________________________________________
  # Output for Foreign-born plot (labour_rate_plot1)
  output$labour_rate_plot1 <- renderPlot({Firstplot(input$Gender,input$Year,"Foreign-born",input$TopN)})
  
  # Output for Native-born plot (labour_rate_plot3) 
  output$labour_rate_plot3 <- renderPlot({Firstplot(input$Gender,input$Year,"Native-born",input$TopN)})
  
#output for second plot________________________________________________________________________________________
  # Output for Foreign-born plot (labour_rate_plot2)
  output$labour_rate_plot2 <- renderPlot({secondplot(input$Gender,input$Year,input$TopN)})
  # Output for Native-born plot (labour_rate_plot4)
  output$labour_rate_plot4 <- renderPlot({secondplot(input$Gender,input$Year,input$TopN)})
  }

# Run the application 
shinyApp(ui = ui, server = server)
