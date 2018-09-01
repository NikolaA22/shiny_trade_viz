#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(shinyTime)
library(ggthemes)
library(gridExtra)

ui <- fluidPage(
   navbarPage("Navbar",
   tabPanel("Load and Explore",
   # Application title
   sidebarPanel(
     fileInput("file", "Upload the file"),
     helpText("Upload OHLC data"),
     br(),
     tags$hgroup(),
     h5(helpText("Select the parameters below")),
     checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
     br(),
     selectInput(inputId = "selectVar", label = "Choose a variable", choices = c(""))
   ),
   mainPanel(
     uiOutput("tb")
   )
  ),
  tabPanel("Plot",
           sidebarPanel(
             h5(helpText("Grayed dates are not present in the data")),
             uiOutput("daterange"),
             br(),
             h5(helpText("You can choose from the following times present in this day: ")),
             uiOutput("chosen_text"),
             br(),
             h5(helpText("This is min hour: ")),
             uiOutput("min_hour"),
             uiOutput("timerange"),
             br(),
             radioButtons("mode", "Buy or sell:",
                          c("Buy" = "buy",
                            "Sell" = "sell"
                           )),
             br(),
             actionButton(inputId = "go", label = "Plot")
            
           ),
             mainPanel(
             plotOutput("filtered_plot", click = "plot_click"),
             verbatimTextOutput("brush_info")
             
           )
           )
  )
 )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  theme_set(theme_gdocs())
  
  vals <- reactiveValues()
  
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read_csv(file = file1$datapath, col_names = input$header)
    #updateSelectInput(session, "variable", choices = colnames(data()), selected = "")
    
  })
  
  output$brush_info <- renderPrint({
    t(nearPoints(filt_data(), input$plot_click, allRows = FALSE, xvar = "time_period_start", yvar = "price_open"))
  })
  
  filt_data <- eventReactive(input$go, {
    df <- data()
    
    startTime <- strftime(as.character(input$startTime), format="%H:%M:%S")
    
    datetime <- paste(vals$chosen_date, startTime, sep = " ")
    
    datetime <- as.POSIXct(datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    
    enddatetime <- datetime + hours(1)
    
    cat(as.character(enddatetime))
    cat("\n")
   
    df <- df %>%
      filter(time_period_start >= datetime) %>%
      filter(time_period_start <= enddatetime)

    # TODO Add an input in the UI and an if-else clause here to choose between two types of plots or add 2 plots for both buy and sell
    
    df
  })
  
  output$filtered_plot <- renderPlot({
    
    p_buy <- ggplot(data = filt_data(), mapping = aes(x = time_period_start, y = price_open)) +
      geom_point(aes(color = buy_decision), size = 4) + geom_path(linetype = 2) +
      labs(color = "Buy", x="Time", y = "Price Open", title = "Buy mode")
    
    p_sell <- ggplot(data = filt_data(), mapping = aes(x = time_period_start, y = price_open)) +
      geom_point(aes(color = sell_decision), size = 4) + geom_path(linetype = 2) +
      labs(color = "Sell", x="Time", y = "Price Open", title = "Sell mode")
    
    # grid.arrange(p_buy, p_sell, ncol=1)
    if(input$mode == 'buy'){
      p_buy
    }else{
      p_sell
    }
    
    
  })
  
  observe({
    if(is.null(data())){return()}
    updateSelectInput(session, "selectVar", choices = names(data()))
  })
  
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  output$daterange <- renderUI({
    
    dateInput(
      inputId = "daterange",
      label = "Select the date for plotting",
      value = "2018-01-01",
      max = max(data()$time_period_start),
      min = min(data()$time_period_start)
    )
    
  })
  
  output$timerange <- renderUI({
    timeInput(inputId = "startTime", label = "Select time", value = strptime("00:00:00", "%T"))
  })
  
  filt_data_tmp <- reactive({
    if(is.null(data())){return()}
    df <- data()
    df$time_period_start_date <- as.Date(df$time_period_start, tz = "UTC")
    df <- subset(df, time_period_start_date == vals$chosen_date)
    df
  })
  
  observe({
    chosen_date <- strftime(as.character(input$daterange), format = "%Y-%m-%d", tz = "UTC")
    vals$chosen_date <- chosen_date
  })
  

  observeEvent(input$daterange, {
    if(is.null(filt_data_tmp())){return()}
    min_hour <- min(filt_data_tmp()$time_period_start, na.rm = TRUE)
    vals$min_hour <- paste(hour(min_hour),minute(min_hour),second(min_hour), sep=":")
  })
  
  
  output$min_hour <- renderText({
    if(is.null(filt_data_tmp())){return()}
    value <- as.character(vals$min_hour)
    value
  })
  
  output$chosen_text <- renderText({
    if(is.null(data())){return()}
    value <- as.character(vals$chosen_date)
    value
  })
  
  output$sum <- renderPrint({
    if(is.null(data())){return()}
    my_df <- data()[, input$selectVar]
    summary(my_df)
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(data())){return()}
    DT::datatable(data())
  })
  
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by Rstudio")
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")), tabPanel("Data", DT::dataTableOutput("table")), tabPanel("Summary",  verbatimTextOutput("sum")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

