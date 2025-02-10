library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Data Upload and Selection"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      
      checkboxInput("use_builtin", "Use Built-in Dataset Instead", FALSE),
      
      conditionalPanel(
        condition = "input.use_builtin == true",
        selectInput("builtin_dataset", "Choose a dataset",
                    choices = c("mtcars", "airquality"))
      ),
      
      actionButton("load_data", "Load Data")
    ),
    
    mainPanel(
      DTOutput("data_table")  # Using DT for enhanced table display
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    if (input$use_builtin) {
      # Load pre-saved CSV file from the app's subfolder
      dataset_path <- file.path("datasets", paste0(input$builtin_dataset, ".csv"))
      if (file.exists(dataset_path)) {
        dataset(read.csv(dataset_path, header = TRUE))
      } else {
        showNotification("Dataset file not found!", type = "error")
      }
    } else if (!is.null(input$file)) {
      # Load user-uploaded file
      req(input$file)
      dataset(read.csv(input$file$datapath, header = input$header, sep = input$sep))
    }
  })
  
  output$data_table <- renderDT({
    req(dataset())
    datatable(dataset(), 
              options = list(pageLength = 10,  # Show 10 rows per page
                             autoWidth = TRUE, 
                             searchHighlight = TRUE,
                             dom = 'Bfrtip'))  # Adds buttons for copy/export (optional)
  })
}

shinyApp(ui, server)
