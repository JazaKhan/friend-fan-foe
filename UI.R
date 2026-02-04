library(shiny)
library(tidyverse)


source("ANALYSIS.R")

# UI
ui <- fluidPage(
  titlePanel("Friend, Fan, or Foe --- Instagram Edition"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "followers_file",
        "Upload Followers JSON:",
        accept = c(".json")
      ),
      
      fileInput(
        "following_file",
        "Upload Following JSON:",
        accept = c(".json")
      ),
      
      actionButton(
        "save_files",
        "Save Files to Data Folder",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      actionButton(
        "load_files",
        "Load Saved Files",
        class = "btn-success"
      ),
      
      hr(),
      
      checkboxGroupInput(
        "show",
        "Select category to display:",
        choices = c("Friends", "Foes", "Fans"),
        selected = c("Friends")
      )
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Read 
  data_store <- reactiveVal(NULL)
  
  observeEvent(c(input$followers_file, input$following_file), {
    req(input$followers_file, input$following_file)
    
    processed <- process_instagram_data(
      input$followers_file$datapath,
      input$following_file$datapath
    )
    
    data_store(processed)
  })
  
  # Save
  observeEvent(input$save_files, {
    req(input$followers_file, input$following_file)
    
    save_uploaded_files(
      input$followers_file,
      input$following_file
    )
    
    showNotification(
      "Data saved!",
      type = "message"
    )
  })
  
  # Load
  observeEvent(input$load_files, {
    
    loaded_data <- load_saved_files()
    
    if (!is.null(loaded_data)) {
      data_store(loaded_data)
      showNotification(
        "Loaded data successfully!",
        type = "message"
      )
    } else {
      showNotification(
        "No data saved yet!",
        type = "warning"
      )
    }
  })
  
  output$results <- renderTable({
    data <- data_store()
    req(data)
    
    # Get Analysed Data
    res <- list(
      "Friends" = data$friends,
      "Foes" = data$foes,
      "Fans" = data$fans
    )
    
    # Divide
    selected <- res[input$show]
    
    tibble(
      Category = rep(names(selected), sapply(selected, length)),
      Username = unlist(selected)
    )
  })
}

# Run
shinyApp(ui = ui, server = server)
