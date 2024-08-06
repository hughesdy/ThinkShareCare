#install.packages(c("shiny", "shinyjs", "shinyWidgets"))

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)

setwd("~/Desktop/Neurohack/Project/App")

# Define the list of possible variables
possible_vars <- c("Age", "Sex", "Gender", "SES", "Education", "Income", "GAD-7", "PHQ-9")

# Define the UI for the app
ui <- fluidPage(
  
  # Add the header image at the top
  tags$div(
    tags$img(src = "ThinkShareCare_Logo.png", style = "width: 100%; height: auto; max-width: 800px;"),
    style = "text-align: center;"
  ),
  
  # App title and instructions
  titlePanel("Contributor Data Upload and Variable Selection Tool"),
  
  sidebarLayout(
    sidebarPanel(
      # Instructions for file upload
      h4("Upload Your Data"),
      p("Data files must be in .csv format. The first row of the file can contain variable names, while the rest of the data should be numeric."),
      fileInput("file", "Choose CSV File", accept = ".csv"),
      
      # New heading and text for variable selection
      h4("Select Your Variables"),
      p("Please search and select the names of your variables in the order they are listed in your dataset. Variable 1 will be ID, and Variable 2 will be the variable listed in the second column of your dataset, and so on."),
      
      uiOutput("dynamic_inputs"),
      uiOutput("dynamic_inputs_details"),
      actionButton("addField", "Add New Field"),
      actionButton("submit", "Submit"),
      textOutput("error_msg")  # Add a text output to display error messages
    ),
    
    mainPanel(
      tableOutput("uploadedData")
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  # Reactive value to store the number of input fields
  field_count <- reactiveVal(1)
  
  # Reactive value to store the data frame
  df <- reactiveVal(NULL)
  
  # Render the dynamic inputs
  output$dynamic_inputs <- renderUI({
    fields <- lapply(2:(field_count() + 1), function(i) {
      selectizeInput(paste0("var", i), label = paste("Variable", i),
                     choices = c("Select Variable" = "", possible_vars, "Add New Variable" = "new"),
                     options = list(create = FALSE),
                     selected = input[[paste0("var", i)]])
    })
    # Always include the fixed "ID" field as the first input
    fixed_id_field <- selectizeInput("var1", label = "Variable 1",
                                     choices = c("ID"),
                                     options = list(create = FALSE),
                                     selected = "ID")
    # Combine fixed and dynamic fields
    tagList(fixed_id_field, fields)
  })
  
  # Render additional fields for new variables
  output$dynamic_inputs_details <- renderUI({
    details <- lapply(2:(field_count() + 1), function(i) {
      if (input[[paste0("var", i)]] == "new") {
        tagList(
          tags$div(style = "margin-left: 20px; margin-top: 10px;",
                   textInput(paste0("var_name_", i), label = paste("Variable", i, "Name"), placeholder = "Enter variable name", value = input[[paste0("var_name_", i)]]),
                   radioButtons(paste0("var_type_", i), label = paste("Variable", i, "Type"), choices = c("Numerical" = "numerical", "Categorical" = "categorical"), selected = input[[paste0("var_type_", i)]]),
                   textInput(paste0("var_desc_", i), label = paste("Variable", i, "Description (up to 100 characters)"), placeholder = "Enter description", width = "100%", value = input[[paste0("var_desc_", i)]])
          )
        )
      }
    })
    tagList(details)
  })
  
  # Add new field
  observeEvent(input$addField, {
    current_count <- field_count()
    if (current_count < 100) {
      field_count(current_count + 1)
    }
  })
  
  # Load and validate data when a file is uploaded
  observe({
    req(input$file)
    
    tryCatch({
      uploaded_df <- read.csv(input$file$datapath)
      
      # Check that the first row contains characters
      if (!all(sapply(uploaded_df[1,], is.character))) {
        output$error_msg <- renderText("Error: The first row must contain character fields.")
        df(NULL)  # Reset the data frame
        return()
      }
      
      # Check that all other rows contain numerical values
      if (!all(sapply(uploaded_df[-1,], is.numeric))) {
        output$error_msg <- renderText("Error: All other rows must contain numerical values.")
        df(NULL)  # Reset the data frame
        return()
      }
      
      output$error_msg <- renderText("")  # Clear any previous error messages
      df(uploaded_df)
      output$uploadedData <- renderTable({
        head(uploaded_df)
      })
    }, error = function(e) {
      output$error_msg <- renderText(paste("Error reading the file:", e$message))
      df(NULL)  # Reset the data frame
    })
  })
  
  # Handle submit action
  observeEvent(input$submit, {
    req(df())  # Ensure df is not NULL
    
    # Collect the selected variables, starting from the second column as "ID" is fixed
    selected_vars <- sapply(2:(field_count() + 1), function(i) {
      var <- input[[paste0("var", i)]]
      if (var == "new") {
        list(
          name = input[[paste0("var_name_", i)]],
          type = input[[paste0("var_type_", i)]],
          description = input[[paste0("var_desc_", i)]]
        )
      } else {
        var
      }
    })
    
    # Print the selected variables for demonstration
    print(selected_vars)
    
    # Here you can add code to process the selected variables
  })
}

# Run the application
shinyApp(ui = ui, server = server)