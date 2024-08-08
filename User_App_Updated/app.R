library(shiny)
library(DBI)
library(RSQLite)
library(aws.s3)

#set up server connection
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQMEY53X73DF2AD6C",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-west-2")


ui <- fluidPage(
  # Top panel with logo and download button
  fluidRow(
    column(12,
           tags$div(style = "display: flex; align-items: center; justify-content: space-between; padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd;",
                    tags$img(src = "ThinkShareCare_Logo.png", height = "100px"),
                    downloadButton("downloadDictionary", "Download Variable Dictionary")
           )
    )
  ),
  
  # Main content
  titlePanel("Model Selector and Risk Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h3("Search and Select a Model", style = "font-size: 18px; margin-bottom: 20px;"),  # New header with margin
      
      tags$div(style = "margin-top: 20px;",  # Add margin above "Select Predictors:"
               selectInput("predictors", "Select Predictors:", 
                           choices = c("Select Predictors" = "", "Age", "Sex Assigned at Birth", "Gender Identity", "Income", "Education", "GAD7", "PHQ9", "BDI", "PSS", "LEC-5", "SIPS", "PQB", "CBCL", "FSIQ", "MDD_dx_lifetime", "GAD_dx_lifetime", "SZ_dx_lifetime", "SAD_dx_lifetime", "OCD_dx_lifetime", "BPI_dx_lifetime", "BPII_dx_lifetime", "PTSD_dx_lifetime", "SUD_dx_lifetime", "AUD_dx_lifetime", "ADHD_dx_lifetime"),
                           selected = NULL, 
                           multiple = TRUE)
      ),
      
      selectInput("outcome", "Select Outcome:",
                  choices = c("Select Outcome" = "", "MDD_dx_current", "GAD_dx_current", "SZ_dx_current", "SAD_dx_current", "OCD_dx_current", "BPI_dx_current", "BPII_dx_current", "PTSD_dx_current", "SUD_dx_current", "AUD_dx_current", "ADHD_dx_current"),
                  selected = NULL),
      
      actionButton("searchModels", "Search Models"),
      
      uiOutput("modelList")  # Output for displaying model names
    ),
    
    mainPanel(
      textOutput("selectedModel"),
      uiOutput("modelWeights"),  # Output for displaying model weights
      uiOutput("result")  # Output for displaying predicted values
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$searchModels, {
    # Fetch the list of model files from S3
    model_files <- get_bucket("thinksharecare-beta1")
    print(model_files)  # Debugging: print model_files
    
    if ("Contents" %in% names(model_files)) {
      model_files <- sapply(model_files, function(x) x$Key)
    } else {
      model_files <- character(0)  # If no files are found, return an empty character vector
    }
    print(model_files)  # Debugging: print model_files after processing
    
    # Filter models based on user input
    filtered_models <- NULL
    for (file in model_files) {
      model_df <- s3read_using(FUN = read.csv, object = as.character(file), bucket = "thinksharecare-beta1")
      variable_names <- model_df[2:nrow(model_df),1]
      if (all(input$predictors %in% variable_names) && input$outcome %in% model_df[1,3]) {
        filtered_models <- c(filtered_models, file)
      }
    }
    
    # Update the UI with the list of filtered models
    output$modelList <- renderUI({
      if (length(filtered_models) > 0) {
        selectInput("selectedModel", "Select a Model:", choices = filtered_models)
      } else {
        h5("No models match the selected criteria.")
      }
    })
  })
  
  observeEvent(input$selectedModel, {
    if (!is.null(input$selectedModel)) {
      # Fetch and read the selected model file from S3
      model_file <- input$selectedModel
      model_df <- s3read_using(FUN = read.csv, object = model_file, bucket = "thinksharecare-beta1")
      
      # Display the model weights
      output$modelWeights <- renderUI({
        renderTable(model_df)
      })
    }
  })
  
  output$selectedModel <- renderText({
    paste("Selected Model:", input$selectedModel)
  })
  
  output$result <- renderUI({
    req(input$selectedModel)
    
    model_file <- input$selectedModel
    model_df <- s3read_using(FUN = read.csv, object = model_file, bucket = "thinksharecare-beta1")
    
    intercept <- as.numeric(model_df[1, 2])
    weights <- as.numeric(model_df[2:nrow(model_df), 2])
    variables <- model_df[2:nrow(model_df), 1]
    
    pt_values <- sapply(variables, function(var) input[[var]])
    
    if (all(!is.na(pt_values))) {
      individual_terms <- sum(weights * pt_values)
      predicted <- intercept + individual_terms
      
      HTML(paste0("<b>Predicted value: ", round(predicted, 3), "</b>"))
    } else {
      HTML("")
    }
  })
  
  # Download handler for variable dictionary
  output$downloadDictionary <- downloadHandler(
    filename = function() {
      "variable_dictionary.csv"
    },
    content = function(file) {
      file.copy("www/variable_dictionary.csv", file)
    }
  )
}

shinyApp(ui = ui, server = server)