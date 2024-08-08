#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)

ui <- fluidPage(
  titlePanel("Patient characteristics"),
  sidebarLayout(
    sidebarPanel(
      fileInput("model_file", "Upload Model File", accept = c(".csv")),
      uiOutput("variable_inputs")
    ),
    mainPanel(
      verbatimTextOutput("weights"),
      htmlOutput("result")
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  model_data <- reactive({
    req(input$model_file)
    data <- read.csv(input$model_file$datapath)
    data
  })
  
  # Reactive expression to read the model file
  model_vars <- reactive({
    data = model_data()
    model_vars <- as.character(data[, "var"])
    model_vars
  })
  
  model_vals <- reactive({
    data = model_data()
    model_data <- read.csv(input$model_file$datapath)
    model_vals <- as.numeric(data[,"coef"])
    model_vals
  })
  
  # Display the weights
  output$weights <- renderPrint({
    vars <- model_vars()
    vals = model_vals()
    
    weights <- round(vals, 3)
    
    names(weights) <- vars
    weights
  })
    
  
  # Render the input fields based on the uploaded model
  output$variable_inputs <- renderUI({
    vars <- model_vars()
    
    input_list <- lapply(vars[2:length(vars)], function(var) {
      numericInput(inputId = var, label = var, value = NULL)
    })
    
    do.call(tagList, input_list)
  })
  
  # Create predicted values
  output$result <- renderUI({
    
    # Pull weights from linear model
    allWeights = model_vals()
    
    intercept = allWeights[1] ## take intercept 'weight'/coefficient
    weights = allWeights[2:length(allWeights)] # take weights for individual terms
    
    vars = model_vars()
    
    # take user inputs (i.e., patient characteristics)
    pt_values = sapply(vars[2:length(vars)], function(var) input[[var]]) 
    
    # Only print predicted value if all of the fields have been populated
    if (all(!is.na(pt_values))) {
      # Compute predicted values
      individual_terms = sum(weights*pt_values) # multiply weights by each patient value
      predicted = intercept + individual_terms # add the intercept term to the above
      
      HTML(paste0("<b>Predicted value: ", round(predicted, 3), "</b>"))
      
    } else {
      HTML("")
    }
    

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
