library(shiny)
library(shinyjs)
library(shinyWidgets)
library(rsconnect)
library(aws.s3)
library(metafor)
library(dplyr)
## Master App


# Set AWS credentials
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQMEY53X73DF2AD6C",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-west-2")


# pull variables from server
possible_vars <- s3read_using(FUN = read.csv, object = "possible_vars.csv", bucket = 'thinksharecare-beta1')

possible_predictors <- possible_vars[,1]

possible_outcomes <-  possible_vars[,2]


# Define the paths to the UI and server files
# contributor_ui_path <- file.path("..", "Contributor_App", "app.R")
# user_ui_path <- file.path("..", "User_App_Updated", "app.R")

# # Source the UI and server files
# source(contributor_ui_path, local = TRUE)$value
# source(user_ui_path, local = TRUE)$value

contributor_ui <- function(id) {
  ns = NS(id)
  fluidPage(
 
   # Add the header image at the top with a button above aligned to the right
  tags$div(
    style = "text-align: right; margin-bottom: 10px;",  # Align button to the right and add margin below
    downloadButton(ns("downloadDictionary"), "Download Variable Dictionary")),
  
  # App title and instructions
  titlePanel("Contributor: Data Upload and Variable Selection Tool"),
  
  sidebarLayout(
    sidebarPanel(
      # Instructions for file upload
      h4("Upload Your Data"),
      p("Data files must be in .csv format. The first row of the file can contain variable names, while the rest of the data should be numeric."),
      fileInput(ns("file"), "Choose CSV File", accept = ".csv"),
      
      # New heading and text for variable selection
      h4("Select Your Variables"),
      p("Please search and select the names of your variables in the order they are listed in your dataset. Variable 1 will be ID, and Variable 2 will be the variable listed in the second column of your dataset, and so on."),
      
      selectizeInput(ns("outcome"), 
                     label = "Outcome",
                     choices = c("Select Outcome" = "", possible_outcomes),
                     multiple = TRUE),  # Allows multiple selections
      
      selectizeInput(ns("predictor"), 
                     label = "Predictors",
                     choices = c("Select Predictors" = "", possible_predictors),
                     multiple = TRUE),
      
      actionButton(ns("runmodel"), "Run Model"),
      textOutput(ns("error_msg")), # Add a text output to display error messages
      verbatimTextOutput(ns("printResults")),
      uiOutput(ns("s3filename")),
      textOutput(ns("uploadStatus")),
      textOutput(ns("metaregUpdate")), # to provide update about whether metaregression can be performed
      
      ## In case you need to debug
      textOutput(ns("testprint")),
    ),
    
    mainPanel(
      tableOutput(ns("uploadedData"))
    )
  )
)
}

contributor_server <- function(input, output, session) {
  
  
  # Reactive value to store the data frame
  df <- reactiveVal(NULL)
  
  
  # Load and validate data when a file is uploaded
  observe({
    req(input$file)

    tryCatch({
      uploaded_df <- read.csv(input$file$datapath)
      
      # Check that the first row contains characters
      # if (!all(sapply(uploaded_df[1,], is.character))) {
      #   output$error_msg <- renderText("Error: The first row must contain character fields.")
      #   df(NULL)  # Reset the data frame
      #   return()
      # }
      
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
  
  # Handle run model action
  observeEvent(input$runmodel, {
    
    req(df())  # Ensure df is not NULL
    
    selected_predictors = input$predictor
    
    
    selected_predictors_and_outcome = c(input$outcome, selected_predictors)
    
    # Print the selected variables for demonstration
    # output$testprint <- renderText(paste0(selected_vars_and_outcome))
    # Build Model
    model_leftSide = paste0(selected_predictors_and_outcome[1], " ~ ")
    model_rightSide = paste0(selected_predictors_and_outcome[2:length(selected_predictors_and_outcome)],
                             collapse = " + ")
    
    # Print model
    fullModel_pasted = paste0("lm(", model_leftSide, model_rightSide, ", data)")
    output$printResults = renderText(fullModel_pasted)
    
    # Run model
    formula_pasted = paste0(model_leftSide, model_rightSide)
    model = lm(formula(formula_pasted), df())
    
    # Get coefficients
    summ = summary(model)
    coef = as.data.frame(summ$coefficients)
    weights = data.frame("predictor" = rownames(coef),
                         "weight" = coef$Estimate,
                         "se" = coef$`Std. Error`,
                         "r2" = summ$adj.r.squared,
                         "outcome" = c(selected_predictors_and_outcome[1], rep("", (nrow(coef)-1))))
    
    s3filename = paste0("individual_models/Outcome-", paste0(input$outcome),
                        "__Pred-", paste(selected_predictors, collapse = "+"), ".csv")
    
    write_csv_norownames <- function(object, file) {
      write.csv(object, file = file, row.names=F)
    }
    
    output$testprint = renderText(paste0("Saved file to bucket as: ", s3filename))
    
    s3write_using(x = weights,
                  FUN = write_csv_norownames,
                  object = s3filename,
                  bucket = "thinksharecare-beta1"
    )
    
    
    output$uploadStatus = renderText(paste0("Upload successful!"))
    
    
    #Make it so that it will look at the predictors that were run by contributors and match it up to model that is already in the things; some overlapping predictors
    ##########################################################
    model_files <- get_bucket('thinksharecare-beta1', prefix = "individual_models/")
    
    output$testprint = renderText(paste0(model_files))
    
    if ("Contents" %in% names(model_files)) {
      model_files <- sapply(model_files, function(x) x$Key)
    } else {
      model_files <- character(0)  # If no files are found, return an empty character vector
    }
    
    # --
    # Check the server for models for which all predictors are contained within the current model (provided by contributor). If there are overlapping models, re-run them in a meta-regression
    # --
    matched = 0 # initialize a variable that will keep track of the number of matched models
  
    # Iterate through the files in the s3 bucket
    for (file in model_files) {
      model_df <- s3read_using(FUN = read.csv, object = as.character(file), bucket = 'thinksharecare-beta1')
      variable_names <- model_df[2:nrow(model_df),1]
      
      ## If the predictors in the ith server model overlap with the currently provided model, pull the weights/standard errors and run the meta regression
      if (all(variable_names %in% selected_predictors) && model_df[1,5] == input$outcome) {

        if (matched == 0) {
          ## If there's a match, we want to create a dataframe initialized with the weights and standard errors from the current model
          pred_df = data.frame("predictor" = variable_names)
          
          ## add to dataframe the current weights and stderrs
          matched_effects = left_join(pred_df,
                                      select(weights, predictor, weight),
                                      by = "predictor")
          colnames(matched_effects)[2] = "model0_weight" # indicate that these weights correspond to the current model weights (i.e., the zero-th model)
          
          matched_stderrs = left_join(pred_df,
                                      select(weights, predictor, se),
                                      by = "predictor")
          colnames(matched_stderrs)[2] = "model0_stderr" # indicate that these standard errors correspond to the current model standard errors (i.e., of the zero-th model)
          
          matched = matched + 1
          
        } else {
          
          effName = paste0("model", matched, "_weight")
          seName = paste0("model", matched, "_stderr")
          
          matched_effects = left_join(matched_effects, 
                                      select(model_df, predictor, weight),
                                      by = "predictor")
          colnames(matched_effects)[matched + 1] = effName 
          
          matched_stderrs = left_join(matched_stderrs,
                                      select(model_df, predictor, se),
                                      by = "predictor")
          colnames(matched_stderrs)[matched + 1] = seName
          
          matched = matched + 1
        }
        
      }
    }
    
    ## If there are matches and we've pulled the corresponding effect sizes and standard errors, then we will run the meta-regression
    
    if (matched > 0) {

      output$metaregUpdate = renderText(paste0("Overlapping variables found. Running metaregression...."))
      
      
      # Create dataframe to store metareg coefficients;

      # for Neurohack24, we are only including I2 as a metric to assess the reliability/trustability of the metaregression weight. If editors choose to update this in the future, this will be the place in the code to edit to accomodate other metrics
      metaregOutput = data.frame("predictor" = variable_names,
                                 "weight" = rep(NA, length(variable_names)),
                                 "i2" = rep(NA, length(variable_names)))


      ## Iterate over rows in matched_effects dataframe. Each row corresponds to a different predictor in a multivariable model. Thus, for each predictor we will input its weights and standard errors (across models/studies) into the metaregression algorithm to 'tune' the estimate
      for (i in c(1:nrow(matched_effects))) {
        forMeta = data.frame("eff" = matched_effects[i, 2:ncol(matched_effects)],
                                "se" = matched_stderrs[i, 2:ncol(matched_stderrs)])

        metareg = rma.uni(yi = forMeta$eff,
                          sei = forMeta$se,
                          method = "REML")

        metaregOutput$weight[i] = as.numeric(metareg$beta)
        metaregOutput$i2[i] = as.numeric(metareg$I2)
      }
      
      ## write metaregOuput to .csv in the s3 server
      s3filename_deconstructed = strsplit(s3filename,
                                          split = ".",
                                          fixed = T)[[1]]
      
      s3filename_metareg = paste0(s3filename_deconstructed[1],
                                  "__METAREG.",
                                  s3filename_deconstructed[2])
      
      s3write_using(x = metaregOutput,
                    FUN = write_csv_norownames,
                    object = s3filename_metareg,
                    bucket = "thinksharecare-beta1"
      )
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

user_ui <- function(id) {
  ns = NS(id)
  fluidPage(
    # Add the header image at the top with a button above aligned to the right
    tags$div(
      style = "text-align: right; margin-bottom: 10px;",  # Align button to the right and add margin below
      downloadButton(ns("downloadDictionary"), "Download Variable Dictionary")),
    
    # Main content
    titlePanel("User: Model Selector and Risk Calculator"),
    
    sidebarLayout(
      sidebarPanel(
        tags$h3("Search and Select a Model", style = "font-size: 18px; margin-bottom: 20px;"),  # New header with margin
        
        tags$div(style = "margin-top: 20px;",  # Add margin above "Select Predictors:"
                 selectInput(ns("predictors"), "Select Predictors:", 
                             choices = c("Select Predictors" = "", possible_predictors),
                             selected = NULL, 
                             multiple = TRUE)
        ),
        
        selectInput(ns("outcome"), "Select Outcome:",
                    choices = c("Select Outcome" = "", possible_outcomes),
                    selected = NULL),
        
        actionButton(ns("searchModels"), "Search Models"),
        
        uiOutput(ns("modelList")),  # Output for displaying model names
        
        uiOutput(ns("variable_inputs")),
        
        textOutput(ns("testprint"))
        
      ),
      
      mainPanel(
        textOutput(ns("selectedModel")),
        uiOutput(ns("modelWeights")),  # Output for displaying model weights
        uiOutput(ns("result"))  # Output for displaying predicted values
      )
    )
  )
}
  

user_server <- function(input, output, session) {
  
  observeEvent(input$searchModels, {
    # Fetch the list of model files from S3
    model_files <- get_bucket('thinksharecare-beta1')
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
      model_df <- s3read_using(FUN = read.csv, object = as.character(file), bucket = 'thinksharecare-beta1')
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
      model_df <- s3read_using(FUN = read.csv, object = model_file, bucket = 'thinksharecare-beta1')
      
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
    model_df <- s3read_using(FUN = read.csv, object = model_file, bucket = 'thinksharecare-beta1')
    
    intercept <- as.numeric(model_df[1, 2])
    weights <- as.numeric(model_df[2:nrow(model_df), 2])
    variables <- model_df[2:nrow(model_df), 1]
    
    showModal(modalDialog(
      title = "Patient characteristics",
      uiOutput("predictors"),
      easyClose = T,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("proceed", "Proceed")
      )
    ))
    
    output$predictors <- renderUI({
      fields <- lapply(variables, function(var) {
        textInput(inputId = tolower(var), label = var)
      })
      do.call(tagList, fields)
    })
    
    observeEvent(input$proceed, {
      pt_values <- sapply(variables, function(var) input[[tolower(var)]])
      
      #output$testprint = renderText(paste0(pt_values, ":", weights))
      
      if (all(!is.na(pt_values))) {
        individual_terms <- sum(as.numeric(weights) * as.numeric(pt_values))
        
        predicted <- as.numeric(intercept + individual_terms)
        
        #HTML(paste0("<b>Predicted value: ", round(predicted, 3), "</b>"))
        output$testprint = renderText(paste0("Predicted value: ", round(predicted, 3)))
      } else {
        HTML("")
      }
      
      removeModal()
    })
    
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

# Define UI
ui <- navbarPage(
  title = NULL,
  id = "navbar",  # Add an id for controlling tab switching via JavaScript
  
  tabPanel("Main Page",
           fluidPage(
             # Add the header image at the top with a button above aligned to the right
             tags$div(
               tags$div(
                 style = "text-align: right; margin-bottom: 10px;",  # Align button to the right and add margin below
                 downloadButton("downloadDictionary", "Download Variable Dictionary")
               ),
               tags$img(src = "ThinkShareCare_Logo.png", style = "width: 100%; height: auto; max-width: 500px;"),
               style = "text-align: center; margin-bottom: 20px;"
             ),
             
             # Heading and description
             tags$h2("What is ThinkShareCare?"),
             tags$p("A tool nested within an interactive web app that hosts various linear models updated by the community. The weights (coefficients) from these models are accessible to end users (clinicians) such that users can input their own patient’s characteristics and be returned with a value representing their patient’s predicted outcome. In the long run, specific outcomes would be defined by the community (i.e., depending on what data contributors provide) and could entail treatment outcomes, diagnoses, subtypes of psychiatric disorders, etc. This is not to be used as a definitive diagnostic tool but rather as an additional piece of information to consider within the context of the larger clinical conceptualization."),
             
             # Two large buttons for navigation
             fluidRow(
               column(6, 
                      actionButton("contributorButton", "CONTRIBUTOR", 
                                   style = "width: 100%; height: 100px; font-size: 20px; background-color: #b4e4a4; color: black; border: none; border-radius: 8px;")
               ),
               column(6, 
                      actionButton("userButton", "USER", 
                                   style = "width: 100%; height: 100px; font-size: 20px; background-color: #e49cdc; color: black; border: none; border-radius: 8px;")
               )
             )
           )
  ),
  
  tabPanel("Contributor", contributor_ui("contributor")),
  tabPanel("User", user_ui("user")),
  
  # Include custom JavaScript to handle button clicks
  tags$script(HTML("
    document.getElementById('contributorButton').onclick = function() {
      $('#navbar').find('a[data-value=\"Contributor\"]').tab('show');
    };
    document.getElementById('userButton').onclick = function() {
      $('#navbar').find('a[data-value=\"User\"]').tab('show');
    };
  "))
)

# Define server logic
server <- function(input_main, output_main, session) {
  # Call the appropriate server logic based on the active tab
  observe({
    if (input_main$navbar == "Contributor") {
      callModule(contributor_server, "contributor")
    } else if (input_main$navbar == "User") {
      callModule(user_server, "user")
    }
  })
  
  # Download handler for variable dictionary
  output_main$downloadDictionary <- downloadHandler(
    filename = function() {
      "variable_dictionary.csv"
    },
    content = function(file) {
      file.copy("www/variable_dictionary.csv", file)
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)