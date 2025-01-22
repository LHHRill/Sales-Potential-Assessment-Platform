library(googlesheets4)
source("global.R")
# Authenticate with the service account
gs4_auth(path = "sandler-applicant-data-a4eb71c49c3b.json")
sheet_id <- "1jMyf1iWKVJs0q-NtdqkiojMS_nAaeHQPIzpa06TsKmA"
# Load necessary models and data
logistic_model <- readRDS("logistic_model.rds")
lasso_model <- readRDS("lasso_model.rds")
svm_model <- readRDS("svm_model.rds")
rf_model <- readRDS("random_forest_model.rds")
xgb_model <- readRDS("xgboost_model.rds")
preprocess_recipe <- readRDS("preprocess_recipe.rds")
optimal_thresholds <- readRDS("optimal_thresholds.rds")
model_performance <- readRDS("model_performance.rds")

# Load feature importance data
lasso_importance <- readRDS("lasso_importance.rds")
svm_importance <- readRDS("svm_importance.rds")
rf_importance <- readRDS("rf_importance.rds")
xgb_importance <- readRDS("xgb_importance.rds")

# Get feature names from the model
feature_names <- readRDS("feature_names.rds")

# Server logic
server <- function(input, output, session) {
  # Input validation
  iv <- InputValidator$new()
  
  # Age validation
  iv$add_rule("age", sv_required())
  iv$add_rule("age", ~ if (!is.na(as.numeric(.)) && as.numeric(.) >= 18 && as.numeric(.) <= 70) NULL else "Must be between 18 and 70.")
  
  # Gender validation
  iv$add_rule("gender", ~ if (. == "") "Please select your gender" else NULL)
  
  # Degree level validation
  iv$add_rule("degree_level", ~ if (. == "") "Please select your highest degree" else NULL)
  
  # Training questions validation
  iv$add_rule("non_sandler_trained", ~ if (. == "") "Please indicate if you have received non-Sandler sales training" else NULL)
  iv$add_rule("sandler_trained", ~ if (. == "") "Please indicate if you have received Sandler sales training" else NULL)
  
  # Prior industry validation
  iv$add_rule("prior_industry", ~ if (. == "") "Please select your prior industry" else NULL)
  
  # Current job sales years validation
  iv$add_rule("current_job_sales_years", sv_required())
  iv$add_rule("current_job_sales_years", sv_between(0, 50))
  
  # Enable validation
  iv$enable()
  
  # Reactive applicant data
  applicant_data <- reactive({
    req(iv$is_valid())
    data <- list(
      age = as.numeric(input$age),
      gender = input$gender,
      degree_level = input$degree_level,
      non_sandler_trained = input$non_sandler_trained,
      sandler_trained = input$sandler_trained,
      prior_industry = input$prior_industry,
      current_job_sales_years = as.numeric(input$current_job_sales_years)
    )
    
    for (trait in names(personality_questions)) {
      for (i in 1:10) {
        input_name <- paste0(trait, i)
        data[[input_name]] <- if (!is.null(input[[input_name]])) input[[input_name]] else 3
      }
    }
    data
  })
  
  evaluate_applicant <- function(data) {
    # Process education level
    data$education_level <- factor(case_when(
      data$degree_level == "Masters" ~ "Masters",
      data$degree_level == "Bachelors" ~ "Bachelors",
      data$degree_level == "Associates" ~ "Associates",
      data$degree_level == "Some College" ~ "Some College",
      data$degree_level == "No Degree" ~ "No College",
      TRUE ~ "Unknown"
    ), levels = c("No College", "Some College", "Associates", "Bachelors", "Masters", "Unknown"))
    
    # Process industry group
    data$industry_group <- factor(data$prior_industry, levels = c(
      "IT & Technology", "Financials", "Consumer Discretionary", "Industrials", "Healthcare",
      "Consumer Services", "Education", "Consulting", "Government & Non-Profit", "Others"
    ))
    
    # Process experience group
    data$experience_group <- cut(as.numeric(data$current_job_sales_years),
                                 breaks = c(-Inf, 1, 2, 3, 5, 7, 10, 15, 20, Inf),
                                 labels = c("0-1", "1-2", "2-3", "3-5", "5-7", "7-10", "10-15", "15-20", "20+")
    )
    data$experience_group <- factor(data$experience_group, levels = c("0-1", "1-2", "2-3", "3-5",
                                                                      "5-7", "7-10", "10-15", "15-20", "20+"))
    
    # Process training combo
    data$training_combo <- factor(case_when(
      data$sandler_trained == "yes" & data$non_sandler_trained == "yes" ~ "Both",
      data$sandler_trained == "yes" & data$non_sandler_trained == "no" ~ "Sandler Only",
      data$sandler_trained == "no" & data$non_sandler_trained == "yes" ~ "Non-Sandler Only",
      data$sandler_trained == "no" & data$non_sandler_trained == "no" ~ "No Training"
    ), levels = c("No Training", "Non-Sandler Only", "Sandler Only", "Both"))
    
    # Convert personality scores to numeric
    for (trait in c("AS", "SC", "AD", "DO")) {
      for (i in 1:10) {
        var_name <- paste0(trait, i)
        data[[var_name]] <- as.numeric(data[[var_name]])
      }
    }
    data$age <- NULL
    data$gender <- NULL
    applicant_df <- data.frame(as.list(data), stringsAsFactors = FALSE)
    
    # Apply preprocessing
    processed_data <- bake(preprocess_recipe, new_data = applicant_df)
    
    # Ensure all expected features are present and remove any extra features
    missing_features <- setdiff(feature_names, names(processed_data))
    extra_features <- setdiff(names(processed_data), feature_names)
    
    if (length(missing_features) > 0) {
      # Add missing features with zero value
      for (feature in missing_features) {
        processed_data[[feature]] <- 0
      }
    }
    
    if (length(extra_features) > 0) {
      # Remove extra features
      processed_data <- processed_data[, !(names(processed_data) %in% extra_features)]
    }
    
    # Reorder columns to match feature_names
    processed_data <- processed_data[, feature_names]
    
    # Predict using the selected model
    model_name_full <- input$model_choice
    model_name <- str_trim(str_split(model_name_full, "-")[[1]][1])
    
    if (model_name == "Logistic Regression") {
      model <- logistic_model
      pred_probs <- predict(model, newdata = processed_data, type = "prob")
    } else if (model_name == "Lasso Regression") {
      model <- lasso_model
      pred_probs <- predict(model, newdata = processed_data, type = "prob")
    } else if (model_name == "Support Vector Machine") {
      model <- svm_model
      pred_probs <- predict(model, newdata = processed_data, type = "prob")
    } else if (model_name == "Random Forest") {
      model <- rf_model
      pred_probs <- predict(model, newdata = processed_data, type = "prob")
    } else if (model_name == "XGBoost") {
      model <- xgb_model
      # For XGBoost, the predict function returns raw scores; apply logistic function
      pred_scores <- predict(model, newdata = as.matrix(processed_data))
      pred_probs <- data.frame(High = pred_scores, Low = 1 - pred_scores)
    } else {
      stop("Invalid model selected")
    }
    
    # Extract probability of the "High" class
    if (is.data.frame(pred_probs) && "High" %in% colnames(pred_probs)) {
      pred <- pred_probs[,"High"]
    } else {
      pred <- as.vector(pred_probs)
    }
    
    # Ensure pred is a single value
    if (length(pred) > 1) {
      warning("Multiple predicted probabilities returned, using the first one.")
      pred <- pred[1]
    }
    
    # Get optimal threshold for the selected model
    optimal_thresh <- optimal_thresholds[[model_name]]
    
    # Determine potential
    if (pred >= optimal_thresh) {
      result_text <- "You have potential to become a successful salesperson."
      probability <- pred
      alternative <- NULL
    } else {
      result_text <- "You might be better suited for a different role."
      probability <- 1 - pred
      alternative <- recommend_career(data)
    }
    
    return(list(
      prediction = result_text,
      confidence = probability,
      alternative_careers = alternative
    ))
  }
  
  # Career recommendation function
  recommend_career <- function(data) {
    # Calculate average scores for personality traits
    assertiveness <- mean(as.numeric(unlist(data[paste0("AS", 1:10)])))
    social_confidence <- mean(as.numeric(unlist(data[paste0("SC", 1:10)])))
    adventurousness <- mean(as.numeric(unlist(data[paste0("AD", 1:10)])))
    dominance <- mean(as.numeric(unlist(data[paste0("DO", 1:10)])))
    
    # Recommend careers based on personality traits
    recommendations <- c()
    if (assertiveness > 3.5 && social_confidence > 3.5) {
      recommendations <- c(recommendations, "Marketing", "Public Relations")
    }
    if (adventurousness > 3.5 && dominance > 3.5) {
      recommendations <- c(recommendations, "Entrepreneurship", "Business Development")
    }
    if (social_confidence > 3.5 && dominance < 3) {
      recommendations <- c(recommendations, "Customer Service", "Human Resources")
    }
    if (assertiveness > 3.5 && dominance > 3.5) {
      recommendations <- c(recommendations, "Management", "Leadership")
    }
    if (adventurousness > 3.5 && social_confidence < 3) {
      recommendations <- c(recommendations, "Research", "Data Analysis")
    }
    if (length(recommendations) == 0) {
      recommendations <- c("Administrative Support", "Operations")
    }
    
    return(recommendations)
  }
  
  # Submit button observer
  observeEvent(input$submit, {
    req(iv$is_valid())
    
    # Evaluate applicant with the selected model
    res <- evaluate_applicant(applicant_data())
    
    # Render the result panel
    output$result_panel <- renderUI({
      data <- applicant_data()
      
      fluidRow(
        box(
          title = "Assessment Summary",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          fluidRow(
            column(
              width = 4,
              h4("Applicant Information"),
              img(src = "https://www.w3schools.com/howto/img_avatar.png", height = "100px"),
              tags$ul(
                tags$li(strong("Age: "), data$age),
                tags$li(strong("Gender: "), ifelse(data$gender == "1", "Male",
                                                   ifelse(data$gender == "2", "Female", "Other")
                )),
                tags$li(strong("Degree Level: "), data$degree_level),
                tags$li(strong("Sandler Trained: "), data$sandler_trained),
                tags$li(strong("Prior Industry: "), data$prior_industry),
                tags$li(strong("Years in Sales Job: "), data$current_job_sales_years),
                tags$li(strong("Selected Model: "), input$model_choice)
              )
            ),
            column(
              width = 4,
              h4("Prediction"),
              valueBox(
                value = paste0(round(res$confidence * 100, 2), "%"),
                subtitle = res$prediction,
                icon = icon("chart-line"),
                color = ifelse(res$prediction == "You have potential to become a successful salesperson.", "green", "orange"),
                width = NULL
              )
            ),
            column(
              width = 4,
              h4("Recommended Careers"),
              if (is.null(res$alternative_careers)) {
                "Consider pursuing a career in sales."
              } else {
                tags$ul(
                  lapply(res$alternative_careers, tags$li)
                )
              }
            )
          )
        )
      )
    })
    
    # Read the existing data from the sheet
    existing_data <- tryCatch(
      read_sheet(sheet_id),
      error = function(e) NULL
    )
    
    # Generate ImportId and record id
    if (is.null(existing_data) || nrow(existing_data) == 0) {
      new_record_id <- 1
    } else {
      existing_record_ids <- as.numeric(existing_data$record_id)
      existing_record_ids <- existing_record_ids[!is.na(existing_record_ids)]
      if (length(existing_record_ids) > 0) {
        max_record_id <- max(existing_record_ids)
      } else {
        max_record_id <- 0
      }
      new_record_id <- max_record_id + 1
    }
    import_id <- new_record_id  # Assuming ImportId is the same as record_id
    
    # Collect all data into a data frame
    applicant_df <- as.data.frame(applicant_data())
    
    # Add ImportId and record_id
    applicant_df$ImportId <- import_id
    applicant_df$record_id <- new_record_id
    
    # Add Prediction, Assessment Results, and Recommended Careers
    applicant_df$Prediction <- paste0(round(res$confidence * 100, 2), "%")
    applicant_df$Assessment_Results <- res$prediction
    if (is.null(res$alternative_careers)) {
      applicant_df$Recommended_Careers <- "Consider pursuing a career in sales."
    } else {
      applicant_df$Recommended_Careers <- paste(res$alternative_careers, collapse = "; ")
    }
    # Add Selected Model
    applicant_df$Selected_Model <- input$model_choice
    
    # Reorder columns as per requirement
    required_columns <- c("ImportId", "record_id", "age", "gender", "degree_level",
                          "non_sandler_trained", "sandler_trained", "prior_industry", "current_job_sales_years",
                          paste0("AS", 1:10), paste0("SC", 1:10), paste0("AD", 1:10), paste0("DO", 1:10),
                          "Prediction", "Assessment_Results", "Recommended_Careers", "Selected_Model")
    
    # Ensure all required columns are present
    applicant_df <- applicant_df[, required_columns]
    
    # Check if the sheet is empty or needs initialization
    if (is.null(existing_data) || nrow(existing_data) == 0) {
      # Sheet is empty, write data with headers
      sheet_write(data = applicant_df, ss = sheet_id)
    } else {
      # Append data to Google Sheet
      sheet_append(ss = sheet_id, data = applicant_df)
    }
    
    # Switch to results tab
    updateTabItems(session, "tabs", "results")
  })
  
  # Lasso Regression Feature Importance Plot
  output$lasso_plot <- renderPlotly({
    p <- ggplot(lasso_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = 'identity', fill = 'steelblue') +
      coord_flip() +
      labs(title = 'Lasso Regression Feature Importance (Top 10 Features)', x = 'Feature', y = 'Importance') +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # SVM Feature Importance Plot
  output$svm_plot <- renderPlotly({
    p <- ggplot(svm_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = 'identity', fill = 'darkgreen') +
      coord_flip() +
      labs(title = 'Support Vector Machine Feature Importance (Top 10 Features)', x = 'Feature', y = 'Importance') +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # Random Forest Feature Importance Plot
  output$rf_plot <- renderPlotly({
    p <- ggplot(rf_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = 'identity', fill = 'purple') +
      coord_flip() +
      labs(title = 'Random Forest Feature Importance (Top 10 Features)', x = 'Feature', y = 'Importance') +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # XGBoost Feature Importance Plot
  output$xgb_plot <- renderPlotly({
    p <- ggplot(xgb_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = 'identity', fill = 'orange') +
      coord_flip() +
      labs(title = 'XGBoost Feature Importance (Top 10 Features)', x = 'Feature', y = 'Importance') +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })
  
  # Model Performance Comparison Plot
  output$model_performance_plot <- renderPlotly({
    library(reshape2)
    
    # Reshape the data to long format
    performance_long <- melt(model_performance, id.vars = "Model")
    
    # Rename the 'variable' column to 'Metric' to avoid confusion
    colnames(performance_long) <- c("Model", "Metric", "Value")
    
    # Create the plot
    p <- ggplot(performance_long, aes(x = Model, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Model Performance Comparison", y = "Value", x = "Model") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 12)
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p)
  })
}