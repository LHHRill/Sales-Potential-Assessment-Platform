# Load model performance data
model_performance <- readRDS("model_performance.rds")
source("global.R")
# Prepare model choices with accuracy
model_choices <- setNames(
  model_performance$Model,
  paste0(
    model_performance$Model,
    " (Accuracy: ",
    round(model_performance$Accuracy * 100, 2),
    "%)"
  )
)

# Prepare model choices with accuracy
model_choices <- paste0(
  c("Logistic Regression", "Lasso Regression", "Support Vector Machine", "Random Forest", "XGBoost"),
  " (Accuracy: ",
  round(model_performance$Accuracy * 100, 2),
  "%)"
)
names(model_choices) <- c("Logistic Regression", "Lasso Regression", "Support Vector Machine", "Random Forest", "XGBoost")

model_values <- model_performance$Model
names(model_values) <- model_choices

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Sales Potential Assessment"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Personal Info", tabName = "personal_info", icon = icon("user")),
      menuItem("Assertiveness", tabName = "assertiveness", icon = icon("chart-line")),
      menuItem("Social Confidence", tabName = "social_confidence", icon = icon("comments")),
      menuItem("Adventurousness", tabName = "adventurousness", icon = icon("globe")),
      menuItem("Dominance", tabName = "dominance", icon = icon("hand-rock")),
      menuItem("Assessment Results", tabName = "results", icon = icon("check")),
      menuItem("Model Evaluation", tabName = "model_evaluation", icon = icon("chart-bar"))
    ),
    br(),
    actionButton("submit", "Submit Assessment", icon = icon("paper-plane"), class = "btn-success")
  ),
  dashboardBody(
    useShinyjs(),
    shinyjs::hidden(
      generate_questions("AS"),
      generate_questions("SC"),
      generate_questions("AD"),
      generate_questions("DO")
    ),
    tabItems(
      # Personal Info Tab
      tabItem(
        tabName = "personal_info",
        h2("Personal Information"),
        fluidRow(
          column(6, textInput("age", "Age:", value = "", placeholder = "Enter your age")),
          column(6, selectInput("gender", "Gender:",
                                choices = c("Please select" = "", "Male" = "1", "Female" = "2", "Other" = "3"),
                                selected = ""))
        ),
        fluidRow(
          column(12, selectInput("degree_level", "What is the highest degree you have completed or are currently pursuing?",
                                 choices = c(
                                   "Please select" = "",
                                   "No Degree" = "No Degree",
                                   "Currently pursuing Bachelor's" = "Some College",
                                   "Associate's Degree" = "Associates",
                                   "Bachelor's Degree" = "Bachelors",
                                   "Master's Degree" = "Masters"
                                 ), selected = ""))
        ),
        fluidRow(
          column(6, selectInput("non_sandler_trained", "Received non-Sandler sales training?",
                                choices = c("Please select" = "", "Yes" = "yes", "No" = "no"),
                                selected = "")),
          column(6, selectInput("sandler_trained", "Received Sandler sales training?",
                                choices = c("Please select" = "", "Yes" = "yes", "No" = "no"),
                                selected = ""))
        ),
        fluidRow(
          column(6, selectInput(
            inputId = "prior_industry",
            label = "Prior Industry:",
            choices = c(
              "Please select" = "",
              "IT & Technology",
              "Financials",
              "Consumer Discretionary",
              "Industrials",
              "Healthcare",
              "Consumer Services",
              "Education",
              "Consulting",
              "Government & Non-Profit",
              "Others"
            ),
            selected = ""
          )),
          column(6, selectInput(
            inputId = "model_choice",
            label = "Select Prediction Model:",
            choices = model_values,
            selected = "Random Forest"
          ))
        ),
        numericInput("current_job_sales_years", "Years in current sales job:", value = NULL, min = 0, max = 50)
      ),
      # Assertiveness Tab
      tabItem(
        tabName = "assertiveness",
        h2("Assertiveness Assessment"),
        generate_questions("AS")
      ),
      # Social Confidence Tab
      tabItem(
        tabName = "social_confidence",
        h2("Social Confidence Assessment"),
        generate_questions("SC")
      ),
      # Adventurousness Tab
      tabItem(
        tabName = "adventurousness",
        h2("Adventurousness Assessment"),
        generate_questions("AD")
      ),
      # Dominance Tab
      tabItem(
        tabName = "dominance",
        h2("Dominance Assessment"),
        generate_questions("DO")
      ),
      # Results Tab
      tabItem(
        tabName = "results",
        h2("Assessment Results"),
        uiOutput("result_panel")
      ),
      # Model Evaluation Tab
      tabItem(
        tabName = "model_evaluation",
        h2("Model Evaluation"),
        fluidRow(
          box(
            title = "Feature Importance - Lasso Regression",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("lasso_plot"),
            width = 6
          ),
          box(
            title = "Feature Importance - Support Vector Machine",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("svm_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Feature Importance - Random Forest",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("rf_plot"),
            width = 6
          ),
          box(
            title = "Feature Importance - XGBoost",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("xgb_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Model Performance Comparison",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("model_performance_plot"),
            width = 12
          )
        )
      )
    )
  )
)
