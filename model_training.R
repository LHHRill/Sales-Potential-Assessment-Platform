# Load necessary libraries
library(ROSE)
library(pROC)
library(caret)
library(xgboost)
library(tidyverse)
library(recipes)
library(e1071)
library(ranger)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(stringdist)
library(ranger)
library(fastshap)
library(glmnet)
library(xgboost)
# Define global industry groups
industry_groups <- list(
  "IT & Technology" = c("technology", "web tech", "programmer", "high tech sales", "tech sales", "engineer", "tech", "techs", "it", "software"),
  "Financials" = c("banking", "financial", "insurance", "accounting", "financial analyst", "finance"),
  "Consumer Discretionary" = c("retail", "sale", "sales", "saleing", "relail"),
  "Industrials" = c("construction", "building", "manufacturing", "building and construction", "operations", "pilot"),
  "Healthcare" = c("health care", "nurse", "healthcare", "medical"),
  "Consumer Services" = c("restaurant", "food service", "hospitality", "customer service", "call center", "day care"),
  "Education" = c("teacher", "education", "training"),
  "Consulting" = c("consulting", "marketing"),
  "Government & Non-Profit" = c("government", "non-profit", "administration"),
  "Others" = c("other", "none", "unknown")
)

# Improved match_industry function
match_industry <- function(industry) {
  # Convert input to lowercase and remove extra spaces
  industry_input <- tolower(trimws(industry))
  
  # Remove punctuation and extra whitespace
  industry_input <- gsub("[[:punct:]]", "", industry_input)
  industry_input <- gsub("\\s+", " ", industry_input)
  
  # Initialize best match
  best_match <- "Others"
  lowest_distance <- Inf
  
  for (group in names(industry_groups)) {
    for (keyword in industry_groups[[group]]) {
      # Clean keyword
      keyword_clean <- tolower(trimws(keyword))
      keyword_clean <- gsub("[[:punct:]]", "", keyword_clean)
      
      # Calculate string distance
      distance <- stringdist(industry_input, keyword_clean, method = "jw")
      if (distance < lowest_distance) {
        lowest_distance <- distance
        best_match <- group
      }
    }
  }
  
  # Set a threshold for matching
  if (lowest_distance < 0.3) {
    return(best_match)
  } else {
    return("Others")
  }
}

# Read data
data <- read.csv("data.csv", stringsAsFactors = FALSE)

# Data preprocessing function
preprocess_data <- function(data) {
  # Convert factor variables
  factor_cols <- c("college.degree", "degree.level", "non.sandler.trained", "sandler.trained")
  data[factor_cols] <- lapply(data[factor_cols], factor)
  
  # Remove 'gender' variable to avoid bias
  data$gender <- NULL
  
  # Apply match_industry function
  data$industry_group <- sapply(data$prior.Industry, match_industry)
  
  # Convert avg.percent.of.revenue.goal to numeric
  data$avg.percent.of.revenue.goal <- as.numeric(gsub("%", "", data$avg.percent.of.revenue.goal)) / 100
  
  # Create target variable
  data$high_performer <- factor(ifelse(data$avg.percent.of.revenue.goal >= 0.6, "High", "Low"), levels = c("Low", "High"))
  
  # Bin experience
  data$experience_group <- cut(data$current.job.sales.years, 
                               breaks = c(-Inf, 1, 2, 3, 5, 7, 10, 15, Inf), 
                               labels = c("0-1", "1-2", "2-3", "3-5", "5-7", "7-10", "10-15", "15+"))
  
  # Process education level
  data$education_level <- factor(case_when(
    data$degree.level == "Masters" ~ "Masters",
    data$degree.level == "Bachelors" ~ "Bachelors",
    data$degree.level == "Associates" ~ "Associates",
    data$college.degree == "y" & data$degree.level == "none" ~ "Some College",
    data$college.degree == "n" & data$degree.level == "none" ~ "No College",
    TRUE ~ "Unknown"
  ), levels = c("No College", "Some College", "Associates", "Bachelors", "Masters", "Unknown"))
  
  # Create training combo variable
  data$training_combo <- factor(case_when(
    data$sandler.trained == "yes" & data$non.sandler.trained == "yes" ~ "Both",
    data$sandler.trained == "yes" & data$non.sandler.trained == "no" ~ "Sandler Only",
    data$sandler.trained == "no" & data$non.sandler.trained == "yes" ~ "Non-Sandler Only",
    data$sandler.trained == "no" & data$non.sandler.trained == "no" ~ "No Training"
  ))
  
  # Remove unnecessary columns
  data <- data %>% select(-college.degree, -degree.level,
                          -sandler.trained, -non.sandler.trained, -prior.Industry)
  
  return(data)
}

# Apply preprocessing
data <- preprocess_data(data)

# Ensure factor variables are correctly set
factor_vars <- c("education_level", "industry_group", "experience_group", "training_combo", "high_performer")
data[factor_vars] <- lapply(data[factor_vars], factor)

# Combine data to get all possible levels
combined_data <- data

# Get unique levels for each categorical variable
industry_levels <- levels(factor(combined_data$industry_group))
education_levels <- levels(factor(combined_data$education_level))
experience_levels <- c("0-1", "1-2", "2-3", "3-5", "5-7", "7-10", "10-15", "15+")
training_levels <- levels(factor(combined_data$training_combo))

data$industry_group <- factor(data$industry_group, levels = industry_levels)
data$education_level <- factor(data$education_level, levels = education_levels)
data$experience_group <- factor(data$experience_group, levels = experience_levels)
data$training_combo <- factor(data$training_combo, levels = training_levels)

# Feature selection function
select_features <- function(data) {
  # Select features for modeling
  feature_cols <- c("education_level", "industry_group", "experience_group", "training_combo", 
                    paste0("AS", 1:10), paste0("SC", 1:10), paste0("AD", 1:10), paste0("DO", 1:10))
  
  model_data <- data[, c(feature_cols, "high_performer")]
  
  return(model_data)
}

# Select features
model_data <- select_features(data)

# Split training and test sets
set.seed(123)
train_index <- createDataPartition(model_data$high_performer, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Ensure factor variables are correctly set
factor_vars <- c("education_level", "industry_group", "experience_group", "training_combo", "high_performer")
train_data[factor_vars] <- lapply(train_data[factor_vars], factor)
test_data[factor_vars] <- lapply(test_data[factor_vars], factor)

# Balance data using ROSE
set.seed(123)
train_data_balanced <- ovun.sample(high_performer ~ ., data = train_data, method = "both", N = nrow(train_data), p = 0.5, seed = 1)$data
train_data_balanced[factor_vars] <- lapply(train_data_balanced[factor_vars], factor)
train_data <- train_data_balanced

# Get feature names
feature_names <- setdiff(names(train_data), "high_performer")

# Define a custom naming function for dummy variables
custom_dummy_names <- function(var, lvl, ordinal = FALSE) {
  # Simplify variable names
  var_simple <- case_when(
    var == "industry_group" ~ "industry",
    var == "education_level" ~ "education",
    var == "experience_group" ~ "experience",
    var == "training_combo" ~ "training",
    TRUE ~ var  # Keep other variable names as is
  )
  
  # Create the dummy variable names
  paste0(var_simple, "_", make.names(lvl))
}

# Preprocessing recipe
preprocess_recipe <- recipe(high_performer ~ ., data = train_data) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE, naming = custom_dummy_names)


preprocess_recipe <- prep(preprocess_recipe, training = train_data, allow_novel_levels = TRUE)

# Apply preprocessing
train_data_processed <- bake(preprocess_recipe, new_data = train_data)
test_data_processed <- bake(preprocess_recipe, new_data = test_data)

# Ensure test and train sets have the same features
missing_cols <- setdiff(names(train_data_processed), names(test_data_processed))
for (col in missing_cols) {
  test_data_processed[[col]] <- 0
}
test_data_processed <- test_data_processed[, names(train_data_processed)]

# Identify and remove zero variance variables
zero_var_features <- nearZeroVar(train_data_processed, saveMetrics = TRUE)
zero_var_cols <- rownames(zero_var_features[zero_var_features$zeroVar == TRUE, ])

# Remove zero variance variables
if (length(zero_var_cols) > 0) {
  train_data_processed <- train_data_processed[, !names(train_data_processed) %in% zero_var_cols]
  test_data_processed <- test_data_processed[, !names(test_data_processed) %in% zero_var_cols]
}

# Update feature names
feature_names <- setdiff(names(train_data_processed), "high_performer")

# Define training control for logistic regression
lr_trcontrol <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Define lambda sequence for regularization
lambda_seq <- 10^seq(-4, 0, length = 100)

# Train logistic regression with elastic net regularization
set.seed(123)
cv_logistic <- train(
  high_performer ~ .,
  data = train_data_processed,
  method = "glmnet",
  family = "binomial",
  trControl = lr_trcontrol,
  metric = "ROC",
  tuneGrid = expand.grid(
    alpha = c(0, 0.5, 1),
    lambda = lambda_seq
  )
)
# Lasso Regression Optimization

# Load necessary library
library(glmnet)

# Define a wider range of lambda values
lambda_seq <- 10^seq(-5, 1, length = 100)

# Training control with repeated cross-validation
lasso_trcontrol <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Train the Lasso Regression model with the optimized parameters
set.seed(123)
cv_lasso <- train(
  high_performer ~ .,
  data = train_data_processed,
  method = "glmnet",
  family = "binomial",
  trControl = lasso_trcontrol,
  metric = "ROC",
  tuneGrid = expand.grid(
    alpha = 1,  # Lasso regression (L1 regularization)
    lambda = lambda_seq
  )
)

# Define parameter grid for SVM
svmGrid <- expand.grid(
  sigma = c(0.001, 0.005, 0.01, 0.02),
  C = c(0.1, 1, 10, 100)
)

# Training control for SVM
svm_trcontrol <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Train SVM model
set.seed(123)
cv_svm <- train(
  high_performer ~ .,
  data = train_data_processed,
  method = "svmRadial",
  trControl = svm_trcontrol,
  metric = "ROC",
  tuneGrid = svmGrid,
  preProcess = c("center", "scale")
)

# Do not balance data using ROSE
# Comment out the ROSE balancing
# set.seed(123)
# train_data_balanced <- ovun.sample(high_performer ~ ., data = train_data, method = "both", N = nrow(train_data), p = 0.5, seed = 1)$data
# train_data_balanced[factor_vars] <- lapply(train_data_balanced[factor_vars], factor)
# train_data <- train_data_balanced

# Instead, calculate class weights
class_counts <- table(train_data$high_performer)
class_weights <- sum(class_counts) / (length(class_counts) * class_counts)
names(class_weights) <- levels(train_data$high_performer)

# Ensure factor variables are correctly set
factor_vars <- c("education_level", "industry_group", "experience_group", "training_combo", "high_performer")
train_data[factor_vars] <- lapply(train_data[factor_vars], factor)
test_data[factor_vars] <- lapply(test_data[factor_vars], factor)

# Proceed with preprocessing as before
# Preprocessing recipe
preprocess_recipe <- recipe(high_performer ~ ., data = train_data) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE, naming = custom_dummy_names)

preprocess_recipe <- prep(preprocess_recipe, training = train_data, allow_novel_levels = TRUE)

# Apply preprocessing
train_data_processed <- bake(preprocess_recipe, new_data = train_data)
test_data_processed <- bake(preprocess_recipe, new_data = test_data)

# Ensure test and train sets have the same features
missing_cols <- setdiff(names(train_data_processed), names(test_data_processed))
for (col in missing_cols) {
  test_data_processed[[col]] <- 0
}
test_data_processed <- test_data_processed[, names(train_data_processed)]

# Identify and remove zero variance variables
zero_var_features <- nearZeroVar(train_data_processed, saveMetrics = TRUE)
zero_var_cols <- rownames(zero_var_features[zero_var_features$zeroVar == TRUE, ])

# Remove zero variance variables
if (length(zero_var_cols) > 0) {
  train_data_processed <- train_data_processed[, !names(train_data_processed) %in% zero_var_cols]
  test_data_processed <- test_data_processed[, !names(test_data_processed) %in% zero_var_cols]
}

# Update feature names
feature_names <- setdiff(names(train_data_processed), "high_performer")

# Random Forest Optimization

# Define a more extensive grid for hyperparameter tuning
rf_grid <- expand.grid(
  mtry = c(floor(sqrt(length(feature_names))), floor(length(feature_names)/3), floor(length(feature_names)/2), floor(2*length(feature_names)/3)),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Training control with repeated cross-validation
rf_trcontrol <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  allowParallel = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Convert class_weights to named vector
class_weights_vector <- c("Low" = class_weights["Low"], "High" = class_weights["High"])

# Train the Random Forest model with class weights
set.seed(123)
cv_rf <- train(
  high_performer ~ .,
  data = train_data_processed,
  method = "ranger",
  num.trees = 2000,  # Increased number of trees for better performance
  importance = 'impurity',
  trControl = rf_trcontrol,
  tuneGrid = rf_grid,
  metric = "ROC",
  class.weights = class_weights_vector
)

# Load necessary libraries
library(xgboost)
library(caret)

# Prepare data for XGBoost
train_matrix <- as.matrix(train_data_processed[, feature_names])
test_matrix <- as.matrix(test_data_processed[, feature_names])

# Convert labels to numeric format
train_labels <- ifelse(train_data_processed$high_performer == "High", 1, 0)
test_labels <- ifelse(test_data_processed$high_performer == "High", 1, 0)

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Set initial parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.05,  # Lower learning rate
  max_depth = 6,  # Increased depth to allow model to learn more complex patterns
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  gamma = 0,
  lambda = 1,  # L2 regularization
  alpha = 0  # L1 regularization
)

# Cross-validation to find the optimal number of rounds
set.seed(123)
cv.nfold <- 5
cv.nround <- 500
early_stopping_rounds <- 10

xgb_cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = cv.nround,
  nfold = cv.nfold,
  early_stopping_rounds = early_stopping_rounds,
  maximize = TRUE,
  verbose = FALSE
)

best_nrounds <- xgb_cv$best_iteration

# Train the final model with the optimal number of rounds
set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain),
  verbose = FALSE
)

# Evaluation function
evaluate_model <- function(model, test_data, test_labels, model_name) {
  if (class(model)[1] == "xgb.Booster") {
    pred_probs <- predict(model, as.matrix(test_data))
  } else {
    pred_probs <- predict(model, test_data, type = "prob")
    if (is.data.frame(pred_probs) || is.matrix(pred_probs)) {
      if ("High" %in% colnames(pred_probs)) {
        pred_probs <- pred_probs[, "High"]
      } else {
        pred_probs <- as.vector(pred_probs)
      }
    } else {
      pred_probs <- as.vector(pred_probs)
    }
  }
  
  # Handle potential NA values in pred_probs
  if (any(is.na(pred_probs))) {
    warning("Predicted probabilities contain NA values. Removing corresponding cases.")
    valid_indices <- which(!is.na(pred_probs))
    pred_probs <- pred_probs[valid_indices]
    test_labels <- test_labels[valid_indices]
  }
  
  # Calculate ROC curve
  roc_obj <- roc(test_labels, pred_probs)
  
  # Find the optimal threshold
  optimal_coords <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  optimal_thresh <- as.numeric(optimal_coords["threshold"])
  print(paste("Optimal Threshold for", model_name, ":", optimal_thresh))
  # Use the optimal threshold
  pred_classes <- ifelse(pred_probs >= optimal_thresh, "High", "Low")
  pred_classes <- factor(pred_classes, levels = c("Low", "High"))
  
  # Ensure test_labels is a factor with the same levels
  test_labels <- factor(test_labels, levels = c("Low", "High"))
  
  # Confusion Matrix
  cm <- confusionMatrix(pred_classes, test_labels, positive = "High")
  
  # Collect metrics
  evaluation <- list(
    confusion_matrix = cm,
    accuracy = cm$overall["Accuracy"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    f1_score = cm$byClass["F1"],
    auc = auc(roc_obj),
    optimal_thresh = optimal_thresh
  )
  
  return(evaluation)
}

# Evaluate models
test_labels <- factor(test_data$high_performer, levels = c("Low", "High"))

logistic_eval <- evaluate_model(cv_logistic, test_data_processed[, feature_names], test_labels, "Logistic Regression")
lasso_eval <- evaluate_model(cv_lasso, test_data_processed[, feature_names], test_labels, "Lasso Regression")
svm_eval <- evaluate_model(cv_svm, test_data_processed[, feature_names], test_labels, "Support Vector Machine")
rf_eval <- evaluate_model(cv_rf, test_data_processed[, feature_names], test_labels, "Random Forest")
xgb_eval <- evaluate_model(xgb_model, test_data_processed[, feature_names], test_labels, "XGBoost")

# Compare model performance
model_performance <- data.frame(
  Model = c("Logistic Regression", "Lasso Regression", "Support Vector Machine", "Random Forest", "XGBoost"),
  Accuracy = c(logistic_eval$accuracy, lasso_eval$accuracy, svm_eval$accuracy, rf_eval$accuracy, xgb_eval$accuracy),
  AUC = c(logistic_eval$auc, lasso_eval$auc, svm_eval$auc, rf_eval$auc, xgb_eval$auc)
)

# Save model performance for displaying in the app
saveRDS(model_performance, "model_performance.rds")

# Feature Importance Plots (Top 10 Features)
# Lasso Regression Feature Importance
lasso_coef <- coef(cv_lasso$finalModel, s = cv_lasso$bestTune$lambda)
lasso_coef_matrix <- as.matrix(lasso_coef)
lasso_coef_matrix <- lasso_coef_matrix[-1, , drop = FALSE]
feature_names_lasso <- rownames(lasso_coef_matrix)
lasso_importance <- abs(lasso_coef_matrix[, 1])
lasso_importance_df <- data.frame(
  Feature = feature_names_lasso,
  Importance = lasso_importance
)
lasso_importance_df <- lasso_importance_df[order(-lasso_importance_df$Importance), ]
lasso_top_features <- lasso_importance_df[1:10, ]

# Save Lasso Feature Importance
saveRDS(lasso_top_features, "lasso_importance.rds")

# SVM Feature Importance
svm_features <- train_data_processed[, -which(names(train_data_processed) == "high_performer")]
svm_target <- train_data_processed$high_performer
svm_importance <- filterVarImp(x = svm_features, y = svm_target)
svm_importance_df <- data.frame(
  Feature = rownames(svm_importance),
  Importance = svm_importance$High
)
svm_importance_df <- svm_importance_df %>% arrange(desc(Importance))
svm_top_features <- svm_importance_df[1:10, ]

# Save SVM Feature Importance
saveRDS(svm_top_features, "svm_importance.rds")

# Random Forest Feature Importance
rf_importance <- cv_rf$finalModel$variable.importance
rf_importance_df <- data.frame(
  Feature = names(rf_importance),
  Importance = as.numeric(rf_importance)
)
rf_importance_df <- rf_importance_df %>% arrange(desc(Importance))
rf_top_features <- rf_importance_df[1:10, ]

# Save Random Forest Feature Importance
saveRDS(rf_top_features, "rf_importance.rds")

# XGBoost Feature Importance
xgb_importance <- xgb.importance(
  feature_names = feature_names,
  model = xgb_model
)
xgb_top_features <- xgb_importance[1:10, ]
xgb_importance_df <- data.frame(
  Feature = xgb_top_features$Feature,
  Importance = xgb_top_features$Gain
)

# Save XGBoost Feature Importance
saveRDS(xgb_importance_df, "xgb_importance.rds")

# Save all models
saveRDS(cv_logistic, "logistic_model.rds")
saveRDS(cv_lasso, "lasso_model.rds")
saveRDS(cv_svm, "svm_model.rds")
saveRDS(cv_rf, "random_forest_model.rds")
saveRDS(xgb_model, "xgboost_model.rds")
saveRDS(preprocess_recipe, "preprocess_recipe.rds")

# Save optimal thresholds
optimal_thresholds <- list(
  "Logistic Regression" = logistic_eval$optimal_thresh,
  "Lasso Regression" = lasso_eval$optimal_thresh,
  "Support Vector Machine" = svm_eval$optimal_thresh,
  "Random Forest" = rf_eval$optimal_thresh,
  "XGBoost" = xgb_eval$optimal_thresh
)
saveRDS(optimal_thresholds, "optimal_thresholds.rds")
# 在模型训练结束后，更新 feature_names
feature_names <- setdiff(names(train_data_processed), "high_performer")
saveRDS(train_data_processed, "train_data_processed.rds")

# 保存 feature_names
saveRDS(feature_names, "feature_names.rds")
saveRDS(train_data, "train_data.rds") 
# Print the model performance
print("Model Performance:")
print(model_performance)
saveRDS(train_data, "train_data.rds")
# Alternatively, print each model's performance individually
for (i in 1:nrow(model_performance)) {
  cat("Model:", model_performance$Model[i], "\n")
  cat("  Accuracy:", round(model_performance$Accuracy[i] * 100, 2), "%\n")
  cat("  AUC:", round(model_performance$AUC[i], 4), "\n\n")
}

library(ggplot2)
library(dplyr)
library(stringr)
# Assuming rf_importance_df is already available
# Sort the importance in descending order
rf_importance_df_sorted <- rf_importance_df %>% arrange(desc(Importance))

# Select top 10 features
top10_features <- rf_importance_df_sorted %>% head(10)

# Plot
p1 <- ggplot(top10_features, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = 'identity', fill = 'purple') +
  coord_flip() +
  labs(title = 'Random Forest Top 10 Feature Importances', x = 'Feature', y = 'Importance') +
  theme_minimal(base_size = 12)

# Display the plot
print(p1)
# Select bottom 10 features
bottom10_features <- rf_importance_df_sorted %>% tail(10)

# Plot
p2 <- ggplot(bottom10_features, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = 'identity', fill = 'purple') +
  coord_flip() +
  labs(title = 'Random Forest Bottom 10 Feature Importances', x = 'Feature', y = 'Importance') +
  theme_minimal(base_size = 12)

# Display the plot
print(p2)
# Filter for personality traits
personality_features <- rf_importance_df_sorted %>%
  filter(str_detect(Feature, '^(AS|SC|AD|DO)\\d+$'))

# Select top 10 personality traits
top10_personality <- personality_features %>% head(10)

# Plot
p3 <- ggplot(top10_personality, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() +
  labs(title = 'Random Forest Top 10 Personality Traits Importances', x = 'Personality Trait', y = 'Importance') +
  theme_minimal(base_size = 12)

# Display the plot
print(p3)
# Select bottom 10 personality traits
bottom10_personality <- personality_features %>% tail(10)

# Plot
p4 <- ggplot(bottom10_personality, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() +
  labs(title = 'Random Forest Bottom 10 Least Important Personality Traits', x = 'Personality Trait', y = 'Importance') +
  theme_minimal(base_size = 12)

# Display the plot
print(p4)