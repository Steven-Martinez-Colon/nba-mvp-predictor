############## Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix", "Metrics"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

######################## Loading Data ##############################

# MVP data from 1980-2021
# Data was found through kaggle and it has already been cleaned
master_table <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1621KKM90l1uTYBHlH7ConHnMlaNvvADP1gorRyqFVrE/edit?gid=1076660604#gid=1076660604")

# Renaming WS/48
master_table <- master_table %>% rename(WS_per_48 = `WS/48`)

# Renaming W/L% to win_loss_ratio
master_table <- master_table %>% rename(win_loss_ratio = `W/L%`)

####################### Exploratory Data Analysis ###############################

# Select only numeric columns and compute correlation
cor_matrix <- cor(select_if(master_table, is.numeric), use = "pairwise.complete.obs")

# Extract correlations with K%
share_cor <- cor_matrix[, "Share"] %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Correlation = ".")

# View top correlations
print(share_cor)

# Create correlation matrix heatmap
ggcorrplot(share_cor, 
           method = "square",   # Use "square" or "circle" instead of "color"
           type = "lower", 
           lab = FALSE, 
           lab_size = 3, 
           colors = c("blue", "white", "red"), 
           title = "Correlation Matrix") +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 8))



# Value over replacement vs Share plot
vorp_share_plot <- ggplot(data = master_table, 
                          aes(x = value_over_replacement_player, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Value Over Replacement vs. MVP Share") +
  theme_bw()

print(vorp_share_plot)

# Box Plus/Minus vs Share plot
box_plus_minus_share_plot <- ggplot(data = master_table, 
                                    aes(x = box_plus_minus, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Box Plus/Minus vs. MVP Share") +
  theme_bw()

print(box_plus_minus_share_plot)

# Win Shares vs Share plot
ws_share_plot <- ggplot(data = master_table, 
                        aes(x = WS, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Win Shares vs. MVP Share") +
  theme_bw()

print(ws_share_plot)

# WS/48 vs Share plot
ws_48_share_plot <- ggplot(data = master_table, 
                           aes(x = WS_per_48, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Win Shares per 48 Minutes vs. MVP Share") +
  theme_bw()

print(ws_48_share_plot)

# Player Efficiency Rating vs Share plot
per_share_plot <- ggplot(data = master_table, 
                         aes(x = player_efficiency_rating, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Player Efficiency Rating vs. MVP Share") +
  theme_bw()

print(per_share_plot)

# Team Seeding vs Share plot
seed_share_plot <- ggplot(data = master_table,
                          aes(x = seed, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Team Seeding vs. MVP Share") +
  theme_bw()

print(seed_share_plot)

# Points per game vs Share plot
pts_share_plot <- ggplot(data = master_table, 
                         aes(x = PTS, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Points per Game vs. MVP Share") +
  theme_bw()

print(pts_share_plot)

# Offensive box plus/minus vs share plot
off_box_plus_minus_share_plot <- ggplot(data = master_table, 
                                        aes(x = offensive_box_plus_minus,
                                            y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Offensive Box Plus/Minus vs. MVP Share") +
  theme_bw()

print(off_box_plus_minus_share_plot)

# Offensive win shares vs Shares plot
off_ws_plot <- ggplot(data = master_table,
                 aes(x = offensive_win_shares,
                     y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Offensive Win Shares vs. MVP Share") +
  theme_bw()

print(off_ws_plot)

# Usage percentage vs Shares plot
usage_percentage_plot <- ggplot(data = master_table,
                 aes(x = usage_percentage,
                     y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Usage Percentage vs. MVP Share") +
  theme_bw()

print(usage_percentage_plot)

# Win/loss ratio vs Shares plot
win_loss_ratio_plot <- ggplot(data = master_table,
                                aes(x = win_loss_ratio,
                                    y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "MVP",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Win/Loss Ratio vs. MVP Share") +
  theme_bw()

print(win_loss_ratio_plot)

# Finding the average for each variable by rank
average_by_rank <- master_table %>%
  group_by(Rank) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Looking at the averages of the variables for the first five ranks
average_by_rank_1_thru_5 <- average_by_rank %>% 
  filter(Rank %in% c("1", "2", "3", "4", "5"))   # Selecting just the first five ranks


########################### Linear Regression Model ################################

# We are going to start with a linear model with the following predictors:
# "value_over_replacement_player", "WS", "box_plus_minus", "WS_per_48", "player_efficiency_rating"

# Features for prediction
features <- c("value_over_replacement_player", "WS", "box_plus_minus", "WS_per_48", "player_efficiency_rating")

target <- "Share"

# Setting seed
set.seed(123)

# Initializing counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for leave one year out cross-validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Train Linear Regression model
  formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
  model <- lm(formula, data = train_data)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}


# Compute final metrics
lm_average_mae <- mean(mae_values, na.rm = TRUE)
lm_average_r2 <- mean(r2_values, na.rm = TRUE)
lm_accuracy <- correct_predictions / total_years

# Print results
print(lm_average_mae)
print(lm_average_r2)
print(lm_accuracy)

# We only got an accuracy of 57%, so we are going to adjust some of the predictors and see what happens.
# If we are not satisfies with the new results, we are going to explore other models.

features_2 <- c("value_over_replacement_player", "WS", "box_plus_minus", "WS_per_48", "player_efficiency_rating",
                "PTS", "seed", "win_loss_ratio", "usage_percentage")

# Setting seed
set.seed(123)

# Initializing counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for leave one year out cross-validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Train Linear Regression model
  formula <- as.formula(paste(target, "~", paste(features_2, collapse = " + ")))
  model <- lm(formula, data = train_data)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}


# Compute final metrics
lm2_average_mae <- mean(mae_values, na.rm = TRUE)
lm2_average_r2 <- mean(r2_values, na.rm = TRUE)
lm2_accuracy <- correct_predictions / total_years

# Print results
print(lm2_average_mae)
print(lm2_average_r2)
print(lm2_accuracy)

# The model predictions slightly improved from 57% to 64%. However, this is still not great. 
# I think we should explore some nonlinear models instead.

########################## Random Forest Model ############################

# We are now going to try a random forest model.
# We are going to try this model two times. The first time with the first features we originally had (features).
# Second time will be with the features we added after (features_2).

# Setting seed
set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Convert features into a formula format for Random Forest
  formula <- as.formula(paste("Share", "~", paste(features, collapse = " + ")))
  
  # Train Random Forest model
  rf_model <- randomForest(formula, data = train_data, ntree = 500, importance = TRUE)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(rf_model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
rf_accuracy <- correct_predictions / total_years
rf_average_mae <- mean(mae_values, na.rm = TRUE)
rf_average_r2 <- mean(r2_values, na.rm = TRUE)

# Print results
print(rf_average_mae)
print(rf_average_r2)
print(rf_accuracy)

# This model did not perform well either with just 57.1% accuracy.

# Second time with second set of features (features_2)

# Setting seed
set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Convert features into a formula format for Random Forest
  formula <- as.formula(paste("Share", "~", paste(features_2, collapse = " + ")))
  
  # Train Random Forest model
  rf_model <- randomForest(formula, data = train_data, ntree = 500, importance = TRUE)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(rf_model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
rf2_accuracy <- correct_predictions / total_years
rf2_average_mae <- mean(mae_values, na.rm = TRUE)
rf2_average_r2 <- mean(r2_values, na.rm = TRUE)

# Print results
print(rf2_average_mae)
print(rf2_average_r2)
print(rf2_accuracy)

# This model performed much better with the second set of features by achieving 71.4% accuracy.
# However, we are going to keep exploring other options and try to be more accurate.


###################### Gradient Boosting Model (XGBoost) ####################

# We are now going to explore a XGBoost model since the random forest model performed relatively well.
# A XGBoost model would be ideal because it's more powerful than random forest because it boosts weak decision trees,
# reducing errors and improving predictive accuracy.

# Once again, we are going to run this model twice with both set of features.

# Setting seed
set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Convert data to matrix format for XGBoost
  X_train <- as.matrix(train_data %>% select(all_of(features)))
  y_train <- train_data$Share
  
  X_test <- as.matrix(test_data %>% select(all_of(features)))
  y_test <- test_data$Share   # Actual MVP Share values
  
  # Convert to XGBoost DMatrix format
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test)
  
  # Train XGBoost model
  xgb_model <- xgboost(data = dtrain, 
                       nrounds = 100, # number of boosting iterations to train
                       objective = "reg:squarederror", # used for regression problems and minimizes squared error loss.
                       max_depth = 6, # max depth of each tree
                       eta = 0.1, # learning rate
                       subsample = 0.8, # Fraction of training data randomly sampled for each boosting round.
                       colsample_bytree = 0.8, # Fraction of features (columns) randomly selected for each tree.
                       verbose = 0) # no output during training
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(xgb_model, dtest)
  
  # Compute MAE and R-Squared for this year
  mae_values <- c(mae_values, mae(y_test, test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
xgb_average_mae <- mean(mae_values)
xgb_average_r2 <- mean(r2_values)
xgb_accuracy_xgb <- correct_predictions / total_years

# Printing results
print(xgb_average_mae)
print(xgb_average_r2)
print(xgb_accuracy_xgb)

# This first xgboost model did not perform well. The accuracy was only 52.38%.
# We are going to run the model again with the second set of features.


# Setting seed
set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()


# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Convert data to matrix format for XGBoost
  X_train <- as.matrix(train_data %>% select(all_of(features_2)))
  y_train <- train_data$Share
  
  X_test <- as.matrix(test_data %>% select(all_of(features_2)))
  y_test <- test_data$Share   # Actual MVP Share values
  
  # Convert to XGBoost DMatrix format
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test)
  
  # Train XGBoost model
  xgb_model <- xgboost(data = dtrain, 
                       nrounds = 100, 
                       objective = "reg:squarederror",
                       max_depth = 6, 
                       eta = 0.1, 
                       subsample = 0.8, 
                       colsample_bytree = 0.8, 
                       verbose = 0)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(xgb_model, dtest)
  
  # Compute MAE and R-Squared for this year
  mae_values <- c(mae_values, mae(y_test, test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
xgb2_average_mae <- mean(mae_values)
xgb2_average_r2 <- mean(r2_values)
xgb2_accuracy_xgb <- correct_predictions / total_years

# Printing results
print(xgb2_average_mae)
print(xgb2_average_r2)
print(xgb2_accuracy_xgb)

# This model improves with the new predictors, 69%, but it did not perform better than the random forest model.


######################## Models with new predictors after some research #################

# After some research, I found a great article by David Yoo. The data for this research came from his kaggle notebook.
# Reading his article, I decided to use the predictors that he had used for my models too.
# Below are the predictors that he used along with all the models again and their perfomance with the new predictors.


### LM with new features ###

features_3 <- c("value_over_replacement_player", "WS_per_48",
                "player_efficiency_rating", "PTS", "seed", "free_throw_attempt_rate",
                "offensive_box_plus_minus", "win_loss_ratio", "usage_percentage")

# Setting seed
set.seed(123)

# Initializing counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for leave one year out cross-validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Train Linear Regression model
  formula <- as.formula(paste(target, "~", paste(features_3, collapse = " + ")))
  model <- lm(formula, data = train_data)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Model summary
summary(model)

# Compute final metrics
lm3_average_mae <- mean(mae_values, na.rm = TRUE)
lm3_average_r2 <- mean(r2_values, na.rm = TRUE)
lm3_accuracy <- correct_predictions / total_years

# Print results
print(lm3_average_mae)
print(lm3_average_r2)
print(lm3_accuracy)

# The accuracy of the linear model with the new predictors is 64.3%, which is the same accuracy as the second model we already had.
# It seems like a linear model is not ideal.


### RF with new features ###

# Setting seed
set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Convert features into a formula format for Random Forest
  formula <- as.formula(paste("Share", "~", paste(features_3, collapse = " + ")))
  
  # Train Random Forest model
  rf_model <- randomForest(formula, data = train_data, ntree = 500, importance = TRUE)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(rf_model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
rf3_accuracy <- correct_predictions / total_years
rf3_average_mae <- mean(mae_values, na.rm = TRUE)
rf3_average_r2 <- mean(r2_values, na.rm = TRUE)

# Print results
print(rf3_average_mae)
print(rf3_average_r2)
print(rf3_accuracy)

# The new random forest model has an accuracy of 61.9%, which is much lower than our previous random forest model.


### XGBoost with new features ###

features_3 <- c("value_over_replacement_player", "WS_per_48",
                "player_efficiency_rating", "PTS", "seed", "free_throw_attempt_rate",
                "offensive_box_plus_minus", "win_loss_ratio", "usage_percentage")

# Setting seed
set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(master_table$year))
mae_values <- c()
r2_values <- c()


# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(master_table$year)) {
  
  # Split training and test data
  train_data <- master_table %>% filter(year != yr)
  test_data <- master_table %>% filter(year == yr)
  
  # Convert data to matrix format for XGBoost
  X_train <- as.matrix(train_data %>% select(all_of(features_3)))
  y_train <- train_data$Share
  
  X_test <- as.matrix(test_data %>% select(all_of(features_3)))
  y_test <- test_data$Share   # Actual MVP Share values
  
  # Convert to XGBoost DMatrix format
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test)
  
  # Train XGBoost model
  xgb_model <- xgboost(data = dtrain, 
                       nrounds = 16, 
                       objective = "reg:squarederror",
                       max_depth = 5, 
                       eta = 0.2745, 
                       subsample = 1, 
                       colsample_bytree = 1, 
                       verbose = 0)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(xgb_model, dtest)
  
  # Compute MAE and R-Squared for this year
  mae_values <- c(mae_values, mae(y_test, test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(Player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(Player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
xgb3_average_mae <- mean(mae_values)
xgb3_average_r2 <- mean(r2_values)
xgb3_accuracy_xgb <- correct_predictions / total_years

# Printing results
print(xgb3_average_mae)
print(xgb3_average_r2)
print(xgb3_accuracy_xgb)

# This is the best model we have built so far with an 83.3% accuracy. 
# This is going to be the model that we are going to use for the 2025 season prediction.


########### 2025 NBA MVP Prediction #################


# Define the structure of the data frame
nba_2025_mvp <- data.frame(
  Player = c("Nikola Jokic", "Shai Gilgeous-Alexander", "Giannis Antetokounmpo", 
             "Karl-Anthony Towns", "Jayson Tatum", "Evan Mobley", "Domantas Sabonis", 
             "Jarrett Allen", "Darius Garland", "LeBron James"),  # Player names
  value_over_replacement_player = c(7.7, 6.7, 4.1, 2.9, 3.8, 2.8, 3.5, 2.6, 2.6, 3.8),
  WS_per_48 = c(.327, .316, .212, .213, .177, .219, .206, .248, .201, .159),
  player_efficiency_rating = c(32.9, 30.8, 29.8, 24.8, 21.8, 23.2, 22.9, 22.0, 21.6, 23.5),
  PTS = c(29.2, 32.4, 30.9, 24.8, 26.6, 18.7, 19.9, 13.5, 21.3, 24.9),
  seed = c(3, 1, 5, 3, 2, 1, 10, 1, 1, 4),
  free_throw_attempt_rate = c(.312, .424, .507, .335, .315, .332, .325, .411, .232, .250),
  offensive_box_plus_minus = c(10.5, 8.7, 6.1, 4.4, 4.5, 3.4, 4.2, 3.1, 5.1, 5.4),
  win_loss_ratio = c(.644, .810, .569, .655, .712, .828, .517, .828, .828, .632),
  usage_percentage = c(29.4, 34.5, 36.2, 27.2, 30.6, 23.3, 21.4, 15.6, 27.6, 30.7),
  stringsAsFactors = FALSE  # Keeps player names as characters
)


# Convert 2025 data into a matrix for XGBoost prediction
X_2025 <- as.matrix(nba_2025_mvp %>% select(-Player))  # Exclude 'Player' column

# Convert to XGBoost DMatrix format
d2025 <- xgb.DMatrix(data = X_2025)

# Predict MVP Share for 2025 candidates
nba_2025_mvp$Predicted_Share <- predict(xgb_model, d2025)

# Sort players by predicted MVP Share
nba_2025_mvp <- nba_2025_mvp %>% arrange(desc(Predicted_Share))

# Selecting top 5 predictions
nba_2025_mvp <- nba_2025_mvp %>%
  arrange(desc(Predicted_Share)) %>%  # Sort by Predicted_Share in descending order
  slice_head(n = 5)  # Select the top 5 players

# Selecting the 2025 NBA MVP
nba_2025_mvp %>% 
  filter(Predicted_Share == max(Predicted_Share)) %>% 
  select(Player)


# SGA IS GOING TO BE THE 2025 NBA MVP!!!!









































