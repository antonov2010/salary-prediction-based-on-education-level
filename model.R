#imports
library(data.table) #required to read csv files
library(dplyr) #required to manipulate dataframes
library(ggplot2) #required to plot graphs
library(VIM) # Visualization and Imputation of Missing Values
library(base) # This package contains the basic functions which let R function as a language
library(naniar) # naniar is a package to make it easier to summarise and handle missing values in R
library(corrplot) # A graphical display of a correlation matrix, confidence interval. 
library(randomForest) # randomForest implements Breiman's random forest algorithm for classification and regression.
library(caret)  # Load caret package after installation
library(MASS)  # Load MASS package for MedAE function
library(plotly) # An R package for creating interactive web-based graphs via the open source JavaScript graphing library plotly.js.

# read csv file and load to a table data
file_path <- "./enoe_n_2022_trim4_csv (2)/ENOEN_SDEMT422.csv"
data <- fread(file_path)

# Statistical functions
c_Mode <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

c_Median <- function(x) {
  median(x, na.rm = TRUE)
}

c_Mean <- function(x) {
  mean(x, na.rm = TRUE)
}

#Calculate percentages for grouped data
percent_by_group <- function(df, col, total_count = nrow(df)) {
  df %>%
    group_by(df[, ..col]) %>%  # Group by values in 'col' (# Using column selection)
    summarise(n = n(), # Count observations (including missing values)
              pct = n / total_count * 100  # Calculate percentage relative to total count
              )
}

# Identify and report columns in a data frame that have a high percentage of missing values (above acceptable percentage or threshold).
# Identify and report columns in a data frame that are within an acceptable percentage or threshold.
print_grouped_missing_percentages <-
  function(df,
           acceptable_percentage,
           comparison_operator = 'gt',
           return_list = FALSE) {
    # Identify columns with at least one NA value
    columns_with_na <- colnames(df)[apply(df, 2, anyNA)]
    # Sort and print column names with na values
    sorted_na_columns <- sort(columns_with_na)
    # Error handling for missing arguments
    if (missing(df) |
        missing(sorted_na_columns) |
        missing(acceptable_percentage)) {
      stop("Missing required arguments: df, sorted_na_columns, and acceptable_percentage.")
    }
    
    if (comparison_operator == 'gt') {
      cat(
        'High NA Values: Printing columns with NA value rates exceeding',
        acceptable_percentage,
        "%\n"
      )
    }
    else{
      cat(
        'Acceptable NA Levels: Printing columns with NA value rates at most',
        acceptable_percentage,
        "%\n"
      )
    }
    
    # Validate comparison operator
    valid_operators <- c("gt", "lte")
    if (!comparison_operator %in% valid_operators) {
      stop("Invalid comparison_operator. Use 'gt' for greater than or 'lte' for less than.")
    }
    line_number <- 1
    my_list <- list()  # Start with an empty list
    for (col in sorted_na_columns) {
      # Calculate percentage by group
      grouped_counts_percentage <- percent_by_group(df, col)
      
      # Filter based on comparison operator
      if (comparison_operator == 'gt') {
        na_data <- grouped_counts_percentage[is.na(
          grouped_counts_percentage[[col]]
          &
            grouped_counts_percentage[['pct']] > acceptable_percentage
        ), 'pct']
      }
      else{
        na_data <- grouped_counts_percentage[is.na(
          grouped_counts_percentage[[col]]
          &
            grouped_counts_percentage[['pct']] <= acceptable_percentage
        ), 'pct']
      }
      
      if (nrow(na_data) > 0) {
        my_list <- c(my_list, col)
        if (return_list) {
          next
        }
        percent <- na_data[['pct']]
        
        # Round to one decimal place
        rounded_value <- round(percent, 1)
        
        # Format as a string with percentage sign using paste
        formatted_value <- paste(rounded_value, "%", sep = "")
        
        # Print results for problematic groups
        cat(line_number, "-", col, ":", formatted_value, "\n")
        cat(
          "Mode:",
          c_Mode(data[[col]]),
          "Median:",
          c_Median(data[[col]]),
          "Mean:",
          round(c_Mean(data[[col]]), 1),
          "\n"
        )
        line_number <- line_number + 1
      }
    }
    if (return_list) {
      return(my_list)
    }
  }

# Drop column with 100% missing data
drop_column_from_dataset <- function(df, column_name = NULL) {
  if (!is.null(column_name)) {
    df <- df %>% select(-column_name)
  }
  return(df)
}

# Print regression model metrics for performance evaluation
print_results <- function(preds, obs) {
  mae <- mean(abs(preds - obs))
  cat("Mean Absolute Error:", mae, '\n')
  medae <- median(abs(preds - obs))
  cat("Median Absolute Error:", medae, '\n')
  rmse <- sqrt(mean((preds - obs) ^ 2))
  cat("Root Mean Squared Error:", rmse, '\n')
  mse <- mean((preds - obs) ^ 2)
  cat("Mean Squared Error:", mse, '\n')
}

# List of columns to select from original data frame
col_list <- c(
  'mun',
  'cd_a',
  'ent',
  'par_c',
  'sex',
  'eda',
  'nac_dia',
  'nac_mes',
  'nac_anio',
  'l_nac_c',
  'cs_p12',
  'cs_p13_1',
  'cs_p13_2',
  'cs_p14_c',
  'cs_p15',
  'cs_p16',
  'cs_p17',
  'n_hij',
  'e_con',
  'cs_p20c_1',
  'cs_ad_mot',
  'cs_ad_des',
  'cs_nr_mot',
  'cs_nr_ori',
  'zona',
  'salario',
  'anios_esc',
  'ingocup',
  'ma48me1sm',
  't_tra'
)

# Subset data frame with dplyr
data <- select(data, col_list)

comparison_op <- 'gt'
treshold <- 10

# Get columns with high percentage of missing values
high_missing_data <- print_grouped_missing_percentages(data, treshold, comparison_op, TRUE)

# Remove columns with high percentage of missing values
for (item in high_missing_data) {
  data <- drop_column_from_dataset(df = data, item)
}

# Report and print if any column has a high percentage of missing values
print_grouped_missing_percentages(data, treshold, comparison_op, FALSE)

comparison_op <- 'lte'

# Get columns with an acceptable percentage of missing data
high_missing_data <- print_grouped_missing_percentages(data, treshold, comparison_op, TRUE)

# Print size of missing data for each reported column to identify correlated patters in missing data
for (item in high_missing_data) {
  cat(item, ':', n_miss(data[[item]]), '\n') # number of missing value for a given variable
}

#Imputation
#Based on data analisys we proceed to remove missing observations since they represent a lower percentage of the whole data

# Select rows with complete data (no missing values) in all columns
complete_rows <- complete.cases(data)  # Check for complete cases in all columns
data_clean <- data[complete_rows, ]  # Select rows based on logical vector

# Select observations where income is greater 16k since based on our data analisys
# we have determined this sub set or group could be a potential sample to train our model
subset_ingocup <- filter(data_clean, ingocup > 16000)

# Delete missing column for this group or sub set of data
subset_ingocup$ma48me1sm <- NULL

# scaled_data <- scale(subset_ingocup)

# Manual model splitting
training_prop <- 0.7  # Proportion for training data
# Select training indices from our sub set
training_indices <- sample(1:nrow(subset_ingocup), size = nrow(subset_ingocup) * training_prop)
# Based on previous selected indices select the training data
training_data <- subset_ingocup[training_indices, ]
#  This line leverages negation (-) for efficient test data selection.
testing_data <- subset_ingocup[-training_indices, ]

# Train a random forest model on the training data (no parameter tuning)
basic_model <- randomForest(
  formula = ingocup ~ .,
  data = training_data,
  ntree = 300,
  importance = TRUE,
  do.trace = TRUE
)

# Plotting the Error vs Number of Trees Graph.
plot(basic_model)

# Visualize the variable importance in random forest model.
varImpPlot(basic_model)

importance(basic_model)

# Hyperparameter tuning

# Step 1
# Grid search
set.seed(1234)
# Allows to define a set of parameters that govern how the training process is executed and evaluated.
trControl <- trainControl(
  method = "cv",
  number = 10,
  search = "grid",
  verboseIter = TRUE
)

#  Train and tune random forest model with default parameters and hyperparameters
rf_default <- train(
  ingocup ~ .,
  data = training_data,
  method = "rf",
  metric = "RMSE",
  trControl = trControl
)

# Print the results
print(rf_default)

plot(rf_default)

# Visualize the variable importance in random forest model.
varImpPlot(basic_model)

importance(basic_model)

# Get the min error index
min_index_df <- which.min(rf_default$results$MAE)
# Select the error based on the previous selected index
selected_row_df <- rf_default$results$MAE[min_index_df]

# Default parameters
print(rf_default$results$MAE)
cat('DF MAE:', selected_row_df)


# Step 2
# Define a grid of hyperparameter values for model training and tuning
tuneGrid <- expand.grid(.mtry = c(1:10))

#  Train and tune random forest model with grid of possible values for mtry
rf_mtry <- train(
  ingocup ~ .,
  data = training_data,
  method = "rf",
  metric = "RMSE",
  tuneGrid = tuneGrid,
  trControl = trControl,
  importance = TRUE,
  nodesize = 14,
  ntree = 300
)

# Print the results
print(rf_mtry)

plot(rf_mtry)

cat(rf_mtry$bestTune$mtry, "MAE:", rf_mtry$results[rf_mtry$bestTune$mtry, 'MAE'])

# Best mtry: Number of variable is randomly collected to be sampled at each split time.

# Get the min error index
min_index_mtry <- which.min(rf_mtry$results$MAE)
# Select the error based on the previous selected index
selected_row_mtry <- rf_mtry$results$MAE[min_index_df]

print(rf_mtry$results$MAE)
cat('MTRY MAE:', selected_row_mtry)

# Save the best mtry value we got after going trough a list of possible values (1:10)
best_mtry <- rf_mtry$bestTune$mtry

# Step 3
# List to hold the models for each different selected maxnode value (25 to 50)
store_maxnode <- list()
# Set the best mtry by default (previously optimized)
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(25:50)) {
  print(paste("Current max node:", maxnodes))
  set.seed(1234)  # Set random seed for reproducibility (optional)
  # Train random forest model with current maxnodes
  rf_maxnode <- train(
    ingocup ~ .,  # Formula: target variable ~ all predictors
    data = training_data,
    method = "rf",  # Random forest algorithm
    metric = "RMSE",  # Evaluation metric (Root Mean Squared Error)
    tuneGrid = tuneGrid,  # Predefined grid search configuration
    trControl = trControl,  # Training control object for cross-validation
    importance = TRUE,  # Calculate variable importance
    nodesize = 14,  # Minimum size of terminal nodes (optional)
    maxnodes = maxnodes,  # Current maxnodes value being tested
    ntree = 300  # Number of trees in the forest (previously chosen)
  )
  # Store the trained model with current maxnodes
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}

# Evaluate Model Performance Across Different Maxnodes
results_max_node <- resamples(store_maxnode)
summary_max_node <- summary(results_max_node)

# Identify Model with Minimum Mean Absolute Error (MAE)
min_index_summary_max_node <- which.min(summary_max_node$statistics$MAE[, 'Min.'])  # Replace "column_name" with your actual column
selected_row_summary_max_node <- summary_max_node$statistics$MAE[min_index_summary_max_node, ]
# Best maxnodes: Controls the maximum number of terminal nodes (also called leaves) that can be created in each tree within the forest.
# Print Best Maxnodes and Corresponding Minimum MAE
cat(
  names(min_index_summary_max_node),
  "MAE:",
  round(selected_row_summary_max_node[1], 7)
)

# Extract Best Maxnodes Value as Integer
best_max_nodes <- as.integer(names(min_index_summary_max_node))


# Step 4: Grid Search for Number of Trees (ntree)
# List to store models trained with different ntree values
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  print(paste("Current ntree:", ntree))
  # Train Random Forest Model with Current ntree
  set.seed(1234)
  rf_maxtrees <- train(
    ingocup ~ .,  # Formula: target variable ~ all predictors
    data = training_data,
    method = "rf",  # Random forest algorithm
    metric = "RMSE",  # Evaluation metric (Root Mean Squared Error)
    tuneGrid = tuneGrid,  # Predefined grid search configuration (assuming mtry is set)
    trControl = trControl,  # Training control object for cross-validation
    importance = TRUE,  # Calculate variable importance
    nodesize = 14,  # Minimum size of terminal nodes (optional)
    maxnodes = best_max_nodes,  # Use previously identified best maxnodes
    ntree = ntree  # Current ntree value being tested
  )
  # Store the Trained Model with Current ntree
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}

# Evaluate Model Performance Across Different ntree Values
results_ntrees <- resamples(store_maxtrees)  # Perform resampling (cross-validation)
summary_ntrees <- summary(results_ntrees)  # Summarize resampling results

# Identify Model with Minimum Mean Absolute Error (MAE)
min_index_summary_ntrees <- which.min(summary_ntrees$statistics$MAE[, 'Min.'])  # Replace "column_name" with your actual column
selected_row_summary_ntrees <- summary_ntrees$statistics$MAE[names(min_index_summary_ntrees), ]

# Best ntree: ntree refers to the number of trees that are grown in the entire forest.
# Print Best ntree and Corresponding Minimum MAE
cat(names(min_index_summary_ntrees),
    "MAE:",
    round(selected_row_summary_ntrees[1], 7))

# Extract Best ntree Value as Integer
best_ntree <- as.integer(names(min_index_summary_ntrees))

print(rf_default$results)
print(rf_mtry$results)
print(summary_max_node)
print(summary_ntrees)

print(rf_default$results$MAE) # Step 1
print(rf_mtry$results$MAE) # Step 2
print(summary_max_node$statistics$MAE) # Step 3
print(summary_ntrees$statistics$MAE) # Step 4

cat('DF MAE:', selected_row_df)
cat('MTRY MAE:', selected_row_mtry)
cat("MAXNODES MAE:", round(selected_row_summary_max_node[1], 7))
cat("NTREE MAE:", round(selected_row_summary_ntrees[1], 7))

# Final Model Training with Best Hyperparameters using caret train function
fit_rf <- train(
  ingocup ~ .,
  training_data,
  method = "rf",
  metric = "RMSE",
  tuneGrid = tuneGrid,
  trControl = trControl,
  importance = TRUE,
  nodesize = 14,
  ntree = best_ntree,
  maxnodes = best_max_nodes
)

# Final Model Training with randomForest function
model <- randomForest(
  formula = ingocup ~ .,  # Formula: target variable ~ all predictors
  data = training_data,  # Training data subset
  ntree = best_ntree,     # Number of trees (using best value)
  maxnodes = best_max_nodes,  # Maximum terminal nodes (using best value)
  mtry = best_mtry,        # Number of variables tried at each node (using best value)
  nodesize = 14,          # Minimum size of terminal nodes (optional)
  importance = TRUE,      # Calculate variable importance
  do.trace = 50           # Print information every 50 trees (optional)
)

plot(basic_model)
plot(model)
varImpPlot(basic_model)
varImpPlot(model)

# Make predictions using different trained models
basic_preds <- predict(basic_model, testing_data)
ct_preds <- predict(fit_rf, testing_data)
preds <- predict(model, testing_data)

# Print prediction results
print_results(basic_preds, testing_data$ingocup)
print_results(ct_preds, testing_data$ingocup)
print_results(preds, testing_data$ingocup)

# Plot results for predictions of the model created with caret::train() method
plot(testing_data$ingocup, ct_preds)
abline(a = 0, b = 1, col = "red")  # Diagonal reference line

plot(ct_preds, testing_data$ingocup - preds, main = "Residuals vs. Predicted Values")
abline(a = 0, h = 1, col = "red")  # Add a horizontal reference line at zero

hist(testing_data$ingocup)
hist(ct_preds)


plot(x=preds, y= testing_data$ingocup,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)

# create dataframe with actual and predicted values 
plot_data <- data.frame(Predicted_value = ct_preds,   
                        Observed_value = testing_data$ingocup)


# plot predicted values and actual values 
ggplot(plot_data, aes(x = Observed_value, y = Predicted_value)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "green")

# Creates an interactive graph using plotly package
ggplot_object <- ggplot(plot_data, aes(Observed_value, Predicted_value)) +
  geom_point(aes(color = "Observed"), size = 3) +
  geom_line(aes(color = "Observed")) +
  geom_point(aes(y = Predicted_value, color = "Predicted", alpha = .1), size = 5) +
  geom_line(aes(y = Predicted_value, color = "Predicted")) +
  scale_color_manual(values = c("orange", "deepskyblue3")) +
  theme_bw()

# Display plot
ggplot_object


# Display dynamic plotly graph
ggplotly(ggplot_object)
