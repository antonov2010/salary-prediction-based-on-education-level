#imports
library(data.table) #required to read csv files
library(dplyr) #required to manipulate dataframes
library(ggplot2) #required to plot graphs
library(VIM)
library(base)
library(naniar)
library(corrplot)
library(randomForest)
library(caret)  # Load caret package after installation
library(e1071)


# read csv file and load to a table data
file_path <- "./enoe_n_2022_trim4_csv (2)/ENOEN_SDEMT422.csv"
data <- fread(file_path)

# Statistical functions
c_Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
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
    group_by(df[,..col]) %>%  # Group by values in 'col' (# Using column selection)
    summarise(
      n = n(),  # Count observations (including missing values)
      pct = n / total_count * 100  # Calculate percentage relative to total count
    )
}


print_grouped_missing_percentages <- 
  function(df, acceptable_percentage, comparison_operator = 'gt', return_list=FALSE) {
    # Identify columns with at least one NA value
    columns_with_na <- colnames(df)[ apply(df, 2, anyNA)]
    # Sort and print column names with na values
    sorted_na_columns <- sort(columns_with_na)
    # Error handling for missing arguments
    if (missing(df) | missing(sorted_na_columns) | missing(acceptable_percentage)) {
      stop("Missing required arguments: df, sorted_na_columns, and acceptable_percentage.")
    }
    
    if(comparison_operator == 'gt'){
      cat('High NA Values: Printing columns with NA value rates exceeding', acceptable_percentage, "%\n")
    }
    else{
      cat('Acceptable NA Levels: Printing columns with NA value rates at most', acceptable_percentage, "%\n")
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
      if(comparison_operator == 'gt'){
        na_data <- grouped_counts_percentage[is.na(grouped_counts_percentage[[col]] 
                                                   & grouped_counts_percentage[['pct']] > acceptable_percentage), 'pct']
      }
      else{
        na_data <- grouped_counts_percentage[is.na(grouped_counts_percentage[[col]] 
                                                   & grouped_counts_percentage[['pct']] <= acceptable_percentage), 'pct']
      }
      
      if (nrow(na_data) > 0) {
        my_list <- c(my_list, col)
        if(return_list){
          next
        }
        percent <- na_data[['pct']]
        
        # Round to one decimal place
        rounded_value <- round(percent, 1)
        
        # Format as a string with percentage sign using paste
        formatted_value <- paste(rounded_value, "%", sep = "")
        
        # Print results for problematic groups
        cat( line_number, "-", col, ":", formatted_value,"\n")
        cat("Mode:", c_Mode(data[[col]]), "Median:", c_Median(data[[col]]), "Mean:", round(c_Mean(data[[col]]),1), "\n")
        line_number <- line_number + 1
      }
    }
    if(return_list){
      return(my_list)
    }
  }

#Drop column with 100% missing data
drop_column_from_dataset <- function(df, column_name = NULL){
  if(!is.null(column_name)){
    df <- df %>% select(-column_name)
  }
  return(df)
}

# List of columns to select
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

high_missing_data <- print_grouped_missing_percentages(data, treshold, comparison_op, TRUE)

for(item in high_missing_data){
  data <- drop_column_from_dataset(df = data, item)
} 

print_grouped_missing_percentages(data, treshold, comparison_op, FALSE)

comparison_op <- 'lte'
high_missing_data <- print_grouped_missing_percentages(data, treshold, comparison_op, TRUE)
for(item in high_missing_data){
  cat(item,':', n_miss(data[[item]]),'\n') # number of missing value for a given variable
}

# Select rows with complete data (no missing values) in all columns
complete_rows <- complete.cases(data)  # Check for complete cases in all columns
data_clean <- data[complete_rows, ]  # Select rows based on logical vector

# subset_ingocup <- filter(data_clean, ingocup > 2000 & ingocup <= 14000)
subset_ingocup <- filter(data_clean, ingocup > 16000)

summary(subset_ingocup)

# Delete missing column
subset_ingocup$ma48me1sm <- NULL

scaled_data <- scale(subset_ingocup)

# Manual model
training_prop <- 0.7  # Proportion for training data
training_indices <- sample(1:nrow(scaled_data), size = nrow(scaled_data) * training_prop)
training_data <- scaled_data[training_indices, ]
testing_data <- scaled_data[-training_indices, ]

glimpse(training_data)
glimpse(testing_data)


glimpse(training_data)
glimpse(testing_data)

model <- randomForest(formula = ingocup ~ ., data = training_data, ntree = 300, importance = TRUE, do.trace = TRUE)

?randomForest

plot(model)
varImpPlot(model)

# Step 1
# Grid search
set.seed(1234)
# Run the model
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid",
                          verboseIter = TRUE)

rf_default <- train(ingocup~.,
                    data = training_data,
                    method = "rf",
                    metric = "RMSE",
                    trControl = trControl)
# Print the results
print(rf_default)

min_index_df <- which.min(rf_default$results$MAE)
selected_row_df <- rf_default$results$MAE[min_index_df]

# Default parameters
print(rf_default$results$MAE)
cat('DF MAE:',selected_row_df)


# Step 2
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(ingocup~.,
                 data = training_data,
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)

cat(rf_mtry$bestTune$mtry, "MAE:", rf_mtry$results[rf_mtry$bestTune$mtry,'MAE'])

# Best mtry: Number of variable is randomly collected to be sampled at each split time.
min_index_mtry <- which.min(rf_mtry$results$MAE)
selected_row_mtry <- rf_mtry$results$MAE[min_index_df]

print(rf_mtry$results$MAE)
cat('MTRY MAE:',selected_row_mtry)

best_mtry <- rf_mtry$bestTune$mtry 

# Step 3
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)

for (maxnodes in c(25: 50)) {
  print(paste("Current max node:", maxnodes))
  set.seed(1234)
  rf_maxnode <- train(ingocup~.,
                      data = training_data,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}

results_max_node <- resamples(store_maxnode)
summary_max_node <- summary(results_max_node)

print(selected_row_summary_max_node)

min_index_summary_max_node <- which.min(summary_max_node$statistics$MAE[,'Min.'])  # Replace "column_name" with your actual column
selected_row_summary_max_node <- summary_max_node$statistics$MAE[min_index_summary_max_node, ]
# Best maxnodes: Controls the maximum number of terminal nodes (also called leaves) that can be created in each tree within the forest.
cat(names(min_index_summary_max_node), "MAE:", round(selected_row_summary_max_node[1],7))

best_max_nodes <- as.integer(names(min_index_summary_max_node))

# Step 4
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  print(paste("Current ntree:", ntree))
  set.seed(5678)
  rf_maxtrees <- train(ingocup~.,
                       data = training_data,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = best_max_nodes,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}

results_ntrees <- resamples(store_maxtrees)
summary_ntrees <- summary(results_ntrees)

print(summary_ntrees)

min_index_summary_ntrees <- which.min(summary_ntrees$statistics$MAE[,'Min.'])  # Replace "column_name" with your actual column
selected_row_summary_ntrees <- summary_ntrees$statistics$MAE[names(min_index_summary_ntrees),]

# Best ntree: ntree refers to the number of trees that are grown in the entire forest.
cat(names(min_index_summary_ntrees), "MAE:", round(selected_row_summary_ntrees[1],7))

best_ntree <- as.integer(names(min_index_summary_ntrees))

print(rf_default$results)
print(rf_mtry$results)
print(summary_max_node)
print(summary_ntrees)

print(rf_default$results$MAE) # Step 1
print(rf_mtry$results$MAE) # Step 2
print(summary_max_node$statistics$MAE) # Step 3
print(summary_ntrees$statistics$MAE) # Step 4

cat('DF MAE:',selected_row_df)
cat('MTRY MAE:',selected_row_mtry)
cat("MAXNODES MAE:", round(selected_row_summary_max_node[1],7))
cat("NTREE MAE:", round(selected_row_summary_ntrees[1],7))

#Final model with the best parameters
fit_rf <- train(ingocup~.,
                training_data,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = best_ntree,
                maxnodes = best_max_nodes)

prediction <-predict(model, testing_data[,'ingocup'])

print(typeof(testing_data))

# Since we're working with a continuous variable confusion matrix is not appropriate way to evaluate the model performance
confusionMatrix(prediction, testing_data[,'ingocup'])

?randomForest


model <- randomForest(formula = ingocup ~ .,
                      data = training_data,
                      ntree = best_ntree,
                      maxnodes = best_max_nodes,
                      mtry = best_mtry,
                      nodesize = 14,
                      importance = TRUE, do.trace = 50)

varImpPlot(model)

typeof(fit_rf)

importance <- varImp.train(fit_rf)

importance <- varImp.train(fit_rf)

plot(model)

mae <- mean(abs(prediction - testing_data[,'ingocup']))
cat("Mean Absolute Error:", mae)

library(caret)

mae <- mae(prediction, testing_data[,'ingocup'])
cat("Mean Absolute Error (using metrics package):", mae)

rmse <- sqrt(mean((prediction - testing_data[,'ingocup'])^2))
cat("Root Mean Squared Error:", rmse)

mse <- mean((prediction - testing_data[,'ingocup'])^2)
cat("Mean Squared Error:", mse)

library(caret)

# Assuming you have predicted values in 'predicted_values'
# and actual values in 'testing_data$target'

r_squared <- Rsq(prediction, testing_data[,'ingocup'])
cat("R-squared (using caret):", r_squared)


ggplot(testing_data, aes(x = 'ingocup')) + 
  geom_histogram(binwidth = .05) + 
  facet_wrap(prediction) + 
  xlab("Probability of Class #1")
