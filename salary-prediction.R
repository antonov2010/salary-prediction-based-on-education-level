#required to read csv files
library(data.table)
#required to plot graphs
library(ggplot2)
#required to manipulate dataframes
library(dplyr)
#for data manipulation
library(tidyr)

# read csv file and load to a table data
file_path <- "./enoe_n_2022_trim4_csv (2)/ENOEN_SDEMT422.csv"
data <- fread(file_path)


# print data type of data var
print(typeof(data))

# Get records quantity
total_records <- nrow(data)

# Identify columns with at least one NA value
columns_with_na <- colnames(data)[ apply(data, 2, anyNA)]

print(columns_with_na[2])
# Sort and print column names with na values
sorted_na_columns <- sort(columns_with_na)

print(typeof(columns_with_na))

# Loop through unique values in the grouping column
for (col in sorted_na_columns) {
  cat("Column:", col, "\n")
}

print(sorted_na_columns)

# Write the variable with columns with at least one missing value to a CSV file
write.csv(sorted_na_columns, "columns_with_missing_values.csv", row.names = FALSE)

# Calculate mode for categorical data
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

# Calculate median (both continuous and categorical data)
median_value <- c_Median(data$cs_ad_des, na.rm = TRUE)
mode_value = c_Mode(data$cs_ad_des)

for (col in sorted_na_columns) {
  cat(col, "->","Median:", c_Median(data[[col]]),"Mode:", c_Mode(data[[col]]),"Mean:", c_Mean(data[[col]]), "\n")
}

?median

# Print the results
cat("Median:", median_value)
print(mode_value)

#count by a given value
column_value <- sum(data$cs_ad_des == 9, na.rm = TRUE)

print(column_value)

# dplyr: Group by values in 'my_column', and count occurrences
grouped_counts <- data %>%
  group_by(cs_ad_des) %>%  # Group by values in 'my_column'
  count()  # Count occurrences of each group

# Print the results
print(grouped_counts)

# Function to group data by column values and count occurrences, including NA
group_and_count_na <- function(df, column_name) {
  df %>%
    #filter(!is.na(!!column_name)) %>%  # Filter out rows with NA using !! for non-standard evaluation
    #group_by(!!column_name) %>%  # Group by values in 'column_name' using !! for non-standard evaluation
    #filter(!is.na({{column_name}})) %>%
    group_by(df[[column_name]]) %>%
    count()
}

grouped_counts <- group_and_count_na(data, "cs_ad_des")
print(grouped_counts)

for (col in sorted_na_columns) {
  cat("Column:", col, "\n")
  print(group_and_count_na(data, col))
  cat("\n")
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

#function to print groups percentages based on a condition filter, to identify missing data rates
print_grouped_percentages_for_columnas_with_na <- function(acceptable_percentage, comparison_operator = 'gt'){
  for (col in sorted_na_columns) {
    grouped_counts_percentage = percent_by_group(data, col)
    if(comparison_operator == 'gt'){
      na_data <- grouped_counts_percentage[is.na(grouped_counts_percentage[[col]] & grouped_counts_percentage[['pct']] > acceptable_percentage), 'pct']
    }
    else{
      na_data <- grouped_counts_percentage[is.na(grouped_counts_percentage[[col]] & grouped_counts_percentage[['pct']] < acceptable_percentage), 'pct']
    }
    if (nrow(na_data) > 0) {
      cat(col, ": ", na_data[['pct']],"\n")
    }
  }
}

print_grouped_missing_percentages <- 
  function(df, sorted_na_columns, acceptable_percentage, comparison_operator = 'gt') {
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
        cs_ad_mot <- na_data[['pct']]
        
        # Round to one decimal place
        rounded_value <- round(cs_ad_mot, 1)
        
        # Format as a string with percentage sign using paste
        formatted_value <- paste(rounded_value, "%", sep = "")
        
        # Print results for problematic groups
        cat( line_number, "-", col, ":", formatted_value,"\n")
        cat("Mode:", c_Mode(data[[col]]), "Median:", c_Median(data[[col]]), "Mean:", c_Mean(data[[col]]), "\n")
        line_number <- line_number + 1
      }
    }
  }

#print_grouped_percentages_for_columnas_with_na(50, 'lt')

print_grouped_missing_percentages(data, sorted_na_columns, 50, 'lte')

