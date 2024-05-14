#imports
library(data.table) #required to read csv files
library(dplyr) #required to manipulate dataframes
library(ggplot2) #required to plot graphs
library(VIM)
library(base)
library(naniar)
library(corrplot)

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
  'loc',
  'mun',
  'est_d_men',
  't_loc_men',
  'cd_a',
  'ent',
  'c_res',
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
  'hrsocup',
  'ingocup',
  'ing_x_hrs',
  'tcco',
  'ma48me1sm',
  'scian',
  't_tra',
  'trans_ppal'
)

# Subset data frame with dplyr
data <- select(data, col_list)

comparison_op <- 'lte'
treshold <- 10

print_grouped_missing_percentages(data, treshold, comparison_op, FALSE)

high_missing_data <- print_grouped_missing_percentages(data, treshold, comparison_op, TRUE)

for(item in high_missing_data){
  data <- drop_column_from_dataset(df = data, item)
}

#drop columns irrelevant variables:
drop_cols <- c('t_loc_men', 'est_d_men')
for(item in drop_cols){
  data <- drop_column_from_dataset(df = data, item)
}
