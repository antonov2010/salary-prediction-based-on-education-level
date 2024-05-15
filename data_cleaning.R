res<-summary(aggr(data_clean, sortVar=TRUE))$combinations
head(res[rev(order(res[,2])),])

print(res)

matrixplot(data_clean, sortby = 2)

pct_miss(data) # percentage of missing value in the data.

n_miss(data) # number of missing values in the 

n_complete(scaled_data) # without missing value

for(item in high_missing_data){
  cat(item,':', n_miss(data[[item]]),'\n') # number of missing value for a given variable
}

# Drop rows with missing values in columns 'x' and 'y'
omit_cols <- c(
  'eda',
  'l_nac_c',
  'mun',
  'nac_anio',
  'nac_dia',
  'nac_mes',
  'par_c',
  'sex')

# Drop observations from a data frame based on missing values in specific columns
# complete_rows <- complete.cases(data[, ..omit_cols])
# data <- data[complete_rows, ]

# Select rows with complete data (no missing values) in all columns
complete_rows <- complete.cases(data)  # Check for complete cases in all columns
data_clean <- data[complete_rows, ]  # Select rows based on logical vector

# Count zeros in each column
zero_counts <- apply(data_clean == 0, 2, sum)  # Check for values equal to 0, sum by column

# Print the counts with column names
colnames(zero_counts) <- colnames(data_clean)
print("Number of zeros in each column:")
print(zero_counts)

count_income_gt_0 <- sum(data_clean$ingocup > 0 & data_clean$ingocup < 2000)

count_income_gt_70k <- sum(data_clean$ingocup > 2000)

print(sum(data_clean$ingocup == 0))
print(sum(data_clean$ingocup > 0 & data_clean$ingocup <= 2000))
print(sum(data_clean$ingocup > 2000 & data_clean$ingocup <= 14000))
print(sum(data_clean$ingocup > 14000 & data_clean$ingocup <= 16000))
print(sum(data_clean$ingocup > 16000 & data_clean$ingocup <= 18000))
print(sum(data_clean$ingocup > 18000 & data_clean$ingocup <= 20000))
print(sum(data_clean$ingocup > 20000 & data_clean$ingocup <= 70000))
print(sum(data_clean$ingocup > 16000))

subset_ingocup <- filter(data_clean, ingocup > 2000 & ingocup <= 14000)

boxplot(data_clean$ingocup, main = "Distribution of Data", ylab = "Value")

bind_shadow(data_clean)


res <- cor(data_clean)
round(res, 2)

library(corrplot)
corrplot(res, method="number")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(res, method="color", col=col(200),  
         type="upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(res, histogram=TRUE, pch=19)

missing_rows <- which(apply(data, 1, is.na))  # Check for missing in each row

data_missing <- data[missing_rows, ]  # Select rows based on index

# Select rows with missing sex (regardless of other variables)
missing_subset <- which(is.na(data$cs_p12))
data_missing <- data[missing_subset, ]

vis_miss(data, sort_miss = TRUE) 

missing_data <- colSums(is.na(data))

print(missing_data['loc'])

col_names <- str(missing_data)

View(missing_data)
for(item in missing_data){
  print(item)
}

# Option 1: Using sapply and is.na (efficient for large datasets)
missing_cols <- sapply(data, function(x) any(is.na(x)))  # Check for any NA in each column

# Get column names with missing values
cols_with_missing <- colnames(data)[missing_cols]

# Print the column names
print(cols_with_missing)
