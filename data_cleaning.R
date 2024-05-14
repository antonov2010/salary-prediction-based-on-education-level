res<-summary(aggr(data, sortVar=TRUE))$combinations
head(res[rev(order(res[,2])),])

print(res)

matrixplot(data, sortby = 2)

pct_miss(data) # percentage of missing value in the data.

n_miss(data) # number of missing values in the 

n_complete(data) # without missing value

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
complete_rows <- complete.cases(data[, ..omit_cols])
data <- data[complete_rows, ]


bind_shadow(data)


res <- cor(data)
round(res, 2)

library(corrplot)
corrplot(res, method="number", type = 'upper')

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
chart.Correlation(data, histogram=TRUE, pch=19)

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
