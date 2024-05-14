
scaled_data <- scale(subset_ingocup)

features <- scaled_data[, names(scaled_data) != "ingocup"]

outcome <- scaled_data[,'ingocup']

scaled_data[,'ingocup']

# Assuming your data frame is named "data"
set.seed(123)  # For reproducibility (set the same seed for each run)
# Splitting ratio (80% training, 20% testing)
ctrl <- trainControl(method = "train_test", p = 0.7)
split_data <- split(scaled_data, ctrl)  # Splitting using caret

?trainControl

?train

# Accessing training and testing data sets
training_data <- split_data$training
testing_data <- split_data$testing

str(scaled_data)

training_prop <- 0.7  # Proportion for training data
training_indices <- sample(1:nrow(scaled_data), size = nrow(scaled_data) * training_prop)
training_data <- scaled_data[training_indices, ]
testing_data <- scaled_data[-training_indices, ]

model <- randomForest(formula = ingocup ~ ., data = training_data, ntree = 10, importance = TRUE, do.trace = TRUE)

set.seed(1234)
# Run the model
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
# data_clean$ingocup = factor(data_clean$ingocup)
rf_default <- train(ingocup~.,
                    data = scaled_data,
                    method = "rf",
                    metric = "RMSE",
                    trControl = trControl)
# Print the results
print(rf_default)

?train
?trainControl
