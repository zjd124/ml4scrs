#' Cross-validation for train_naive_bayes
#'
#' @param data a matrix or data frame of categorical and numeric variables.
#' @param class_column a specified column representing class labels
#' @param k number of folds - default is 5
#'
#' @return average Cross-Validation Accuracy
#' @export naive_bayes_cv
#' @examples
#' naive_bayes_cv(iris, class_column = "Species")
naive_bayes_cv <- function(data, class_column, k = 5) {
  # Create indices for k-fold cross-validation
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  # Create variables to store results
  accuracy_scores <- numeric(k)
  # Perform k-fold cross-validation
  for (i in 1:k) {
    cat("Processing fold", i, "\n")
    # Split the data into training and testing sets
    test_indices <- which(folds == i, arr.ind = TRUE)
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    # Train Naive Bayes model
    model <- train_naive_bayes(train_data, class_column = class_column)
    # Make predictions on the test set
    predictions <- predict_naive_bayes(model, new_data = test_data)
    # Evaluate accuracy
    correct_predictions <- sum(predictions$value == test_data[[class_column]])
    accuracy_scores[i] <- correct_predictions / nrow(test_data)
  }
  # Calculate average accuracy
  average_accuracy <- mean(accuracy_scores)
  return(average_accuracy)
}
