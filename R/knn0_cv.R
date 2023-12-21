#' Cross-validation for KNN
#'
#' @param data input matrix
#' @param k_values user-supplied k sequence for hyper-parameter tuning
#' @param num_folds number of folds
#' @return a table of k and correspond mean accuracy
#' @export knn0_cv
#' @examples
#' iris_cv <- knn0_cv(iris[1:50,], k_values = c(3,5,7,9), num_folds = 5)
#' iris_cv
knn0_cv <- function(data, k_values, num_folds) {
  folds <- cut(seq(1, nrow(data)), breaks = num_folds, labels = FALSE)
  accuracies <- purrr::map_dbl(k_values, function(k) {
    cat("Processing k =", k, "\n")
    fold_accuracies <- purrr::map_dbl(1:num_folds, function(fold) {
      train_indices <- which(folds != fold)
      test_indices <- which(folds == fold)
      train_data <- data[train_indices, ]
      test_data <- data[test_indices, ]

      # Make predictions using knn0
      predictions <- knn0(train_data, test_data, y = 'Species', k = k)
      # Calculate accuracy
      accuracy <- sum(predictions == test_data$Species) / length(test_data$Species)
      return(accuracy)
    })
    # Average accuracy across folds
    mean_accuracy <- mean(fold_accuracies)
    return(mean_accuracy)
  })
  # Return a data frame with k values and corresponding mean accuracies
  result_df <- tibble::tibble(k = k_values, mean_accuracy = accuracies)
  return(result_df)
}
