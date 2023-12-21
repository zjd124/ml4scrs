#' make predictions form a 'train_naive_bayes' object
#'
#' @param model fitted 'train_naive_bayes' object
#' @param new_data matrix of new values for x at which predictions are to be made
#' @importFrom stats na.omit
#' @return a data frame containing the predicted class labels
#' @export predict_naive_bayes
#' @examples
#' train = iris[1:120,]
#' model <- train_naive_bayes(train, class_column = "Species")
#' predict_naive_bayes(model, new_data = iris[1:50,])
predict_naive_bayes <- function(model, new_data) {
  classes <- model$classes
  means <- model$means
  variances <- model$variances
  # Ensure column names match between training and testing datasets
  new_data <- new_data[, intersect(names(new_data), names(means[[1]]))]
  predictions <- character(length = nrow(new_data))
  for (i in seq_along(new_data[, 1])) {
    instance <- new_data[i, , drop = FALSE]
    class_likelihood <- sapply(classes, function(cls) {
      prior <- log(sum(model$classes == cls) / length(model$classes))
      likelihoods <- sapply(seq_along(instance), function(col) {
        data <- instance[[col]]
        mean_val <- means[[cls]][col]
        variance_val <- variances[[cls]][col]
        # Use Normal function for log likelihood calculation
        log_pdf <- Normal(data, mean_val, variance_val)
        # Sum up log PDFs
        sum(log_pdf, na.rm = TRUE)
      })
      sum(likelihoods, na.rm = TRUE) + prior
    })
    if (any(is.na(class_likelihood))) {
      # Handle missing values by assigning a placeholder value (e.g., NA)
      predictions[i] <- NA
    } else {
      max_class <- classes[which.max(class_likelihood)]
      predictions[i] <- as.character(max_class)  # Convert to character
    }
  }
  # Filter out instances with missing predictions
  final_predictions <- data.frame(value = na.omit(predictions), row.names = NULL)

  return(final_predictions)
}
