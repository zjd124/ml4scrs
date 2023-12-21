#' Cross-validation for Kmeans
#'
#' @param X input matrix
#' @param k number of centroids
#' @param n_clusters_values user-supplied number of clusters sequence for
#' hyper-parameter tuning
#' @return best number of clustering
#' @export kmeans_cv
#' @examples
#' X <- iris[, 1:4]
#' n_clusters_values <- seq(2, 6)
#' kmeans_cv(X, k = 5, n_clusters_values)
kmeans_cv <- function(X, k, n_clusters_values) {
  # Split data into k folds
  folds <- cut(seq(1, nrow(X)), breaks = k, labels = FALSE)
  # Initialize a vector to store performance metrics
  performance_metrics <- vector("list", length = length(n_clusters_values))
  # Perform cross-validation for each value of n_clusters
  for (i in seq_along(n_clusters_values)) {
    n_clusters <- n_clusters_values[i]
    # Perform k-fold cross-validation
    for (fold in 1:k) {
      # Create training and testing sets
      test_indices <- which(folds == fold)
      train_data <- X[-test_indices, ]
      test_data <- X[test_indices, ]

      # Train KMeans model on training data
      result <- KMeans(train_data, n_clusters = n_clusters, max_iterations = 100)
      # Calculate the within-cluster sum of squares to evaluate the model on testing data
      wss <- sum(apply(result$centers, 1,
                       function(center) sum((train_data - center)^2)))
      # Store the negative WSS since we want to minimize it
      performance_metrics[[i]] <- c(performance_metrics[[i]], -wss)
    }
  }
  # Identify the value of n_clusters that gives the best performance
  best_n_clusters <- n_clusters_values[which.max(sapply(performance_metrics, mean))]

  return(best_n_clusters)
}
