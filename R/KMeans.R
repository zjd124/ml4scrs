#' K-Means Clustering
#'
#' @param X numeric matrix of data, or an object that can be coerced to such a matrix
#' @param n_clusters number of clusters
#' @param max_iterations number of iterations
#' @return list containing centers and labels
#' @export KMeans
#' @examples
#' X <- iris[, 1:4]
#' result <- KMeans(X, n_clusters = 3)
#' centers <- result$centers
#' labels <- result$labels
KMeans <- function(X, n_clusters, max_iterations = 100) {
  # 1. Randomly choose clusters
  i <- sample(1:nrow(X), n_clusters)
  centers <- X[i, , drop = FALSE]
  for (iteration in 1:max_iterations) {

    # 2. Assign labels based on closest center
    labels <- apply(X, 1,
                    function(x)
                      which.min(colSums((t(centers) - x)^2)))

    # 3. Find new centers from means of points
    new_centers <- t(sapply(1:n_clusters,
                            function(i)
                              colMeans(X[labels == i, , drop = FALSE])))

    # 4. Check for convergence
    if (all(centers == new_centers, na.rm = TRUE)) {
      break
    }
    centers <- new_centers
  }
  return(list(centers = centers, labels = labels))
}
