#' k-Nearest Neighbour Classification
#'
#' @param train matrix or data frame of training set cases
#' @param test matrix or data frame of test set cases
#' @param y factor of true classifications of training set
#' @param k number of neighbours considerd.a specified number
#' @param func optional parameters for distance function
#' @param p Minkowski power
#' @return a single prediction of y
#' @export knn0
#' @examples
#' train = iris[1:120,]
#' test = iris[121:nrow(iris),]
#' knn0(train, test, 'Species', k = 5)
knn0 <- function(train, test, y, k, func = euclidean_distance, p = NULL) {
  # start predictions
  predictions = c()
  y_ind = which(colnames(test) == y)

  # For each observation, we obtain the prediction
  for (i in 1:nrow(test)) {
    neighbors = nearest_neighbors(train[,-y_ind],
                                  test[i,-y_ind], k, FUN = func)
    pred <- knn_prediction(train[neighbors[[1]], ], y)

    # If more than 1 prediction, make prediction with 1 more k
    if (length(pred) > 1) {
      pred <- knn0(train, test[i,], y, k = k + 1,
                   func = func, p = p)
    }
    predictions[i] = pred
  }
  return(predictions)
}
