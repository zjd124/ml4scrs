#' @import dplyr magrittr tibble
knn_prediction <- function(x, y) {
  # Convert to tibble
  x <- as_tibble(x)
  # Calculate the frequency of each class label in column 'y'
  groups <- x %>%
    pull(y) %>%
    table()
  # Find the class label with the highest frequency
  pred <- names(groups)[groups == max(groups)]
  return(pred)
}
