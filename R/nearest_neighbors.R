#' @import dplyr magrittr tibble
nearest_neighbors <- function(x, obs, k, FUN, p = NULL) {
  # Check the number of observations is the same
  if (ncol(x) != ncol(obs)) {
    stop('Data must have the same number of variables')
  }
  # Calculate distance, considering p for Minkowski
  dist <- apply(x, 1, function(row) {
    if (is.null(p)) {
      FUN(row, obs)
    } else {
      FUN(row, obs, p)
    }
  })
  # Find closest neighbors
  neighbors_info <- tibble(distances = dist, index = seq_along(dist))
  neighbors_info <- neighbors_info %>% arrange(distances) %>% slice_head(n = k)

  # Extract indices and distances
  neighbor_ind <- neighbors_info$index
  distances <- neighbors_info$distances

  ret <- list(neighbor_ind, distances)
  return(ret)
}
