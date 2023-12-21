#' Naive Bayes Classifier
#'
#' @param train_data a matrix or data frame of categorical and numeric variables
#' @param class_column a specified column representing class labels
#' @importFrom stats var
#' @return a list containing the unique classes, means, and variances
#' @export train_naive_bayes
#' @examples
#' train = iris[1:120,]
#' model <- train_naive_bayes(train, class_column = "Species")
train_naive_bayes <- function(train_data, class_column) {
  classes <- unique(train_data[[class_column]])
  means <- lapply(classes,
                  function(cls)
                    colMeans(train_data[train_data[[class_column]] == cls,
                                        -which(names(train_data) == class_column), drop = FALSE]))
  variances <- lapply(classes,
                      function(cls)
                        sapply(train_data[train_data[[class_column]] == cls,
                                          -which(names(train_data) == class_column), drop = FALSE], var, na.rm = TRUE))
  return(list(classes = classes, means = means, variances = variances))
}
