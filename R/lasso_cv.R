#' Cross-validation for lasso_fit
#'
#' @param x input matrix as in lasso_fit
#' @param y response variable as in lasso_fit
#' @param lambda user-supplied lambda sequence for hyper-parameter tuning
#' @param nfolds number of folds - default is 10. Smallest value allowable is 3
#' @importFrom stats var
#' @return a object containing call, lambda, beta, best beta, and min mse
#' @export lasso_cv
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = x[,1] + 2*x[,5]+ 6*x[,10] +rnorm(100)
#' lambda_values <- seq(0.1, 0.5, by = 0.1)
#' fit <- lasso_cv(x, y, lambda = lambda_values, nfolds = 10)
#' fit$lambda.best
lasso_cv <- function(x, y, lambda = NULL, nfolds = 10) {
  ### prepare and check for all arguments
  if (!is.null(lambda) && length(lambda) < 2)stop("Need more than one value of lambda for lasso_cv")
  if (any(lambda <= 0))stop("Lambda values should be positive.")
  if (nfolds < 3)stop("nfolds must be bigger than 3; nfolds=10 recommended")
  N = nrow(x)
  y = drop(y)

  mse_values <- numeric(length(lambda))

  for (i in seq_along(lambda)) {
    current_lambda <- lambda[i]

    # Perform k-fold cross-validation
    k <- sample(1:nfolds, N, replace = TRUE)

    mse_fold <- numeric(nfolds)
    for (j in seq(nfolds)) {
      x_train <- x[k != j, ]
      y_train <- y[k != j]
      x_val <- x[k == j, ]
      y_val <- y[k == j]

      # Check for zero variance in true values
      if (var(y_train) == 0) {
        mse_fold[j] <- 0  # Set MSE to zero in this case
      } else {

        # fit Lasso Regression model on train set
        model <- lasso_fit(x_train, y_train, lambda = current_lambda)

        # make predictions on validation set
        y_pred <- lasso_predict(model, x_val)

        # Check for NaN or Inf values in predictions
        if (any(is.nan(y_pred)) || any(is.infinite(y_pred))) {
          mse_fold[j] <- 0  # Set MSE to zero in this case
        } else {
          # Calculate mean squared error
          mse_fold[j] <- mean((y_pred - y_val)^2)
        }
      }
    }

    # Average MSE over k
    mse_values[i] <- mean(mse_fold)
  }

  # Find the lambda with the lowest MSE
  lambda.best <- lambda[which.min(mse_values)]
  model <- lasso_fit(x, y, lambda = lambda.best)

  ### create a list to store results
  result <- list()
  result$call <- match.call()
  result$lambda <- lambda
  result$beta <- model$beta
  result$lambda.best <- lambda.best
  result$mse.min <- min(mse_values)

  return(result)
}
