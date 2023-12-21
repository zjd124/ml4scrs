#' make predictions from a "lasso_fit" object
#'
#' @param model fitted "lasso_fit" model object
#' @param newdata matrix of new values for x at which predictions are to be made
#' @param lambda value(s) of the regularization parameter lambda at which
#' predictions are required.
#' @return an array of predicted value
#' @export lasso_predict
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = x[,1] + 2*x[,5]+ 6*x[,10] +rnorm(100)
#' fit <- lasso_fit(x,y,lambda = 0.01)
#' lasso_predict(fit, newdata = x[1:10, ])
lasso_predict <- function(model, newdata, lambda = NULL) {
  ### prepare and check for all arguments
  if(missing(model))stop("need model for prediction'")
  if (!inherits(model, "list") || !("beta" %in% names(model))) {
    stop("Invalid 'model'. Must be the result of lasso_fit.")
  }
  beta <- model$beta

  #check for newdata
  if(missing(newdata))stop("need newdata for prediction'")
  x <- newdata
  x <- cbind(1, x)
  p = dim(beta)[1]
  if(ncol(x) != p)stop(paste0("The number of variables in newdata must be ",p-1))

  # Check for the presence of lambda argument
  if (is.null(lambda)) {
    if (!("lambda" %in% names(model))) {
      stop("Lambda values not available in the model. Provide a specific lambda or include lambda values during model fitting.")
    }
    lambda_value <- model$lambda
  } else if (lambda == "lambda.best") {
    if (!("lambda.best" %in% names(model))) {
      stop("Cross-validation results not available in the model. Set lambda or run cross-validation.")
    }
    lambda_value <- model$lambda.best
  } else {
    if (!any(model$lambda == lambda))stop("The specified lambda is not available in the model.")
    lambda_value <- lambda
  }

  #make prediction
  return(x %*% beta[, which(model$lambda == lambda_value)])
}
