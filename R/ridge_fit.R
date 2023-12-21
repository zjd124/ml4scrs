#' fit a linear regression model with ridge regularization
#'
#' @param x input matrix, of dimension nobs x nvars
#' @param y response variable
#' @param lambda regularization parameter. can be a single number or a sequence
#' of lambda values
#' @import Matrix
#' @return a object containing call, lambda, and beta
#' @export ridge_fit
#' @examples
#' x = matrix(rnorm(100 * 20), 100, 20)
#' y = x[,1] + 2*x[,5]+ 6*x[,10] +rnorm(100)
#' fit <- ridge_fit(x, y, lambda = c(0.01, 0.02))
#' fit$beta
ridge_fit <- function(x, y, lambda = NULL){
  ### prepare and check for all arguments
  #check for x
  np <- dim(x)
  if(is.null(np)|(np[2]<=1))stop("x should be a matrix with 2 or more columns")
  nobs <- as.integer(np[1])
  nvars <- as.integer(np[2])
  if(any(is.na(x)))stop("x has missing values")
  if(is.null(colnames(x)))colnames(x) <- paste0("V", seq_len(ncol(x))) #rename colname of x if null
  x <- cbind(Intercept = 1, x)
  vnames <- colnames(x)

  #check for y
  y <- drop(y)
  dimy <- dim(y)
  nrowy <- ifelse(is.null(dimy),length(y),dimy[1])
  if(nrowy!=nobs)stop(paste("number of observations in y (",nrowy,") not equal to the number of rows of x (",nobs,")",sep=""))
  if(any(is.na(y)))stop("y has missing values")

  #check for lambda
  if(is.null(lambda))stop("lambda should not be none")
  if(any(lambda<0))stop("lambdas should be non-negative")

  ### create theta to store the coefficients of each predictor variables
  betas <- matrix(0, nrow = nvars+1, ncol = length(lambda))
  rownames(betas) <- vnames
  colnames(betas)<- paste0("lambda", seq_len(ncol(betas)))

  # fit the model for each lambda
  for (i in seq_along(lambda)) {
    current_lambda <- lambda[i]
    # Ridge regression solution using matrix operations
    beta <- solve(t(x) %*% x + current_lambda * diag(nvars + 1)) %*% t(x) %*% y
    betas[, i] <- beta
  }

  ### create a list to store results
  result <- list()
  result$call <- match.call()
  result$lambda <- lambda
  result$beta <- Matrix(betas, sparse = TRUE)
  return(result)
}
