% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lasso_cv.R
\name{lasso_cv}
\alias{lasso_cv}
\title{Cross-validation for lasso_fit}
\usage{
lasso_cv(x, y, lambda = NULL, nfolds = 10)
}
\arguments{
\item{x}{input matrix as in lasso_fit}

\item{y}{response variable as in lasso_fit}

\item{lambda}{user-supplied lambda sequence for hyper-parameter tuning}

\item{nfolds}{number of folds - default is 10. Smallest value allowable is 3}
}
\value{
a object containing call, lambda, beta, best beta, and min mse
}
\description{
Cross-validation for lasso_fit
}
\examples{
x = matrix(rnorm(100 * 20), 100, 20)
y = x[,1] + 2*x[,5]+ 6*x[,10] +rnorm(100)
lambda_values <- seq(0.1, 0.5, by = 0.1)
fit <- lasso_cv(x, y, lambda = lambda_values, nfolds = 10)
fit$lambda.best
}
