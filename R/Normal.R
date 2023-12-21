#' @importFrom stats dnorm
# Vectorized Normal function
Normal <- function(x, mu, var) {
  sd <- sqrt(var)
  pdf <- dnorm(x, mean = mu, sd = sd)
  return(log(pdf))
}
