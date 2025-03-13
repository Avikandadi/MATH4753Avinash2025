#' Plot Normal Distribution Curve with Shaded Probability Area
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The x-value where the area is shaded
#'
#' @returns A graph with the curve of the normal distribution and the shaded area
#' @export
#'
#' @examples
#' myncurve(mu = 10, sigma = 2, a = 12)
myncurve <- function(mu, sigma, a) {
  curve(dnorm(x, mean=mu, sd=sigma),
        xlim = c(mu - 3*sigma, mu + 3*sigma),
        ylim = c(0, dnorm(mu, mean=mu, sd=sigma) * 1.1),
        col="blue", lwd=2,
        ylab="Density", xlab="X",
        main=paste("Normal Curve N(", mu, ",", sigma, "^2), P(X ≤", a, ")"))

  x_vals <- seq(mu - 3*sigma, a, length=100)
  y_vals <- dnorm(x_vals, mean=mu, sd=sigma)

  polygon(c(x_vals, rev(x_vals)), c(y_vals, rep(0, length(x_vals))), col="lightblue")

  prob <- pnorm(a, mean=mu, sd=sigma)

  text(mu, dnorm(mu, mean=mu, sd=sigma)/2, paste("P(X ≤", a, ") =", round(prob, 4)), col="red", cex=1.2)

  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}


