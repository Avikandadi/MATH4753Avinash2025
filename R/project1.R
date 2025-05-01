#' Calcuate the number of tickets to sell using binomial and normal disribution
#'
#' @param N Number of available seats
#' @param gamma Maximum acceptable probabilily of overbooking
#' @param p Probability that a single passenger shows up for the flight
#'
#' @returns A list of the inputs and the plots of the discrete and normal distributions
#' @export
#
#' @examples
#' ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  nd <- N
  while (pbinom(N, size = nd, prob = p) < 1 - gamma) {
    nd <- nd + 1
  }

  obj_fun <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    pnorm(N, mean = mu, sd = sigma) - (1 - gamma)
  }
  nc <- uniroot(obj_fun, lower = N, upper = N + 50)$root

  n_discrete <- seq(N, nd + 10)
  obj_discrete <- sapply(n_discrete, function(n) {
    1 - gamma - pbinom(N, size = n, prob = p)
  })

  n_continuous <- seq(N, ceiling(nc) + 10, by = 0.01)
  obj_continuous <- sapply(n_continuous, function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N, mean = mu, sd = sigma)
  })

  plot(n_discrete, obj_discrete, type = "b", pch = 19, col = "blue", lty = 3,
       ylab = "Objective", xlab = "n", main = paste0(
         "Objective Vs n to find optimal tickets sold\n(",
         nd, ") gamma= ", gamma, " N=", N, " discrete"
       ))
  abline(v = nd, col = "red", lwd = 2)
  points(nd, 0, col = "red", pch = 19, cex = 1.5)

  plot(n_continuous, obj_continuous, type = "l", lwd = 2, col = "black",
       ylab = "Objective", xlab = "n", main = paste0(
         "Objective Vs n to find optimal tickets sold\n(",
         round(nc, 10), ") gamma= ", gamma, " N=", N, " continuous"
       ))
  abline(v = nc, col = "blue", lwd = 2)

  exact_y <- 1 - gamma - pnorm(N, mean = nc * p, sd = sqrt(nc * p * (1 - p)))
  points(nc, exact_y, col = "blue", pch = 19, cex = 1.5)

  return(list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  ))
}

