
test_that("myncurve correctly returns the mean (mu)", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$mu, 10, label = "The mean (mu) should be correctly stored in the list.")
})

test_that("myncurve correctly returns the standard deviation (sigma)", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$sigma, 2, label = "The standard deviation (sigma) should be correctly stored in the list.")
})

test_that("myncurve correctly computes the probability P(X ≤ a)", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expected_prob <- pnorm(12, mean = 10, sd = 2)
  expect_equal(result$probability, expected_prob, tolerance = 1e-6, label = "The probability P(X ≤ a) should match pnorm().")
})
