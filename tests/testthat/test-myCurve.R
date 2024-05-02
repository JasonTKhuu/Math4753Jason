test_that("curve works", {

  mu <- 2
  sigma <- 4
  a <- 1

  result <- myncurve(mu, sigma, a)

  expect_equal(result$mu, mu)
  expect_equal(result$sigma, sigma)
  expect_equal(result$a, a)
})
