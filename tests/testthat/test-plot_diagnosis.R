library(diagnoser)

context("Test for p2lt and p2star")

test_that("p2lt and p2star", {
  expect_equal(p2lt(0.002), as.character(expression(italic(P) < 0.01)))
  expect_equal(p2star(0.02), "**")
})

