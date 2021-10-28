context("Tests that KNN functions return expected results")

test_that("NNreg returns average of correct k nearest neighbors", {

  v.test <- c(.5,1,.3,.4,.7)
  k.test <- 3
  y.test <- c(1,5,30,30,2)

  #if v.test represents similarity the 1st, 2nd, and 5th observationsin y.test are neighbors
  y.hat.expected <- mean(c(1,5,2))

  expect_equal(NNreg(v=v.test,k.in2=k.test,y.in2 = y.test), y.hat.expected)
})


