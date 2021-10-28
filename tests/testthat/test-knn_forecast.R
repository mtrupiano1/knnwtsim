context("Tests that KNN functions return expected results")

test_that("NNreg returns average of correct k nearest neighbors", {

  v.test <- c(.5,1,.3,.4,.7)
  k.test <- 3
  y.test <- c(1,5,30,30,2)

  #if v.test represents similarity the 1st, 2nd, and 5th observations in y.test are neighbors
  y.hat.expected <- mean(c(1,5,2))

  expect_equal(NNreg(v=v.test,k.in2=k.test,y.in2 = y.test), y.hat.expected)
})


test_that("knn.forecast returns expected estimates", {

  Sim.Mat.test <- matrix(c(1,.2,.3,.4,.5,
                      .5,1,.4,.3,.2,
                      .4,.3,1,.2,.5,
                      .3,.2,.5,1,.4,
                      .2,.5,.4,.3,1),nrow=5,ncol=5,byrow=TRUE)
  k.test <- 2
  y.test <- c(1,5,-4,30,40,50)
  f.index.test <- c(4,5)

  #Neighbors for index 4 should be index 1 & 2
  #Neighbors for index 5 should be index 1 & 3

  expected.forecast <- c(3,-1.5)

  expect_equal(knn.forecast(Sim.Mat.in = Sim.Mat.test,f.index.in = f.index.test
                            ,k.in=k.test,y.in = y.test), expected.forecast)
})
