context("Tests that KNN functions return expected results")

test_that("NNreg returns average of correct k nearest neighbors", {

  v.test <- c(.5, 1, .3, .4, .7)
  k.test <- 3
  y.test <- c(1, 5, 30, 30, 2)

  #if v.test represents similarity the 1st, 2nd, and 5th observations in y.test are neighbors
  y.hat.expected <- mean(c(1, 5, 2))

  expect_equal(NNreg(v = v.test, k.in2 = k.test, y.in2 = y.test), y.hat.expected)
})


test_that("knn.forecast returns expected estimates", {

  Sim.Mat.test <- matrix(c(1, .2, .3, .4, .5,
                      .2, 1, .4, .3, .2,
                      .3, .4, 1, .2, .5,
                      .4, .3, .2, 1, .4,
                      .5, .2, .5, .4, 1), nrow = 5, ncol = 5, byrow = TRUE)
  k.test <- 2
  y.test <- c(1, 5, -4, 30, 40, 50)
  f.index.test <- c(4, 5)

  #Neighbors for index 4 should be index 1 & 2
  #Neighbors for index 5 should be index 1 & 3

  expected.forecast <- c(3,-1.5)

  expect_equal(knn.forecast(Sim.Mat.in = Sim.Mat.test,f.index.in = f.index.test
                            , k.in = k.test, y.in = y.test), expected.forecast)
})



test_that("knn.forecast throws errors for bad y.in and f.index.in arguments", {


  Sim.Mat.test <- matrix(c(1, .2, .3, .4, .5,
                           .2, 1, .4, .3, .2,
                           .3, .4, 1, .2, .5,
                           .4, .3, .2, 1, .4,
                           .5, .2, .5, .4, 1), nrow = 5, ncol = 5, byrow = TRUE)
  k.test <- 2
  y.test <- c(1, 5, -4, 30, 40, 50)
  f.index.test <- c(4, 5)

  f.index.ch <- c("a", "b")
  y.ch <- c("a", "b")

  y.mat <- matrix(y.test, nrow = 2, ncol = 3)
  f.index.mat <- matrix(f.index.test, nrow = 2, ncol = 1)


  #test for non-numeric f.index.in or y.in
  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.ch
                            , k.in = k.test, y.in = y.test))

  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                            , k.in = k.test, y.in = y.ch))

  #test for non-vector f.index.in or y.in
  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                            , k.in = k.test, y.in = y.mat))

  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.mat
                            , k.in = k.test, y.in = y.test))

})

test_that("knn.forecast throws errors for bad Sim.Mat.in arguments", {


  Sim.Mat.test <- matrix(c(1, .2, .3, .4, .5,
                           .2, 1, .4, .3, .2,
                           .3, .4, 1, .2, .5,
                           .4, .3, .2, 1, .4,
                           .5, .2, .5, .4, 1), nrow = 5, ncol = 5, byrow = TRUE)
  k.test <- 2
  y.test <- c(1, 5, -4, 30, 40, 50)
  f.index.test <- c(4, 5)

  ch.mat <- matrix(as.character(Sim.Mat.test), nrow = 5, ncol = 5)
  Sim.vec <- as.vector(Sim.Mat.test)

  Sim.Mat.nosymmetric <- Sim.Mat.test
  Sim.Mat.nosymmetric[1, 2] <- 1



  #test for non-numeric Sim.Mat.in
  expect_error(knn.forecast(Sim.Mat.in = ch.mat, f.index.in = f.index.test
                            , k.in = k.test, y.in = y.test))


  #test for non-matrix Sim.Mat.in
  expect_error(knn.forecast(Sim.Mat.in = Sim.vec, f.index.in = f.index.test
                            , k.in = k.test, y.in = y.test))

  #test for non-symmetric Sim.Mat.in
  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.nosymmetric, f.index.in = f.index.test
                            , k.in = k.test, y.in = y.test))

})



test_that("knn.forecast throws errors or warnings for bad k.in arguments", {


  Sim.Mat.test <- matrix(c(1, .2, .3, .4, .5,
                           .2, 1, .4, .3, .2,
                           .3, .4, 1, .2, .5,
                           .4, .3, .2, 1, .4,
                           .5, .2, .5, .4, 1), nrow = 5, ncol = 5, byrow = TRUE)
  k.test <- 2
  y.test <- c(1, 5, -4, 30, 40, 50)
  f.index.test <- c(4, 5)

  k.ch <- "a"
  k.too.long <- c(2,2)
  k.not.int <- 2.3

  #test for non-numeric k.in
  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                            , k.in = k.ch, y.in = y.test))


  #test for too long k.in
  expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                            , k.in = k.too.long, y.in = y.test))

  #test for non-integer k.in
  expect_warning(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                            , k.in = k.not.int, y.in = y.test))

})

test_that("knn.forecast throws warnings and forecasts correctly
  when Sim.Mat.in row count is greater than max forecast index", {


  Sim.Mat.test <- matrix(c(1, .2, .3, .4, .5,
                           .2, 1, .4, .3, .2,
                           .3, .4, 1, .2, .5,
                           .4, .3, .2, 1, .4,
                           .5, .2, .5, .4, 1), nrow = 5, ncol = 5, byrow = TRUE)
  k.test <- 2
  y.test <- c(1, 5, -4, 30, 40, 50)
  f.index.test <- c(4)
  expected.forecast <- 3

  #test warning and correct forecast
  expect_warning(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                              , k.in = k.test, y.in = y.test))

  expect_equal(suppressWarnings(knn.forecast(Sim.Mat.in = Sim.Mat.test,f.index.in = f.index.test
                            , k.in = k.test, y.in = y.test)), expected.forecast)

})


test_that("knn.forecast throws errors when there are less eligible neighbors than k.in", {


    Sim.Mat.test <- matrix(c(1, .2, .3, .4, .5,
                             .2, 1, .4, .3, .2,
                             .3, .4, 1, .2, .5,
                             .4, .3, .2, 1, .4,
                             .5, .2, .5, .4, 1), nrow = 5, ncol = 5, byrow = TRUE)
    k.test <- 4
    y.test <- c(1, 5, -4, 30, 40, 50)
    f.index.test <- c(4, 5)


    #test warning and correct forecast
    expect_error(knn.forecast(Sim.Mat.in = Sim.Mat.test, f.index.in = f.index.test
                                , k.in = k.test, y.in = y.test))

})



