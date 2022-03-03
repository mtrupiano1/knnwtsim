context("Tests on the automated tuning function in tuning.R")

test_that("Item lengths are as anticipated", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)


  tuning.test <- knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    val.holdout.len = 2
  )

  # Should be 6 total items returned the list for any valid input
  expect_equal(length(tuning.test), 6)
  # Should be 3 weights in weight.opt, 1 value for k.opt, a matrix of length nxn,
  # 1 Test.MAPE.opt value, grid.len number of MAPE.all values,
  # and 4 variables in the Grid dataframe
  expect_equal(unname(lengths(tuning.test)), c(3, 1, prod(dim(St.ex)), 1, 10, 4))


  # Ensure NA max.k argument still works
  tuning.test.na <- knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    max.k = NA,
    test.h = 2,
    val.holdout.len = 2
  )

  # Should be 6 total items returned the list for any valid input
  expect_equal(length(tuning.test.na), 6)
})



test_that("Bad integer arguments throw errors or warnings", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)

  # Test too long grid.len
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = c(10, 11),
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test non-numeric grid.len
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = "10",
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test non-integer grid.len
  expect_warning(knn.forecast.randomsearch.tuning(
    grid.len = 10.2,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test grid.len < 1
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 0,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test too long test.h
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = c(2, 1),
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test non-numeric test.h
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = "2",
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test non-integer test.h
  expect_warning(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2.2,
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test test.h < 1
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 0,
    max.k = 2,
    val.holdout.len = 2
  ))

  # Test too long val.holdout.len
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = c(2, 1)
  ))

  # Test non-numeric val.holdout.len
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = "2"
  ))

  # Test non-integer val.holdout.len
  expect_warning(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2.2
  ))

  # Test val.holdout.len < 0
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = -1
  ))

  # Test too long min.k
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2,
    min.k = c(1, 1)
  ))

  # Test non-numeric min.k
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2,
    min.k = "1"
  ))

  # Test non-integer min.k
  expect_warning(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2,
    min.k = 1.2
  ))

  # Test min.k < 1
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2,
    val.holdout.len = 2,
    min.k = 0
  ))

  # Test too long max.k
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = c(2, 2),
    val.holdout.len = 2
  ))

  # Test non-numeric max.k
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = "2",
    val.holdout.len = 2
  ))

  # Test non-integer max.k
  expect_warning(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 2.2,
    val.holdout.len = 2
  ))

  # Test max.k < 1, min.k would also need to be lower to get to the
  # right if statement
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2,
    max.k = 0,
    val.holdout.len = 2,
    min.k = -1
  ))
})


test_that("Bad matrix argument types throw errors", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)

  # Character mats
  ch.St.ex <- matrix(as.character(St.ex),
    nrow = dim(St.ex)[1],
    ncol = dim(St.ex)[2]
  )
  ch.Sp.ex <- matrix(as.character(St.ex),
    nrow = dim(Sp.ex)[1],
    ncol = dim(Sp.ex)[2]
  )
  ch.Sx.ex <- matrix(as.character(St.ex),
    nrow = dim(Sx.ex)[1],
    ncol = dim(Sx.ex)[2]
  )

  # non - matrrx similarities
  vec.St.ex <- as.vector(St.ex)
  vec.Sp.ex <- as.vector(Sp.ex)
  vec.Sx.ex <- as.vector(Sx.ex)


  # Test non-numeric St
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = ch.St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non-numeric Sp
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = ch.Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non-numeric Sx
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = ch.Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non matrix St
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = vec.St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non matrix Sp
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = vec.Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non matrix Sx
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = vec.Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))
})


test_that("Bad matrix argument dimensions throw errors", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)



  # Test non-symmetric St by removing 1 row
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex[-(dim(St.ex)[1]), ],
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non-symmetric Sp by removing 1 row
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = Sp.ex,
    Sp.in = Sp.ex[-(dim(Sp.ex)[1]), ],
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 2
  ))

  # Test non-symmetric Sx by removing 1 row
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = Sx.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex[-(dim(Sx.ex)[1]), ],
    y.in = ex.series,
    test.h = 2
  ))

  # Test differing matrix argument dimensions
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = Sx.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex[-(dim(Sx.ex)[1]), -(dim(Sx.ex)[2])],
    y.in = ex.series,
    test.h = 2
  ))
})

test_that("Bad y.in argument types throw errors", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)

  # bad y.in
  ch.ex.series <- as.character(ex.series)

  mat.ex.series <- matrix(ex.series)


  # Test non-numeric y.in
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ch.ex.series,
    test.h = 2
  ))

  # Test non-vector y.in
  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = mat.ex.series,
    test.h = 2
  ))
})


test_that("Dimension mismatch between length of y.in
          and rows of matrix arguments throw errors", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)

  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series[-length(ex.series)],
    test.h = 2
  ))
})




test_that("k argument restrictions throw warnings and errors correctly", {
  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  # Generate vector of time orders
  df$t <- c(1:nrow(df))

  # Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods, length.out = nrow(df))

  # Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
  Sx.ex <- SxMatrixCalc(X)

  expect_warning(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 5,
    val.holdout.len = 5,
    max.k = 91
  ))


  expect_error(knn.forecast.randomsearch.tuning(
    grid.len = 10,
    St.in = St.ex,
    Sp.in = Sp.ex,
    Sx.in = Sx.ex,
    y.in = ex.series,
    test.h = 5,
    val.holdout.len = 5,
    max.k = 2,
    min.k = 3
  ))
})
