context("Test on the automated tuning function in tuning.R")

test_that("Item lengths are as anticipated", {

  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  #Generate vector of time orders
  df$t <- c(1:nrow(df))

  #Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods,length.out=nrow(df))

  #Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p,nPeriods=nperiods)
  Sx.ex <- SxMatrixCalc(X)


  tuning.test <- knn.forecast.randomsearch.tuning(grid.len = 10
                                                  ,St.in = St.ex
                                                  ,Sp.in = Sp.ex
                                                  ,Sx.in = Sx.ex
                                                  ,y.in = ex.series
                                                  ,test.h = 2
                                                  ,max.k = 2
                                                  ,val.holdout.len = 2)

  #Should be 6 total items returned the list for any valid input
  expect_equal(length(tuning.test), 6)
  #Should be 3 weights in weight.opt, 1 value for k.opt, a matrix of length nxn,
  #1 Test.MAPE.opt value, grid.len number of MAPE.all values, and 4 variables in the Grid dataframe
  expect_equal(unname(lengths(tuning.test)), c(3,1,prod(dim(St.ex)),1,10,4))
})



test_that("Bad integer arguments throw errors or warnings", {

  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  df <- data.frame(ex.series)
  #Generate vector of time orders
  df$t <- c(1:nrow(df))

  #Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods,length.out=nrow(df))

  #Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)

  St.ex <- StMatrixCalc(df$t)
  Sp.ex <- SpMatrixCalc(df$p,nPeriods=nperiods)
  Sx.ex <- SxMatrixCalc(X)

  #Test too long grid.len
  expect_error(knn.forecast.randomsearch.tuning(grid.len = c(10, 11)
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = 2))

  #Test non-numeric grid.len
  expect_error(knn.forecast.randomsearch.tuning(grid.len = '10'
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = 2))

  #Test non-integer grid.len
  expect_warning(knn.forecast.randomsearch.tuning(grid.len = 10.2
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = 2))


  #Test too long test.h
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = c(2, 1)
                                                , max.k = 2
                                                , val.holdout.len = 2))

  #Test non-numeric test.h
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = '2'
                                                , max.k = 2
                                                , val.holdout.len = 2))

  #Test non-integer test.h
  expect_warning(knn.forecast.randomsearch.tuning(grid.len = 10
                                                  , St.in = St.ex
                                                  , Sp.in = Sp.ex
                                                  , Sx.in = Sx.ex
                                                  , y.in = ex.series
                                                  , test.h = 2.2
                                                  , max.k = 2
                                                  , val.holdout.len = 2))


  #Test too long val.holdout.len
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = c(2,1)))

  #Test non-numeric val.holdout.len
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = '2'))

  #Test non-integer val.holdout.len
  expect_warning(knn.forecast.randomsearch.tuning(grid.len = 10
                                                  , St.in = St.ex
                                                  , Sp.in = Sp.ex
                                                  , Sx.in = Sx.ex
                                                  , y.in = ex.series
                                                  , test.h = 2
                                                  , max.k = 2
                                                  , val.holdout.len = 2.2))

  #Test too long min.k
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = 2
                                                , min.k = c(1,1)))

  #Test non-numeric min.k
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = 2
                                                , val.holdout.len = 2
                                                , min.k = '1'))

  #Test non-integer min.k
  expect_warning(knn.forecast.randomsearch.tuning(grid.len = 10
                                                  , St.in = St.ex
                                                  , Sp.in = Sp.ex
                                                  , Sx.in = Sx.ex
                                                  , y.in = ex.series
                                                  , test.h = 2
                                                  , max.k = 2
                                                  , val.holdout.len = 2
                                                  , min.k = 1.2))


  #Test too long max.k
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = c(2,2)
                                                , val.holdout.len = 2))

  #Test non-numeric max.k
  expect_error(knn.forecast.randomsearch.tuning(grid.len = 10
                                                , St.in = St.ex
                                                , Sp.in = Sp.ex
                                                , Sx.in = Sx.ex
                                                , y.in = ex.series
                                                , test.h = 2
                                                , max.k = '2'
                                                , val.holdout.len = 2))

  #Test non-integer max.k
  expect_warning(knn.forecast.randomsearch.tuning(grid.len = 10
                                                  , St.in = St.ex
                                                  , Sp.in = Sp.ex
                                                  , Sx.in = Sx.ex
                                                  , y.in = ex.series
                                                  , test.h = 2
                                                  , max.k = 2.2
                                                  , val.holdout.len = 2))


})




