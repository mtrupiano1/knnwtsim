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
                                  ,test.h=2
                                  ,max.k =2
                                  ,val.holdout.len=2  )

  #Should be 6 total items returned the list for any valid input
  expect_equal(length(tuning.test), 6)
  #Should be 3 weights in weight.opt, 1 value for k.opt, a matrix of length nxn,
  #1 Test.MAPE.opt value, grid.len number of MAPE.all values, and 4 variables in the Grid dataframe
  expect_equal(unname(lengths(tuning.test)), c(3,1,prod(dim(St.ex)),1,10,4))
})
