context("Tests on the bootstrapped prediction intervals function
        in knn_forecast_boot_intervals.R")


test_that("Bad integer arguments throw errors or warnings", {

  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  #Weights pre tuned by random search. In alpha, beta, gamma order
  pre.tuned.wts <- c(0.2148058, 0.2899638, 0.4952303)
  pre.tuned.k <- 5

  df <- data.frame(ex.series)
  #Generate vector of time orders
  df$t <- c(1:nrow(df))

  #Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods,length.out = nrow(df))

  #Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)


  #Calculate the weighted similarity matrix using Sw
  Sw.ex <- SwMatrixCalc(t.in = df$t
                        , p.in = df$p, nPeriods.in = nperiods
                        , X.in = X
                        , weights = pre.tuned.wts )

  n <- length(ex.series)
  #Index we want to forecast
  f.index <- c((n - 5 + 1):length(ex.series))

  #Test too long k.in
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = c(5, 1)
                                           , B = 10))

  #Test non-numeric k.in
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = "5"
                                           , B = 10))


  #Test non-integer k.in
  expect_warning(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = 5.5
                                           , B = 10))



  #Test too long B
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = pre.tuned.k
                                           , B = c(10, 11)))

  #Test non-numeric B
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = pre.tuned.k
                                           , B = 'B'))


  #Test non-integer B
  expect_warning(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                             , f.index.in = f.index
                                             , y.in = ex.series
                                             , k.in = pre.tuned.k
                                             , B = 10.3))

  #Test too long burn.in
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = pre.tuned.k
                                           , burn.in = c(10, 11)
                                           , B = 10))

  #Test non-numeric burn.in
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ex.series
                                           , k.in = pre.tuned.k
                                           , burn.in = 'B'
                                           , B = 10))


  #Test non-integer burn.in
  expect_warning(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                             , f.index.in = f.index
                                             , y.in = ex.series
                                             , k.in = pre.tuned.k
                                             , burn.in = 10.3
                                             , B = 10))

})

test_that("Bad vector argument types throw errors", {

  data("simulation_master_list")
  series.index <- 15
  ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

  #Weights pre tuned by random search. In alpha, beta, gamma order
  pre.tuned.wts <- c(0.2148058, 0.2899638, 0.4952303)
  pre.tuned.k <- 5

  df <- data.frame(ex.series)
  #Generate vector of time orders
  df$t <- c(1:nrow(df))

  #Generate vector of periods
  nperiods <- simulation_master_list[[series.index]]$seasonal.periods
  df$p <- rep(1:nperiods,length.out = nrow(df))

  #Pull corresponding exogenous predictor(s)
  X <- as.matrix(simulation_master_list[[series.index]]$x.chng)


  #Calculate the weighted similarity matrix using Sw
  Sw.ex <- SwMatrixCalc(t.in = df$t
                        , p.in = df$p, nPeriods.in = nperiods
                        , X.in = X
                        , weights = pre.tuned.wts )

  n <- length(ex.series)
  #Index we want to forecast
  f.index <- c((n - 5 + 1):length(ex.series))

  #bad y.in
  ch.ex.series <- as.character(ex.series)

  mat.ex.series <- matrix(ex.series)

  #bad f.index.in
  ch.f.index <- as.character(f.index)

  mat.f.index <- matrix(f.index)


  #Test non-numeric y.in
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = ch.ex.series
                                           , k.in = pre.tuned.k
                                           , B = 10))

  #Test non-vector y.in
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = f.index
                                           , y.in = mat.ex.series
                                           , k.in = pre.tuned.k
                                           , B = 10))

  #Test non-numeric f.index
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = ch.f.index
                                           , y.in = ex.series
                                           , k.in = pre.tuned.k
                                           , B = 10))

  #Test non-vector f.index
  expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                           , f.index.in = mat.f.index
                                           , y.in = ex.series
                                           , k.in = pre.tuned.k
                                           , B = 10))


})

