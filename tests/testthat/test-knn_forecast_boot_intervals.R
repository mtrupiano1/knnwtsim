context("Tests on the bootstrapped prediction intervals function
        in knn_forecast_boot_intervals.R")


test_that("knn.forecast.boot.intervals output lengths are as anticipated", {

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


  B.arg <- 10

  boot.test.sim.false <- knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , B = B.arg)

  boot.test.sim.true <- knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                    , f.index.in = f.index
                                                    , y.in = ex.series
                                                    , k.in = pre.tuned.k
                                                    , B = B.arg
                                                    , return.simulations = TRUE)

  #Should be 4 total items returned if return.simulations is FALSE
  expect_equal(length(boot.test.sim.false), 4)

  #Should be 5 total items returned if return.simulations is TRUE
  expect_equal(length(boot.test.sim.true), 5)


  #lb, ub, mean, and median should all be the same length as f.index.in
  #simulated.paths should have length f.index.in * B
  f.length <- length(f.index)

  expect_equal(unname(lengths(boot.test.sim.true))
               , c(rep(f.length, 4), f.length * B.arg))
})




test_that("knn.forecast.boot.intervals throws errors or warnings for
          bad integer arguments", {

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


test_that("knn.forecast.boot.intervals throws errors for
          bad vector argument types", {

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


test_that("knn.forecast.boot.intervals throws errors for
          bad Sim.Mat.in arguments", {


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

            ch.Sw.ex <- matrix(as.character(Sw.ex), nrow = 5, ncol = 5)
            vec.Sw.ex <- as.vector(Sw.ex)

            Sw.nosymmetric <- Sw.ex
            Sw.nosymmetric[1, 2] <- 1


            #test for non-numeric Sim.Mat.in
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = ch.Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , B = 10))

            #test for non-matrix Sim.Mat.in
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = vec.Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , B = 10))

            #test for non-symmetric Sim.Mat.in
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.nosymmetric
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , B = 10))

          })

test_that("knn.forecast.boot.intervals throws errors for
          bad return.simulations arguments", {

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

            #test for too long return.simulations
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , return.simulations = c(TRUE, FALSE)
                                                     , B = 10))

            #test for non-logical return.simulations
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , return.simulations = 1
                                                     , B = 10))

          })

test_that("knn.forecast.boot.intervals throws errors for
          bad level arguments", {

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

            #Test too long level
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , level = c(.05, .95)
                                                     , B = 10))

            #Test non-numeric level
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , level = '.95'
                                                     , B = 10))

            #Test levels outside (0,1) range
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , level = 0
                                                     , B = 10))


            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , level = 1
                                                     , B = 10))


            })


test_that("knn.forecast.boot.intervals throws errors and warnings
          for conflicting size checks", {

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
            f.index.s <- c((n - 5 + 1):(length(ex.series) - 1))
            f.index.l <- c((n - 5 + 1):(length(ex.series) + 1))

            ex.rmv <- c((n - 6 + 1):length(ex.series))



            #Test extra Sim.Mat.in rows
            expect_warning(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                       , f.index.in = f.index.s
                                                       , y.in = ex.series
                                                       , k.in = pre.tuned.k
                                                       , B = 10))

            #Test max f.index.in above Sim.Mat.in row count
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index.l
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , B = 10))


            #Test min f.index.in more than 1 above y.in length
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series[ex.rmv]
                                                     , k.in = pre.tuned.k
                                                     , B = 10))

            #Test burn.in less than k.in
            expect_error(knn.forecast.boot.intervals(Sim.Mat.in = Sw.ex
                                                     , f.index.in = f.index
                                                     , y.in = ex.series
                                                     , k.in = pre.tuned.k
                                                     , burn.in = 4
                                                     , B = 10))

          })

