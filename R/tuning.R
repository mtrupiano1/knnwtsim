
#' Tune \code{knn.forecast()} Hyperparameters with Random Search
#'
#' A simplistic automated hyperparameter tuning function which randomly
#' generates a grid of hyperparameter sets used to build corresponding \code{S_w} similarity matrices
#' which are used in \code{knn.forecast()} test against the last \code{test.h} points of \code{y.in} after
#' any \code{val.holdout.len} points are removed from the end of \code{y.in}. The best performing set of
#' parameters based on MAPE over over the forecast horizon of \code{test.h} points are returned as part of a list
#' alongside the 'optimum' weighted similarity matrix \code{Sw.opt}, the \code{Grid} of tested sets, and the MAPE
#' results. MAPE is the average of absolute percent errors for each point calculated as: \code{abs((test.actuals - test.forecast.i) / test.actuals) * 100}. Where \code{test.forecast.i}
#' and \code{test.actuals} are both numeric vectors.
#'
#'
#' @param grid.len integer value representing the number of hyperparameter sets to generate and test, must be \code{>= 1}.
#' @param St.in numeric and symmetric matrix of similarities, can be generated with \code{StMatrixCalc()}.
#' @param Sp.in numeric and symmetric matrix of similarities, can be generated with \code{SpMatrixCalc()}.
#' @param Sx.in numeric and symmetric matrix of similarities, can be generated with \code{SxMatrixCalc()}.
#' @param y.in numeric vector of the response series to be forecast.
#' @param test.h integer value representing the number of points in the test forecast horizon, must be \code{>= 1}.
#' @param max.k integer value representing the maximum value of k, \code{knn.forecast()} should use, will be set to \code{min(floor((length(y.in)) * .4), length(y.in) - val.holdout.len - test.h)} if \code{NULL} or \code{NA} is passed. Note this \code{NA} behavior differs from \code{knnwtsim} version 0.1.0.
#'  If a numeric value is passed it must be \code{>= 1}.
#' @param val.holdout.len integer value representing the number of observations at the end of the series to be removed in testing forecast if desired to leave a validation set after tuning, must be \code{>= 0}.
#' @param min.k integer value representing the minimum value of k, \code{knn.forecast()} should use, must be \code{>= 1}.
#'
#' @return list of the following components:
#' \describe{
#'           \item{weight.opt}{numeric vector of the 3 weights to generate \code{Sw.opt} in alpha, beta, gamma order which achieved the best performance in terms of MAPE.}
#'           \item{k.opt}{integer value of neighbors used in \code{knn.forecast()} which achieved the best performance in terms of MAPE.}
#'           \item{Sw.opt}{numeric matrix of similarities calculated using \code{S_w}, with the best performing set of hyperparameters.}
#'           \item{Test.MAPE}{numeric value of the MAPE result for the optimum hyperparamter set achieved on the test points.}
#'           \item{MAPE.all}{numeric vector of MAPE results, each observation corresponds to the row in \code{Grid} of the same index.}
#'           \item{Grid}{dataframe of all hyperparameter sets tested in the tuning.}
#' }
#'
#'
#'
#' @export
#'
#' @seealso
#' * Trupiano (2021) arXiv:2112.06266 for information on the formulation of \code{S_w}.
#' * [StMatrixCalc()] for information on the calculation of \code{S_t}.
#' * [SpMatrixCalc()] for information on the calculation of \code{S_p}.
#' * [SxMatrixCalc()] for information on the calculation of \code{S_x}.
#' * [knn.forecast()] for the function called to perform knn regression.
#' @md
#'
#'
#' @examples
#' data("simulation_master_list")
#' series.index <- 15
#' ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x
#'
#' df <- data.frame(ex.series)
#' # Generate vector of time orders
#' df$t <- c(1:nrow(df))
#' # Generate vector of periods
#' nperiods <- simulation_master_list[[series.index]]$seasonal.periods
#' df$p <- rep(1:nperiods, length.out = nrow(df))
#' # Pull corresponding exogenous predictor(s)
#' X <- as.matrix(simulation_master_list[[series.index]]$x.chng)
#'
#' St.ex <- StMatrixCalc(df$t)
#' Sp.ex <- SpMatrixCalc(df$p, nPeriods = nperiods)
#' Sx.ex <- SxMatrixCalc(X)
#'
#' tuning.test <- knn.forecast.randomsearch.tuning(
#'   grid.len = 10,
#'   y.in = ex.series,
#'   St.in = St.ex,
#'   Sp.in = Sp.ex,
#'   Sx.in = Sx.ex,
#'   test.h = 3,
#'   max.k = 10,
#'   val.holdout.len = 3
#' )
knn.forecast.randomsearch.tuning <- function(grid.len = 100,
                                             St.in,
                                             Sp.in,
                                             Sx.in,
                                             y.in,
                                             test.h = 1,
                                             max.k = NULL,
                                             val.holdout.len = 0,
                                             min.k = 1) {


  # integer argument type checks
  if ((!(is.vector(grid.len, mode = "numeric"))) |
    (!(identical(length(grid.len), 1L)))) {
    stop("grid.len should be an integer with length 1L")
  } else if (!(identical((grid.len %% 1), 0))) {
    warning("grid.len should be an integer
            , argument will be floored to nearest whole number")
    grid.len <- floor(grid.len)
  }

  if ((!(is.vector(test.h, mode = "numeric"))) |
    (!(identical(length(test.h), 1L)))) {
    stop("test.h should be an integer with length 1L")
  } else if (!(identical((test.h %% 1), 0))) {
    warning("test.h should be an integer
            , argument will be floored to nearest whole number")
    test.h <- floor(test.h)
  }

  if ((!(is.vector(val.holdout.len, mode = "numeric"))) |
    (!(identical(length(val.holdout.len), 1L)))) {
    stop("val.holdout.len should be an integer with length 1L")
  } else if (!(identical((val.holdout.len %% 1), 0))) {
    warning("val.holdout.len should be an integer
            , argument will be floored to nearest whole number")
    val.holdout.len <- floor(val.holdout.len)
  }

  if ((!(is.vector(min.k, mode = "numeric"))) |
    (!(identical(length(min.k), 1L)))) {
    stop("min.k should be an integer with length 1L")
  } else if (!(identical((min.k %% 1), 0))) {
    warning("min.k should be an integer
            , argument will be floored to nearest whole number")
    min.k <- floor(min.k)
  }

  # multi layered if statement is here because of the move to the recommended
  # default argument value without breaking calls on previous versions
  if (!identical(max.k, NULL)) {
    if (!identical(max.k, NA)) {
      if (((!(is.vector(max.k, mode = "numeric"))) |
        (!(identical(length(max.k), 1L))))) {
        stop("max.k should be an integer with length 1L, NULL, or NA")
      } else if (!(identical((max.k %% 1), 0))) {
        warning("max.k should be an integer
                , argument will be floored to nearest whole number")
        max.k <- floor(max.k)
      }
    }
  }

  # matrix argument checks
  if (!((is.matrix(St.in) & is.numeric(St.in))) |
    !((is.matrix(Sp.in) & is.numeric(Sp.in))) |
    !((is.matrix(Sx.in) & is.numeric(Sx.in)))) {
    stop("St.in, Sp.in, and Sx.in should be numeric matrices")
  } else if (!(isSymmetric.matrix(St.in)) |
    !(isSymmetric.matrix(Sp.in)) |
    !(isSymmetric.matrix(Sx.in))) {
    stop("St.in, Sp.in, and Sx.in should be a symmetric matrices")
  }

  if (!(identical(dim(St.in)[1], dim(Sp.in)[1])) |
    !(identical(dim(St.in)[1], dim(Sx.in)[1]))) {
    stop("Dimensions of St.in, Sp.in, and Sx.in are not all equal")
  }


  # response series checks
  if (!(is.vector(y.in, mode = "numeric"))) {
    stop("y.in should be a numeric vector")
  }

  n <- length(y.in)

  if (!(identical(dim(St.in)[1], n))) {
    stop("For tuning the length of y.in should match the number of rows of
         the similarity matrix inputs: St.in, Sp.in, Sx.in")
  }

  viable.neighbors.count <- n - val.holdout.len - test.h

  # set a maximum for k, multi layered if statement is here because of
  # the move to the recommended default argument value without breaking
  # calls on previous versions
  if (is.null(max.k)) {
    k.cap <- min(floor((length(y.in)) * .4), viable.neighbors.count)
  } else if ((is.na(max.k))) {
    k.cap <- min(floor((length(y.in)) * .4), viable.neighbors.count)
  } else {
    if (max.k > viable.neighbors.count) {
      warning(paste0("max.k is larger than the number of viable neighbors
                         given the test.h and val.holdout.len supplied, the
                         maximum number of viable neighbors will be used
                         instead: ", viable.neighbors.count))
    }
    k.cap <- min(max.k, viable.neighbors.count)
  }



  if (min.k > k.cap) {
    stop("min.k is greater than max.k")
  }

  # check if integer arguments are below minimum values
  if (any(c(grid.len, test.h, min.k, k.cap) < 1)){
    stop("None of the integer arguments: grid.len, test.h, min.k, and max.k
         can be < 1")
  }

  if (val.holdout.len < 0){
    stop("Integer argument val.holdout.len cannot be < 0")
  }


  # randomly propose sets of hyperparameters
  ks <- base::sample(min.k:k.cap, size = grid.len, replace = TRUE)
  alpha.0 <- stats::runif(grid.len, min = .001, max = 10000)
  beta.0 <- stats::runif(grid.len, min = .001, max = 10000)
  gamma.0 <- stats::runif(grid.len, min = .001, max = 10000)

  Theta <- data.frame(ks, alpha.0, beta.0, gamma.0)

  # normalize weights to sum to 1
  Theta$alpha <- Theta$alpha.0 / (Theta$alpha.0 + Theta$beta.0 + Theta$gamma.0)
  Theta$beta <- Theta$beta.0 / (Theta$alpha.0 + Theta$beta.0 + Theta$gamma.0)
  Theta$gamma <- Theta$gamma.0 / (Theta$alpha.0 + Theta$beta.0 + Theta$gamma.0)


  # remove any holdout points from y
  if (val.holdout.len > 0) {
    val.remove <- as.vector(c((n - val.holdout.len + 1):(n)))
    y.in <- y.in[-(val.remove)]
  }

  # create the test index based on test.h argument and
  # pull out actuals for comparison
  test.index <- c((length(y.in) - test.h + 1):length(y.in))
  test.actuals <- y.in[test.index]

  # set matrix to capture APE
  APE.h.i <- matrix(data = NA, nrow = nrow(Theta), ncol = length(test.index))

  # forecast loop to calculate test index APE for all Theta rows
  for (i in c(1:nrow(Theta))) {
    k.theta.i <- Theta[i, c("ks")]
    alpha.theta.i <- Theta[i, c("alpha")]
    beta.theta.i <- Theta[i, c("beta")]
    gamma.theta.i <- Theta[i, c("gamma")]

    # calculate the Sw matrix for the set of weights
    Sw.theta.i <- (alpha.theta.i * St.in + beta.theta.i * Sp.in
      + gamma.theta.i * Sx.in)

    # remove holdout points from Sw
    if (val.holdout.len > 0) {
      Sw.theta.i <- Sw.theta.i[-(val.remove), -(val.remove)]
    }

    # forecast using Sw.theta.i and compare to actuals
    test.forecast.i <- knn.forecast(
      Sim.Mat.in = Sw.theta.i,
      f.index.in = test.index,
      k.in = k.theta.i,
      y.in = y.in
    )
    APE.h.i[i, ] <- abs((test.actuals - test.forecast.i) / test.actuals) * 100
  }


  MAPE.h.i <- rowMeans(APE.h.i)

  # pull results for best performance in terms of MAPE over the forecast horizon
  Test.MAPE.opt <- MAPE.h.i[which.min(MAPE.h.i)]

  k.opt <- Theta[which.min(MAPE.h.i), c("ks")]
  alpha.opt <- Theta[which.min(MAPE.h.i), c("alpha")]
  beta.opt <- Theta[which.min(MAPE.h.i), c("beta")]
  gamma.opt <- Theta[which.min(MAPE.h.i), c("gamma")]

  weight.opt <- c(alpha.opt, beta.opt, gamma.opt)

  Sw.opt <- alpha.opt * St.in + beta.opt * Sp.in + gamma.opt * Sx.in


  # compile results for final list
  return.list <- list(
    weight.opt = weight.opt,
    k.opt = k.opt,
    Sw.opt = Sw.opt,
    Test.MAPE.opt = Test.MAPE.opt,
    MAPE.all = MAPE.h.i,
    Grid = Theta[, c("ks", "alpha", "beta", "gamma")]
  )

  return(return.list)
}
