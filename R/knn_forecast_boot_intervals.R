

#' KNN Forecast Bootstrap Prediction Intervals
#'
#' @param Sim.Mat.in numeric matrix of similarities (recommend use of \code{S_w}, see \code{SwMatrixCalc()}).
#' @param f.index.in numeric vector indicating the indices of \code{Sim.Mat.in} and \code{y.in} which correspond to the time order of the points to be forecast.
#' @param k.in integer value indicating the the number of nearest neighbors to be considered in forecasting.
#' @param y.in numeric vector of the response series to be forecast.
#' @param burn.in integer value which indicates how many points at the start of the series to set aside as eligible neighbors before calculating forecast errors to be re-sampled.
#' @param B integer value representing the number of bootstrap replications, this will be the number of forecasts simulated and used to calculate outputs.
#' @param return.simulations logical value indicating whether to return all simulated forecasts.
#' @param level numeric value over the range (0,1) indicating the confidence level for the prediction intervals.
#'
#' @return  list of the following components:
#' \describe{
#'           \item{lb}{numeric vector of the same length as \code{f.index.in}, with the estimated lower bound of the prediction interval of the forecast.}
#'           \item{ub}{numeric vector of the same length as \code{f.index.in}, with the estimated upper bound of the prediction interval of the forecast.}
#'           \item{mean}{numeric vector of the same length as \code{f.index.in}, with the mean of the \code{B} simulated paths for each forecasted point.}
#'           \item{median}{numeric vector of the same length as \code{f.index.in}, with the median of the \code{B} simulated paths for each forecasted point.}
#'           \item{simulated.paths}{numeric matrix where each of the \code{B} rows contains a simulated path for the points in \code{f.index.in}, only returned if \code{return.simulations} is \code{TRUE}.}
#' }
#' @export
#'
#' @examples
#'
#'
#'
#'
knn.forecast.boot.intervals <- function(Sim.Mat.in
                                        , f.index.in
                                        , k.in
                                        , y.in
                                        , burn.in = NULL
                                        , B = 200
                                        , return.simulations = FALSE
                                        , level = .95){

  #burn.in defaults to k.in
  if(is.null(burn.in)){
    burn.in <- k.in
  }

  #Set up sim mat and response series to gather residuals
  Sim.Mat.noval <- Sim.Mat.in[-(f.index.in), -(f.index.in)]
  y.noval <- y.in[-(f.index.in)]

  #Gather pool of residuals to sample from
  gather.res.index <- c((burn.in + 1):length(y.noval))
  res.len <- length(gather.res.index)
  et <- numeric(length = res.len)
  test.frcst <- numeric(length = res.len)


  for (t in gather.res.index){
    Sim.Mat.noval.t <- Sim.Mat.noval[1:t, 1:t]
    y.t <- y.noval[1:t]
    y.t.act <- y.noval[t]

    res.est.frcst.t <- knn.forecast(Sim.Mat.in = Sim.Mat.noval.t
                                    , f.index.in = t
                                    , k.in = k.in
                                    , y.in = y.t)

    et.t <- y.t.act - res.est.frcst.t

    et[t - burn.in] <- et.t
    test.frcst[t - burn.in] <- res.est.frcst.t

  }

  h <- length(f.index.in)
  boot.paths <- matrix(NA, nrow = B, ncol = h)

  #Create B simulated forecasts
  for (b in c(1:B)){

    #This loop produces the bth simulated path
    for (i in c(1:h)){
      one.hop.point <- f.index.in[i]
      one.hop.res <- base::sample(et, size = 1, replace = TRUE)

      #Assign a 0 vector for the new forecast if first iteration, then append
      #previously estimated points in later iterations
      if(i == 1){
        path <- numeric(length = h)
        y.sim <- y.noval
      } else {
        y.sim <- c(y.sim, path[i - 1])
      }

      #Removing uneeded columns in the sim matrix to avoid warnings
      if (i != h){
        remove.index <- f.index.in[(i + 1):h]
        #Will need to make sure Sw has dims as high as max f.index.in
        #(may need that in knn.forecast too)
        Sim.Mat.one.hop <- Sim.Mat.in[-(remove.index), -(remove.index)]
      } else {
        Sim.Mat.one.hop <- Sim.Mat.in
      }

      #Produce one step ahead forecast and store result for next iteration
      one.hop.frcst <- knn.forecast(Sim.Mat.in = Sim.Mat.one.hop
                                    , f.index.in = one.hop.point
                                    , k.in = k.in
                                    , y.in = y.sim)

      one.hop.frsct.w.res <- one.hop.frcst + one.hop.res

      path[i] <- one.hop.frsct.w.res
    }
    boot.paths[b, ] <- path
  }

  #Use simulated paths for estimates
  ub.boot <- apply(boot.paths, MARGIN = 2, FUN = stats::quantile
                   , probs = (level + 1) / 2)

  lb.boot <- apply(boot.paths, MARGIN = 2, FUN = stats::quantile
                   , probs = (1 - level) / 2)

  mean.boot <- apply(boot.paths, MARGIN = 2, FUN = mean)
  median.boot <- apply(boot.paths, MARGIN = 2, FUN = stats::median)

  return.list <- list(lb = lb.boot
                      , ub = ub.boot
                      , mean = mean.boot
                      , median = median.boot)

  if (return.simulations == TRUE){
    simulations <- list(simulated.paths = boot.paths)
    return.list <- append(return.list, simulations)
  }

  return(return.list)

}
