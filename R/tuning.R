
#' Tune \code{knn.forecast()} Hyperparameters with Random Search
#'
#' A simplistic automated hyperparameter tuning function which randomly
#' generates a grid of hyperparameter sets used to build corresponding \code{S_w} similarity matrices
#' which are used in \code{knn.forecast()} test against the last \code{test.h} points of \code{y.in} after
#' any \code{val.holdout.len} points are removed from the end of \code{y.in}. The best performing set of
#' parameters based on MAPE over over the forecast horizon of \code{test.h} points are returned as part of a list
#' alongside the 'optimum' weighted similarity matrix \code{Sw.opt}, the \code{Grid} of tested sets, and the MAPE
#' results. MAPE formula used is: \code{abs((test.actuals - test.forecast.i)/test.actuals)*100}. Where \code{test.forecast.i}
#' and \code{test.actuals} are both numeric vectors.
#'
#'
#' @param grid.len numeric value representing the number of hyperparameter sets to generate and test
#' @param St.in numeric matrix of Similarities, recommend generating with \code{StMatrixCalc()}
#' @param Sp.in numeric matrix of Similarities, recommend generating with \code{SpMatrixCalc()}
#' @param Sx.in numeric matrix of Similarities, recommend generating with \code{SxMatrixCalc()}
#' @param y.in numeric vector of the series to be forecast
#' @param test.h numeric value representing the number of points in the test forecast horizon
#' @param max.k numeric value representing the maximum value of k, \code{knn.forecast()} should use, will be set to \code{min(floor((length(y.in))*.4),50)} if \code{NA} is passed
#' @param val.holdout.len numeric value representing the number of observations at the end of the series to be removed in testing foreacst if desired to leave a validation set after tuning
#'
#' @return a list of the following components
#' \describe{
#'           \item{weight.opt}{numeric vector of the 3 weights to generate \code{Sw.opt} in alpha, beta, gamma order which achieved the best performance in terms of MAPE}
#'           \item{k.opt}{numeric value of neighbors used in \code{knn.forecast()} which achieved the best performance in terms of MAPE}
#'           \item{Test.MAPE}{numeric value of the MAPE result for the optimum hyperparamter set achieved on the test points}
#'           \item{MAPE.all}{numeric vector of MAPE results, each observation corresponds to the row in \code{Grid} of the same index}
#'           \item{Grid}{dataframe of all hyperparameter sets tested in the tuning}
#' }
#'
#'
#'
#' @export
#'
#' @examples
#' data("simulation_master_list")
#' series.index <- 15
#' ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x
#'
#' df <- data.frame(ex.series)
#' #Generate vector of time orders
#' df$t <- c(1:nrow(df))
#' #Generate vector of periods
#' nperiods <- simulation_master_list[[series.index]]$seasonal.periods
#' df$p <- rep(1:nperiods,length.out=nrow(df))
#' #Pull corresponding exogenous predictor(s)
#' X <- as.matrix(simulation_master_list[[series.index]]$x.chng)
#'
#' St.ex <- StMatrixCalc(df$t)
#' Sp.ex <- SpMatrixCalc(df$p,nPeriods=nperiods)
#' Sx.ex <- SxMatrixCalc(X)
#'
#' tuning.test <- knn.forecast.randomsearch.tuning(grid.len=10
#' ,y.in = ex.series
#' ,St.in = St.ex
#' ,Sp.in = Sp.ex
#' ,Sx.in = Sx.ex
#' ,test.h = 3
#' ,max.k = 10
#' ,val.holdout.len=3)
knn.forecast.randomsearch.tuning <- function(grid.len = 100
                                             ,St.in
                                             ,Sp.in
                                             ,Sx.in
                                             ,y.in
                                             ,test.h=1
                                             ,max.k =NA
                                             ,val.holdout.len=0  ) {

  n<- length(y.in)

  #Ensure integer inputs are in fact integers
  grid.len <- floor(grid.len)
  max.k <- floor(max.k)
  test.h <- floor(test.h)
  val.holdout.len <- floor(val.holdout.len)


  #Set a maximum for k
  if (is.na(max.k)){
    k.cap <- min(floor((length(y.in))*.4),50)
  } else {
    k.cap <- max.k
  }

  #Randomly propose sets of hyperparameters
  ks <- base::sample(1:k.cap,size=grid.len,replace=T)
  alpha.0 <- stats::runif(grid.len,min=.001,max=10000)
  beta.0<- stats::runif(grid.len,min=.001,max=10000)
  gamma.0<- stats::runif(grid.len,min=.001,max=10000)

  Theta <- data.frame(ks,alpha.0,beta.0,gamma.0)

  #Normalize weights to sum to 1
  Theta$alpha <- Theta$alpha.0/(Theta$alpha.0 + Theta$beta.0 + Theta$gamma.0)
  Theta$beta <- Theta$beta.0/(Theta$alpha.0 + Theta$beta.0 + Theta$gamma.0)
  Theta$gamma <- Theta$gamma.0/(Theta$alpha.0 + Theta$beta.0 + Theta$gamma.0)


  #Remove any holdout points from y
  if (val.holdout.len > 0){
    val.remove <- as.vector(c((n-val.holdout.len+1):(n)))
    y.in <- y.in[-(val.remove)]
  }

  #Create the test index based on test.h argument, pull out actuals for comparison
  test.index <- c((length(y.in)-test.h+1):length(y.in))
  test.actuals <- y.in[test.index]

  #Set matrix to capture APE
  APE.h.i <- matrix(data=NA,nrow=nrow(Theta),ncol=length(test.index))

  #Forecast loop to calculate test index APE for all Theta rows
  for (i in c(1:nrow(Theta))){

    k.theta.i <- Theta[i,c('ks')]
    alpha.theta.i <- Theta[i,c('alpha')]
    beta.theta.i <- Theta[i,c('beta')]
    gamma.theta.i <- Theta[i,c('gamma')]

    #Calculate the Sw matrix for the set of weights
    Sw.theta.i <- alpha.theta.i*St.in + beta.theta.i*Sp.in + gamma.theta.i*Sx.in

    #Remove holdout points from Sw
    if (val.holdout.len > 0){
      Sw.theta.i <- Sw.theta.i[-(val.remove),-(val.remove)]
    }

    #Forecast using Sw.theta.i and compare to actuals
    test.forecast.i <- knn.forecast(Sim.Mat.in = Sw.theta.i,f.index.in = test.index,k.in = k.theta.i,y.in=y.in )
    APE.h.i[i,]  <- abs((test.actuals - test.forecast.i)/test.actuals)*100

  }


  MAPE.h.i <- rowMeans(APE.h.i)

  #Pull results for best performance in terms of MAPE over the forecast horizon
  Test.MAPE.opt <- MAPE.h.i[which.min(MAPE.h.i)]

  k.opt <-Theta[which.min(MAPE.h.i ),c('ks')]
  alpha.opt <- Theta[which.min(MAPE.h.i ),c('alpha')]
  beta.opt <- Theta[which.min(MAPE.h.i ),c('beta')]
  gamma.opt <- Theta[which.min(MAPE.h.i ),c('gamma')]

  weight.opt <- c(alpha.opt,beta.opt,gamma.opt)

  Sw.opt <- alpha.opt*St.in + beta.opt*Sp.in + gamma.opt*Sx.in


  #Compile results for final list
  return.list <- list(weight.opt=weight.opt
                      ,k.opt=k.opt
                      ,Sw.opt=Sw.opt
                      ,Test.MAPE.opt=Test.MAPE.opt
                      ,MAPE.all = MAPE.h.i
                      ,Grid = Theta[,c('ks','alpha','beta','gamma')])

  return(return.list)

}
