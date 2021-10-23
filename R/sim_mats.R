#' Seasonal Absolute Dissimilarity
#'
#'Calculate Seasonal Dissimilarity measure between the respective seasonal period
#'two points, given the number of periods in one full seasonal cycle
#'
#' @param p1 numeric
#' @param p2 numeric
#' @param nPeriods numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#' SeasonalAbsDistance(1,4,4)
SeasonalAbsDistance <- function(p1,p2,nPeriods){
  minPeriod <- 1

  DirectDis <- abs(p1-p2)
  AroundDis <- abs(min(p1,p2) - minPeriod) + abs(nPeriods  - max(p1,p2)) + 1

  Dp <- min(DirectDis,AroundDis)

  return(Dp)
}


#' Calculate Seasonal Similarity Matrix
#'
#' Generates and returns an nxn matrix by calculating the Seasonal Dissimilarity
#' (see \code{SeasonalAbsDistance()}) for each possible pair of points in a vector
#' of seasonal periods, then converts Dissimilarity matrix to a Similarity matrix using 1/(D_p +1)
#'
#' @param v numeric vector
#' @param nPeriods numeric
#'
#' @return matrix
#' @export
#'
#' @examples
#' SpMatrixCalc(c(1,2,3),3)
#'
SpMatrixCalc <- function(v,nPeriods){
  n <- length(v)
  dpmat <- matrix(NA,nrow=n,ncol=n)

  for (i in c(1:n)) {
    p1 <- v[i]
    for (j in c(1:n)){
      p2 <- v[j]
      dpmat[i,j] <- SeasonalAbsDistance(p1,p2,nPeriods)
    }
  }
  spmat <- 1/(1+dpmat)
  return(spmat)
}



#' Temporal Absolute Dissimilarity
#'
#' Simply takes the absolute difference between two points, meaning points close
#' in time will have smaller dissimilarity. This is equivalent to Euclidean Distance
#'
#' @param p1 numeric
#' @param p2 numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#' TempAbsDistance(1,3)
TempAbsDistance <- function(p1,p2){
  Dt <- abs(p1-p2)
  return(Dt)
}


#' Calculate Temporal Similarity Matrix
#'
#' Generates and returns an nxn matrix by calculating the absolute difference
#' (see \code{TempAbsDistance()}) for each possible pair of points in a vector
#' of the time order of each point in a series,
#' then converts Dissimilarity matrix to a Similarity matrix using 1/(D_t +1)
#'
#' @param v numeric vector
#'
#' @return matrix
#' @export
#'
#' @examples
#' StMatrixCalc(c(1,2,3))
#'
StMatrixCalc <- function(v){
  n <- length(v)
  dtmat <- matrix(NA,nrow=n,ncol=n)

  for (i in c(1:n)) {
    p1 <- v[i]
    for (j in c(1:n)){
      p2 <- v[j]
      dtmat[i,j] <- TempAbsDistance(p1,p2)
    }
  }
  stmat <- 1/(1+dtmat)
  return(stmat)
}



#' Calculate Similarity Matrix for Exogenous Predictors
#'
#' Largely a wrapper function for base R's \code{stats::dist()} function. First calculates
#' nxn distance matrix using specified method for an input matrix or vector using
#'  \code{stats::dist()}. Then converts the distance matrix to Similarity matrix using 1/(D_x +1)
#'
#' @param A matrix
#' @param XdistMetric character
#'
#' @return matrix
#' @export
#'
#' @examples
#' X <- matrix(c(1,1,1,2,2,2,3,3,3),nrow=3,ncol=3,byrow=TRUE)
#' SxMatrixCalc(X,'euclidean')
SxMatrixCalc <- function(A,XdistMetric='euclidean'){
  dxmat <- as.matrix(stats::dist(A,method=XdistMetric ,upper=TRUE,diag=TRUE))
  sxmat <- 1/(1+dxmat)
  return(sxmat)
}



#' Calculate Weighted Similarity Matrix
#'
#' @param t.in numeric vector of time orders for points in the response series
#' @param p.in numeric vector of period within a seasonal cycle (ex. 1 for January points in monthly data)
#' @param nPeriods.in numeric scalar indicating the maximum value \code{p.in} could take on (ex. 12 for monthly data)
#' @param X.in numeric vector or matrix of exogenous predictors, where the rows correspond to points in the response series
#' @param XdistMetric.in Distance calculation method to be used by \code{stats::dist()}
#' @param weights numeric vector where first value represents weight for \code{S_t}, second value the weight for \code{S_p}, and the third value the weight for \code{S_x}
#'
#' @return numeric matrix which is the weighted similarity matrix \code{S_w}
#' @export
#'
#' @examples
#' t <- c(1,2,3)
#' p <- c(1,2,1)
#' X <- matrix(c(1,1,1,2,2,2,3,3,3),nrow=3,ncol=3,byrow=TRUE)
#' SwMatrixCalc(t.in=t,p.in=p,nPeriods.in = 2,X.in=X
#'              ,XdistMetric.in = "euclidean",weights=c(1/4,1/4,1/2))
SwMatrixCalc<- function(t.in,p.in,nPeriods.in,X.in,XdistMetric.in='euclidean',weights=c(1/3,1/3,1/3)){
  alpha <- weights[1]
  beta <- weights[2]
  gamma <- weights[3]

  St <- StMatrixCalc(v=t.in)

  Sp <- SpMatrixCalc(v=p.in,nPeriods=nPeriods.in)

  Sx <- SxMatrixCalc(A=X.in,XdistMetric=XdistMetric.in)

  Sw <- alpha*St + beta*Sp + gamma*Sx

  return(Sw)

}






