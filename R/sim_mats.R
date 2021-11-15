#' Seasonal Absolute Dissimilarity
#'
#'Calculate Seasonal Dissimilarity measure between the respective seasonal period
#'two points, given the number of periods in one full seasonal cycle.
#'
#' @param p1 numeric value representing a seasonal period
#' @param p2 numeric value representing a seasonal period
#' @param nPeriods numeric value representing the maximum value \code{p1} or \code{p2} can take on
#'
#' @return numeric value of the Seasonality Dissimilarity between \code{p1} and \code{p2}
#' @export
#'
#' @examples
#' SeasonalAbsDissimilarity(1,4,4)
SeasonalAbsDissimilarity <- function(p1,p2,nPeriods){
  minPeriod <- 1

  DirectDis <- abs(p1-p2)
  AroundDis <- abs(min(p1,p2) - minPeriod) + abs(nPeriods  - max(p1,p2)) + 1

  Dp <- min(DirectDis,AroundDis)

  return(Dp)
}


#' Calculate Seasonal Similarity Matrix
#'
#' Generates and returns an nxn matrix by calculating the Seasonal Dissimilarity
#' (see \code{SeasonalAbsDissimilarity()}) for each possible pair of points in a vector
#' of seasonal periods, then converts Dissimilarity matrix to a Similarity matrix using 1/(D_p +1).
#'
#' @param v numeric vector with the seasonal periods corresponding to each point in the response series
#' @param nPeriods numeric value representing the maximum value \code{v} can take on
#'
#' @return numeric matrix of Seasonal Similarities for the vector \code{v}
#' @export
#'
#' @examples
#' SpMatrixCalc(c(1,2,4),4)
#'
SpMatrixCalc <- function(v,nPeriods){
  n <- length(v)
  dpmat <- matrix(NA,nrow=n,ncol=n)

  for (i in c(1:n)) {
    p1 <- v[i]
    for (j in c(1:n)){
      p2 <- v[j]
      dpmat[i,j] <- SeasonalAbsDissimilarity(p1,p2,nPeriods)
    }
  }
  spmat <- 1/(1+dpmat)
  return(spmat)
}



#' Temporal Absolute Dissimilarity
#'
#' Simply takes the absolute difference between two points, meaning points close
#' in time will have smaller dissimilarity. This is equivalent to Euclidean Distance.
#'
#' @param p1 numeric value representing the time order of the point in the response series
#' @param p2 numeric value representing the time order of the point in the response series
#'
#' @return numeric value of the absolute difference between \code{p1} and \code{p2}
#' @export
#'
#' @examples
#' TempAbsDissimilarity(1,3)
TempAbsDissimilarity <- function(p1,p2){
  Dt <- abs(p1-p2)
  return(Dt)
}


#' Calculate Temporal Similarity Matrix
#'
#' Generates and returns an nxn matrix by calculating the absolute difference
#' (see \code{TempAbsDissimilarity()}) for each possible pair of points in a vector
#' of the time order of each point in a series,
#' then converts Dissimilarity matrix to a Similarity matrix using 1/(D_t +1).
#'
#' @param v numeric vector with the time order corresponding to each point in the response series
#'
#' @return numeric matrix of Temporal Similarities for the vector \code{v}
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
      dtmat[i,j] <- TempAbsDissimilarity(p1,p2)
    }
  }
  stmat <- 1/(1+dtmat)
  return(stmat)
}



#' Calculate Similarity Matrix for Exogenous Predictors
#'
#' Largely a wrapper function for the \code{stats::dist()} function. First calculates
#' nxn distance matrix using specified method for an input matrix or vector using
#'  \code{stats::dist()}. Then converts the distance matrix to Similarity matrix using 1/(D_x +1).
#'
#' @param A numeric matrix or numeric vector where the columns represents exogenous predictor variables and the rows correspond to the points in the respond series
#' @param XdistMetric character describing the method \code{stats::dist()} should use. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'
#' @return numeric matrix of distances for \code{A}
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
#' A wrapper function which calls each of \code{StMatrixCalc(v=t.in)}, \code{SpMatrixCalc(v=p.in,nPeriods=nPeriods.in)},
#' and \code{SxMatrixCalc(A=X.in,XdistMetric=XdistMetric.in)} to generate the three component matrices of \code{S_w}. Then
#' generates the final weighted similarity matrix as the sum of each component matrix multiplied by its corresponding \code{weights}.
#' The first value in \code{weights} will be multiplied by \code{S_t}, the second \code{S_p}, and the third \code{S_x}.
#'
#'
#' @param t.in numeric vector of time orders for points in the response series
#' @param p.in numeric vector of period within a seasonal cycle (ex. 1 for January points in monthly data)
#' @param nPeriods.in numeric scalar indicating the maximum value \code{p.in} could take on (ex. 12 for monthly data)
#' @param X.in numeric vector or matrix of exogenous predictors, where the rows correspond to points in the response series
#' @param XdistMetric.in character describing the method \code{stats::dist()} should use. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
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






