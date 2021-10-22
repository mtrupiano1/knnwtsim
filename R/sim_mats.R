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
