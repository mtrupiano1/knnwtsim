SeasonalAbsDistance <- function(p1,p2,nPeriods){
  minPeriod <- 1

  DirectDis <- abs(p1-p2)
  AroundDis <- abs(min(p1,p2) - minPeriod) + abs(nPeriods  - max(p1,p2)) + 1

  Dp <- min(DirectDis,AroundDis)

  return(Dp)
}


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



TempAbsDistance <- function(p1,p2){
  Dt <- abs(p1-p2)
  return(Dt)
}

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
