context("Tests on the various functions for generating similarities in sim_mats.R")
library(stats)

test_that("SeasonalAbsDissimilarity works when around and direct diss are needed", {
  px <- 1
  py <- 6
  pz <- 3
  np <- 7

  #Test of around diss
  expect_equal(SeasonalAbsDissimilarity(px,py,np), 2)

  #Test of direct diss
  expect_equal(SeasonalAbsDissimilarity(px,pz,np), 2)
})


test_that("SpMatrixCalc matches hand calculation", {
  vp <- c(1,2,5)
  np <- 5

  #diag should be 1
  #1-2 dist is 1, sim is 1/(1+1)=1/2
  #1-5 dist is 1, sim is 1/(1+1)= 1/2
  #2-5 dist is 2, sim is 1/(2+1)= 1/3
  sp.check <- matrix(c(1,.5,.5,.5,1,1/3,.5,1/3,1),nrow=3,ncol=3,byrow=TRUE)


  expect_equal(SpMatrixCalc(vp,np), sp.check)

})


test_that("TempAbsDissimilarity correctly takes absolute difference", {
  px <- 3
  py <- 6
  pz <- 1

  #Test of neg diff
  expect_equal(TempAbsDissimilarity(px,py), 3)

  #Test of pos diff
  expect_equal(TempAbsDissimilarity(px,pz), 2)

})



test_that("StMatrixCalc matches hand calculation", {
  vt <- c(1,2,3)

  #diag should be 1
  #1-2 dist is 1, sim is 1/(1+1)=1/2
  #1-3 dist is 2, sim is 1/(2+1)= 1/3
  #2-3 dist is 1, sim is 1/(1+1)= 1/2
  st.check <- matrix(c(1,.5,1/3,.5,1,.5,1/3,.5,1),nrow=3,ncol=3,byrow=TRUE)

  expect_equal(StMatrixCalc(vt),st.check)

})


test_that("SxMatrixCalc wrapper matches direct call to dist() for vector", {

  x.v <- c(1.2,8.3,4.7)

  #Canberra distance to try a different method
  dxmat.can <- as.matrix(stats::dist(x.v,method='canberra' ,upper=TRUE,diag=TRUE))
  sxmat.can <- 1/(1+dxmat.can)

  #Default which should be Euclidean
  dxmat.euc <- as.matrix(stats::dist(x.v ,upper=TRUE,diag=TRUE))
  sxmat.euc <- 1/(1+dxmat.euc)

  #Test that non default method works
  expect_equal(SxMatrixCalc(x.v,XdistMetric = 'canberra') ,sxmat.can)

  #Test that default method works
  expect_equal(SxMatrixCalc(x.v) ,sxmat.euc)

})

test_that("SxMatrixCalc wrapper matches direct call to dist() for matrix", {

  x.mat <- matrix(c(1.2,8.3,4.7,12,1.6,6.7,7.8,3,7),nrow=3,ncol=3)


  #Canberra distance to try a different method
  dxmat.can <- as.matrix(stats::dist(x.mat,method='canberra' ,upper=TRUE,diag=TRUE))
  sxmat.can <- 1/(1+dxmat.can)

  #Default which should be Euclidean
  dxmat.euc <- as.matrix(stats::dist(x.mat ,upper=TRUE,diag=TRUE))
  sxmat.euc <- 1/(1+dxmat.euc)

  #Test that non default method works
  expect_equal(SxMatrixCalc(x.mat,XdistMetric = 'canberra') ,sxmat.can)

  #Test that default method works
  expect_equal(SxMatrixCalc(x.mat) ,sxmat.euc)

})



test_that("SwMatrixCalc wrapper matches individual calls", {
  vt <- c(1,2,3)
  vp <- c(1,2,5)
  np <- 5
  x.v <- c(1.2,8.3,4.7)

  a<-.4
  b<-.25
  c<-.35

  w.v <- c(a,b,c)

  st.check <- StMatrixCalc(vt)
  sp.check <- SpMatrixCalc(vp,np)
  sx.check <- SxMatrixCalc(x.v,XdistMetric = 'canberra')
  sw.check <- a*st.check + b*sp.check + c*sx.check


  #Test that non default method works
  expect_equal(SwMatrixCalc(t.in=vt,p.in=vp,nPeriods.in=np,X.in=x.v
                            ,XdistMetric.in='canberra',weights=w.v) ,sw.check)

})

