## code to prepare `simulation_master_list` dataset goes here

#Your path here
data.dir <- ''

library(stats)
library(MASS)
library(clusterGeneration)

simulation_master_list <- NULL

#Defining some functions to use in the loop
lin.to.sqrt <- function(x,break.point,coef){
  if (x < break.point) {
    out <- coef*x
  } else {
    out <- sqrt(x)
  }
  return(out)
}

quad.to.cubic <- function(x,break.point,coef){
  if (x < break.point) {
    out <- coef*(x**2)
  } else {
    out <- -coef*(x**3)
  }
  return(out)
}

lin.coef.change <- function(x,break.point,coef1,coef2){
  if (x < break.point) {
    out <- coef1*x
  } else {
    out <- coef2*x
  }
  return(out)
}


ar2.sample <- function() {
  success <- FALSE
  while (!success) {
    # Produce random ar2 coefficients
    ar1 <- stats::runif(1,min=-1,max=1)
    ar2 <- stats::runif(1,min=-1,max=1)
    ar.v <- c(ar1,ar2)
    # check that they meet constraints
    success <- ((ar1 + ar2 < 1) & (ar2 - ar1 < 1))
  }
  return(ar.v)
}

ma2.sample <- function() {
  success <- FALSE
  while (!success) {
    # Produce random ma2 coefficients
    ma1 <- stats::runif(1,min=-1,max=1)
    ma2 <- stats::runif(1,min=-1,max=1)
    ma.v <- c(ma1,ma2)
    # check that they meet constraints
    success <- ((ma1 + ma2 > -1) & (ma1 - ma2 < 1))
  }
  return(ma.v)
}

#=============================================
# Start Loop
#===========================================


seed <- c(10:19)
len <- c(50,100)

seed.len.grid <- expand.grid(seed,len)
names(seed.len.grid) <- c('seed','len')

for (i in (1:nrow(seed.len.grid))) {
  #i<-1
  seed.i <- seed.len.grid[i,1]
  len.i <- seed.len.grid[i,2]

  set.seed(seed.i)


  #===========================================
  #   ARIMA Components
  #   Randomized: p,d,q,ar.list,ma.list
  #============================================


  p <- sample(c(0:2),1)
  d <- sample(c(0:2),1)
  q <- sample(c(0:2),1)

  if (p == 0){
    ar.list <- NULL
  } else if (p==1){
    ar.list <- stats::runif(1,min=-1,max=1)
  } else {
    ar.list <-  ar2.sample()
  }

  if (q == 0){
    ma.list <- NULL
  } else if (q==1){
    ma.list <- stats::runif(1,min=-1,max=1)
  } else {
    ma.list <-  ma2.sample()
  }


  ts.sim <- stats::arima.sim(list(order = c(p,d,q), ar = ar.list,ma=ma.list), n = len.i,n.start =100)
  #arima.sim sometimes produces series of length len.i + 1
  ts.sim <- ts.sim[1:len.i]

  #=====================================================
  # Seasonal Component
  #=====================================================

  #Simulate seasonality
  t <- c(1:len.i)
  s <- sample(c(4,7,12),size=1)
  sin.term <- sin((2*pi*t)/s)
  cos.term <- cos((2*pi*t)/s)

  beta.sin <- stats::runif(1,min=-5,max=5)
  beta.cos <- stats::runif(1,min=-5,max=5)


  #==========================================
  # Simulate matrix of predictors and coefficients
  #============================================

  x.vars <-sample(c(1:10),size=1)
  mu.vec <- rep(0,times=x.vars)


  Sigma.mat <- clusterGeneration::rcorrmatrix(d=x.vars,alphad = 1) #alphad is used in determining parameters of a beta dist

  X.sim <- MASS::mvrnorm(n=len.i,mu=mu.vec,Sigma=Sigma.mat)

  b.x <- stats::runif(n=ncol(X.sim),min=-5,max=5)


  #==========================================================
  #  Piecewise Functional Relationships
  #=========================================================

  #simulate x
  mu.x <- stats::runif(n=1,min=-5,max=5)
  sd.x <- stats::runif(n=1,min=.001,max=10)
  x.chng.sim <- stats::rnorm(n=len.i,mean=mu.x,sd=sd.x)

  #simulate two coefficients for the different functional relationship functions, and break point where relationships change
  coef1.x <- stats::runif(n=1,min=-5,max=5)
  coef2.x <- stats::runif(n=1,min=-5,max=5)
  #Allows some variability but can be very confident in having observations on either side of breakpoint
  break.point.x <- stats::runif(n=1,min=mu.x-sd.x,max=mu.x+sd.x)
  #Make sure sqrt breakpoint is postive
  sqrt.break.point.x <- max(.001,break.point.x)


  lin.to.sqrt.vec <- sapply(x.chng.sim ,lin.to.sqrt,coef=coef1.x,break.point=sqrt.break.point.x)
  quad.to.cubic.vec <- sapply(x.chng.sim ,quad.to.cubic,coef=coef1.x,break.point=break.point.x)
  lin.coef.chng.vec <- sapply(x.chng.sim ,lin.coef.change,coef1=coef1.x,coef2=coef2.x,break.point=break.point.x)


  #=======================================================
  # Additional noise, and constant
  #===================================================

  error.choice <- ifelse(sample(c(1,2),size=1) == 1,'poisson','normal')
  pois.rate <-runif(1,min=.1,max=20)
  norm.sd <- runif(1,min=.1,max=20)

  if (error.choice=='poisson'){
    e.sim <- stats::rpois(len.i,pois.rate)
  } else {
    e.sim <- stats::rnorm(len.i,0,norm.sd)
  }


  constant <- stats::runif(n=1,min=-20,max=20)

  y.sim.mv.normx <-  constant +
    X.sim%*%b.x +
    beta.sin*sin.term +
    beta.cos*cos.term +
    ts.sim +
    e.sim

  y.sim.lin.to.sqrt.x <-  constant +
    lin.to.sqrt.vec +
    beta.sin*sin.term +
    beta.cos*cos.term +
    ts.sim +
    e.sim

  y.sim.lin.coef.chng.x <-  constant +
    lin.coef.chng.vec +
    beta.sin*sin.term +
    beta.cos*cos.term +
    ts.sim +
    e.sim

  y.sim.quad.to.cubic.x <-  constant +
    quad.to.cubic.vec +
    beta.sin*sin.term +
    beta.cos*cos.term +
    ts.sim +
    e.sim

  #See Series appearance
  ts.plot(y.sim.mv.normx)
  ts.plot(y.sim.lin.to.sqrt.x)
  ts.plot(y.sim.lin.coef.chng.x)
  ts.plot(y.sim.quad.to.cubic.x)


  #Append Series details to list
  row <- list(series.len = len.i, random.seed = seed
              ,arima.p=p,arima.d=d,arima.q=q
              ,ar.coefficients = ar.list,ma.coefficients = ma.list
              ,seasonal.periods=s,sin.coef=beta.sin,cos.coef=beta.cos
              ,X.cols = x.vars,X.mu= mu.vec,X.Sigma = Sigma.mat,X =X.sim,x.coef = b.x
              ,x.chng.mu=mu.x,x.chng.sd = sd.x ,x.chng.coef1 = coef1.x
              , x.chng.coef1 = coef1.x, x.chng.break.point =break.point.x ,x.chng.break.point.sqrt =sqrt.break.point.x
              ,x.chng = x.chng.sim,type.noise = error.choice,poisson.rate = pois.rate, normal.sd = norm.sd
              ,noise = e.sim , constant = constant, series.mvnormx=y.sim.mv.normx
              ,series.lin.to.sqrt.x = y.sim.lin.to.sqrt.x, series.lin.coef.chng.x = y.sim.lin.coef.chng.x
              ,series.quad.to.cubic.x = y.sim.quad.to.cubic.x)

  simulation_master_list[[i]] <- row

}



save(simulation_master_list,file=paste0(data.dir,'simulation_master_list.RData'))

#usethis::use_data(simulation_master_list, overwrite = TRUE)

