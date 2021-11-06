
<!-- README.md is generated from README.Rmd. Please edit that file -->

# knnwtsim

<!-- badges: start -->
<!-- badges: end -->

The goal of knnwtsim is to provide a package to share and implement a
project I have been working on around the use of KNN forecasting in
situations where the response series of interest can be predicted by a
combination of its’ own recent realizations, its own periodic patterns,
and by the values of one or multiple exogenous predictors.

The package and project are focused primarily into two components. The
first being calculation of a similarity measure which takes into account
all three factors listed above (*S*<sub>*w*</sub>). And the second being
the usage of this measure to identify neighbors and perform KNN
regression using the similarity measure.

## Installation

You can install the development version of knnwtsim from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mtrupiano1/knnwtsim")
```

## Example

This is a basic example which shows a full forecasting workflow:

``` r
library(knnwtsim)
## basic example code
data("simulation_master_list")
series.index <- 15
ex.series <- simulation_master_list[[series.index]]$series.lin.coef.chng.x

#Weights pre tuned by random search. In alpha, beta, gamma order   
pre.tuned.wts <- c(0.2788147,0.7209188,0.0002665336)
pre.tuned.k <- 11 

df <- data.frame(ex.series)
#Generate vector of time orders
df$t <- c(1:nrow(df))

#Generate vector of periods
nperiods <- simulation_master_list[[series.index]]$seasonal.periods
df$p <- rep(1:nperiods,length.out=nrow(df))

#Pull corresponding exogenous predictor(s)
X <- as.matrix(simulation_master_list[[series.index]]$x.chng)
XdistMetric <- 'euclidean'

#Number of points to set aside for validation
val.len <- ifelse(nperiods==12,nperiods,nperiods*2)

#Calculate the weighted similarity matrix Sw 
Sw.ex <- SwMatrixCalc(#For the recency similarity St
                      t.in = df$t
                      #For the periodic similarity Sp
                      ,p.in=df$p, nPeriods.in = nperiods
                      #For the exogenous similarity Sx
                      ,X.in=X, XdistMetric.in = XdistMetric
                      #Weights to be applied to each similarity
                      ,weights=pre.tuned.wts)

#View the top corner of the weighted similarity matrix Sw 
print(Sw.ex[1:5,1:5])
#>           1         2         3         4         5
#> 1 1.0000000 0.5000444 0.3333827 0.2500869 0.2362313
#> 2 0.5000444 1.0000000 0.5000533 0.3334602 0.2501260
#> 3 0.3333827 0.5000533 1.0000000 0.5000901 0.3333916
#> 4 0.2500869 0.3334602 0.5000901 1.0000000 0.5000314
#> 5 0.2362313 0.2501260 0.3333916 0.5000314 1.0000000
```

``` r
#Index we want to forecast 
val.index <- c((length(ex.series) - val.len + 1):length(ex.series))

#Generate the forecast 
knn.frcst <- knn.forecast(Sim.Mat.in = Sw.ex
                          ,f.index.in = val.index
                          ,k.in=pre.tuned.k
                          ,y.in=ex.series)

ts.plot(ex.series,ylab="Simulated Time Series Value")
lines(x=val.index,y=knn.frcst,col='red',lty=2)
legend('bottomleft',legend=c('actuals','KNN Forecast'),col=c('black','red'),lty=c(1,2))
```

<img src="man/figures/README-knn-forecast-example-1.png" width="100%" />
