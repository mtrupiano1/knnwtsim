% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn_forecast.R
\name{knn.forecast}
\alias{knn.forecast}
\title{KNN Forecast}
\usage{
knn.forecast(Sim.Mat.in, f.index.in, k.in, y.in)
}
\arguments{
\item{Sim.Mat.in}{numeric and symmetric matrix of similarities (recommend use of \code{S_w}, see \code{SwMatrixCalc()}).}

\item{f.index.in}{numeric vector indicating the indices of \code{Sim.Mat.in} and \code{y.in} which correspond to the time order of the points to be forecast.}

\item{k.in}{integer value indicating the the number of nearest neighbors to be considered in forecasting, must be \code{>= 1}.}

\item{y.in}{numeric vector of the response series to be forecast.}
}
\value{
numeric vector of the same length as \code{f.index.in}, of forecasted observations.
}
\description{
Provide an n x n similarity matrix as input, all points both observed and those to be
forecasted should be included. The \code{f.index.in} argument indicates which observations
to identify neighbors for, and removes them from consideration as eligible neighbors.
Once the matrix is subset down to only the columns in \code{f.index.in} and the rows excluding \code{f.index.in}, the
\code{NNreg()} function is applied over the columns, returning for each column the mean of those points in \code{y.in} identified as neighbors
based on the row index of the \code{k.in} most similar observations in the column. It is important that the index of the similarity matrix
and \code{y.in} accurately reflect the time order of the observations.
}
\examples{
Sim.Mat <- matrix(c(1, .5, .2, .5, 1, .7, .2, .7, 1),
  nrow = 3, ncol = 3, byrow = TRUE
)
y <- c(2, 1, 5)
f.index <- c(3)
k <- 2
knn.forecast(Sim.Mat.in = Sim.Mat, f.index.in = f.index, y.in = y, k.in = k)
}
\seealso{
\itemize{
\item \code{\link[=NNreg]{NNreg()}} for the function used to perform knn regression on a single
point.
\item \code{\link[=SwMatrixCalc]{SwMatrixCalc()}} for the function to calculate a matrix with the recommended similarity measure.
}
}
