

#' KNN Forecast
#'
#' Provide an n x n similarity matrix as input, all points both observed and those to be
#' forecasted should be included. The \code{f.index.in} argument indicates which observations
#' to identify neighbors for, and removes them from consideration as eligible neighbors.
#' Once the matrix is subset down to only the columns in \code{f.index.in} and the rows excluding \code{f.index.in}, the
#' \code{NNreg()} function is applied over the columns, returning for each column the mean of those points in \code{y.in} identified as neighbors
#' based on the row index of the \code{k.in} most similar observations in the column. It is important that the index of the similarity matrix
#' and \code{y.in} accurately reflect the time order of the observations.
#'
#' @param Sim.Mat.in numeric and symmetric matrix of similarities (recommend use of \code{S_w}, see \code{SwMatrixCalc()}).
#' @param f.index.in numeric vector indicating the indices of \code{Sim.Mat.in} and \code{y.in} which correspond to the time order of the points to be forecast.
#' @param k.in integer value indicating the the number of nearest neighbors to be considered in forecasting, must be \code{>= 1}.
#' @param y.in numeric vector of the response series to be forecast.
#'
#' @return numeric vector of the same length as \code{f.index.in}, of forecasted observations.
#' @export
#'
#' @seealso
#' * [NNreg()] for the function used to perform knn regression on a single
#' point.
#' * [SwMatrixCalc()] for the function to calculate a matrix with the recommended similarity measure.
#' @md
#'
#' @examples
#' Sim.Mat <- matrix(c(1, .5, .2, .5, 1, .7, .2, .7, 1),
#'   nrow = 3, ncol = 3, byrow = TRUE
#' )
#' y <- c(2, 1, 5)
#' f.index <- c(3)
#' k <- 2
#' knn.forecast(Sim.Mat.in = Sim.Mat, f.index.in = f.index, y.in = y, k.in = k)
knn.forecast <- function(Sim.Mat.in, f.index.in, k.in, y.in) {

  # argument type error checks
  if (!(is.vector(y.in, mode = "numeric")) |
    !(is.vector(f.index.in, mode = "numeric"))) {
    stop("y.in and f.index.in should be numeric vectors")
  }

  if (!((is.matrix(Sim.Mat.in) & is.numeric(Sim.Mat.in)))) {
    stop("Sim.Mat.in should be a numeric matrix")
  } else if (!(isSymmetric.matrix(Sim.Mat.in))) {
    stop("Sim.Mat.in should be a symmetric matrix")
  }

  if ((!(is.vector(k.in, mode = "numeric"))) |
    (!(identical(length(k.in), 1L)))) {
    stop("k.in should be an integer with length 1L")
  } else if (!(identical((k.in %% 1), 0))) {
    warning("k.in should be an integer,
            argument will be floored to nearest whole number")
    k.in <- floor(k.in)
  }

  # dimension conflict error checks
  if (max(f.index.in) < nrow(Sim.Mat.in)) {
    warning("Sim.Mat.in row count is greater than the maximum value of
            f.index.in, rows and columns at indices greater than maximum value
            of f.index.in will be removed")
    remove.indices <- c((max(f.index.in) + 1):nrow(Sim.Mat.in))
    Sim.Mat.in <- Sim.Mat.in[-(remove.indices), -(remove.indices)]
  }

  # error workaround when k.in = 1 in knn.forecast.boot.intervals
  if (isTRUE(k.in > nrow(Sim.Mat.in[-(f.index.in), ]))) {
    stop("k.in is larger than the number of eligible neighbors")
  }

  # check that value of k.in is reasonable
  if (k.in < 1) {
    stop("k.in cannot be < 1")
  }

  # subset similarity matrix to only rows with eligible neighbors, and columns
  # which need an estimate
  Sim.Mat.Eligible <- as.matrix(Sim.Mat.in[-(f.index.in), f.index.in])

  # apply NNreg over columns
  Y.hat <- apply(Sim.Mat.Eligible,
    MARGIN = 2,
    FUN = NNreg, k.in2 = k.in, y.in2 = y.in
  )
  return(Y.hat)
}



#' Estimate a Single Point with K Nearest Neighbors Regression
#'
#' Finds the index of the nearest neighbors for a single point given that point's
#' vector of similarities to all observations eligible to be considered as neighbors. The \code{k.in2}
#' neighbors are identified by their index in the similarity vector, and this index is used to
#' identify the neighbor points in \code{y.in2}. The function then returns the mean of the values
#' in \code{y.in2} identified as neighbors. It is suggested to call this function through \code{knn.forecast()}
#' for all points to be forecasted simultaneously.
#'
#' @param v numeric vector of similarities used to identify nearest neighbors.
#' @param k.in2 integer value indicating the the number of nearest neighbors to be considered.
#' @param y.in2 numeric vector of the response series to be forecast.
#'
#' @return numeric value of the mean of the \code{k.in2} nearest neighbors in \code{y.in2}.
#' @export
#'
#' @seealso [knn.forecast()] the recommended user facing function to perform knn
#' regression for forecasting with \code{NNreg()}.
#' @md
#'
#'
#' @examples
#' Sim.Mat <- matrix(c(1, .5, .2, .5, 1, .7, .2, .7, 1),
#'   nrow = 3, ncol = 3, byrow = TRUE
#' )
#' Sim.Mat.col <- Sim.Mat[-(3), 3]
#' y <- c(2, 1, 5)
#' k <- 2
#' NNreg(v = Sim.Mat.col, k.in2 = 2, y.in2 = y)
NNreg <- function(v, k.in2, y.in2) {
  # sort the provided vector of similarity from most to least similar
  point.sort <- sort(v, decreasing = T, index = T)[[2]]
  # identify the indices of the k.in2 most similar points
  point.neighbors <- point.sort[1:k.in2]
  # subset y.in2 down to only the identified neighbors
  Y.neighbors <- y.in2[point.neighbors]
  # estimate the point of interest by taking the mean of the nearest neighbors
  Y.hat <- mean(Y.neighbors)
  return(Y.hat)
}
