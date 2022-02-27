#' Seasonal Absolute Dissimilarity
#'
#' Calculate seasonal dissimilarity measure between the respective seasonal period
#' two points, given the number of periods in one full seasonal cycle.
#'
#' @param p1 numeric value representing a seasonal period.
#' @param p2 numeric value representing a seasonal period.
#' @param nPeriods numeric value representing the maximum value \code{p1} or \code{p2} can take on.
#'
#' @return numeric value of the seasonal dissimilarity between \code{p1} and \code{p2}.
#' @export
#'
#' @seealso
#' Trupiano (2021) <arXiv:2112.06266v1> for information on the formulation of this seasonal dissimilarity measure.
#' @md
#' @examples
#' SeasonalAbsDissimilarity(1, 4, 4)
SeasonalAbsDissimilarity <- function(p1, p2, nPeriods) {
  minPeriod <- 1

  # picture putting the seasonal periods on a clock, and calculate the distance
  # between any two points both clockwise and counterclockwise, then take min
  DirectDis <- abs(p1 - p2)
  AroundDis <- abs(min(p1, p2) - minPeriod) + abs(nPeriods - max(p1, p2)) + 1

  Dp <- min(DirectDis, AroundDis)

  return(Dp)
}


#' Calculate Seasonal Similarity Matrix
#'
#' Generates and returns an n x n matrix by calculating the seasonal dissimilarity
#' for each possible pair of points in a vector
#' of seasonal periods, then converts dissimilarity matrix to a similarity matrix using 1 / (D_p + 1).
#'
#' @param v positive numeric vector with the seasonal periods corresponding to each point in the response series.
#' @param nPeriods positive numeric value representing the maximum value \code{v} can take on.
#'
#' @return numeric matrix of seasonal similarities for the vector \code{v}.
#' @export
#'
#' @seealso
#' * Trupiano (2021) <arXiv:2112.06266v1> for information on the formulation of this seasonal similarity measure.
#' * [SeasonalAbsDissimilarity()] for the function used to calculate seasonal dissimilarity.
#' @md
#' @examples
#' SpMatrixCalc(c(1, 2, 4), 4)
SpMatrixCalc <- function(v, nPeriods) {

  # argument type error handling
  if (!(is.vector(v, mode = "numeric"))) {
    stop("v should be a numeric vector")
  }

  if ((!(is.vector(nPeriods, mode = "numeric"))) |
    (!(identical(length(nPeriods), 1L)))) {
    stop("nPeriods should be numeric with length 1L")
  }

  # argument value error handling
  if (any(c(v, nPeriods) < 0)) {
    stop("Niether v or nPeriods should contain a value < 0")
  }

  if (any(v > nPeriods)) {
    stop("Some observations of v are greater than nPeriods")
  }

  # set up a matrix to hold calculations
  n <- length(v)
  dpmat <- matrix(NA, nrow = n, ncol = n)

  # fill in the matrix using seasonal dissimilarity function
  for (i in c(1:n)) {
    p1 <- v[i]
    for (j in c(1:n)) {
      p2 <- v[j]
      dpmat[i, j] <- SeasonalAbsDissimilarity(p1, p2, nPeriods)
    }
  }
  # convert dissimilarities to similarities
  spmat <- 1 / (1 + dpmat)
  return(spmat)
}



#' Temporal Absolute Dissimilarity
#'
#' Simply takes the absolute difference between two points, meaning points close
#' in time will have smaller dissimilarity. This is equivalent to Euclidean Distance.
#'
#' @param p1 numeric value representing the time order of the point in the response series.
#' @param p2 numeric value representing the time order of the point in the response series.
#'
#' @return numeric value of the absolute difference between \code{p1} and \code{p2}.
#' @export
#'
#'
#' @examples
#' TempAbsDissimilarity(1, 3)
TempAbsDissimilarity <- function(p1, p2) {
  Dt <- abs(p1 - p2)
  return(Dt)
}


#' Calculate Temporal Similarity Matrix
#'
#' Generates and returns an n x n matrix by calculating the absolute difference
#' for each possible pair of points in a vector of the time orders of each point in a series,
#' then converts dissimilarity matrix to a similarity matrix using 1 / (D_t + 1).
#'
#' @param v numeric vector with the time order corresponding to each point in the response series.
#'
#' @return numeric matrix of temporal similarities for the vector \code{v}.
#' @export
#'
#' @seealso [TempAbsDissimilarity()] for the function used to calculate
#' absolute differences.
#' @md
#' @examples
#' StMatrixCalc(c(1, 2, 3))
StMatrixCalc <- function(v) {

  # argument type error handling
  if (!(is.vector(v, mode = "numeric"))) {
    stop("v should be a numeric vector")
  }

  # set up a matrix to hold calculations
  n <- length(v)
  dtmat <- matrix(NA, nrow = n, ncol = n)
  # fill in the matrix using temporal dissimilarity function
  for (i in c(1:n)) {
    p1 <- v[i]
    for (j in c(1:n)) {
      p2 <- v[j]
      dtmat[i, j] <- TempAbsDissimilarity(p1, p2)
    }
  }
  # convert dissimilarities to similarities
  stmat <- 1 / (1 + dtmat)
  return(stmat)
}



#' Calculate Similarity Matrix for Exogenous Predictors
#'
#' Largely a wrapper function for the \code{stats::dist()} function. First calculates
#' n x n distance matrix using specified method for an input matrix or vector using
#'  \code{stats::dist()}. Then converts the distance matrix to similarity matrix using 1 / (D_x + 1).
#'
#' @param A numeric matrix or numeric vector where the columns represents exogenous predictor variables and the rows correspond to the points in the response series.
#' @param XdistMetric character describing the method \code{stats::dist()} should use.
#' This must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"}, or \code{"minkowski"}.
#'
#' @return numeric matrix of distances for \code{A}.
#' @export
#'
#' @examples
#' X <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, ncol = 3, byrow = TRUE)
#' SxMatrixCalc(X)
SxMatrixCalc <- function(A, XdistMetric = "euclidean") {

  # argument type error handling
  if (!((is.vector(A, mode = "numeric")) | (is.matrix(A) & is.numeric(A)))) {
    stop("A should be a numeric vector or a numeric matrix")
  }

  # get full symmetric distance matrix from stats::dist
  dxmat <- as.matrix(stats::dist(A,
    method = XdistMetric,
    upper = TRUE, diag = TRUE
  ))
  # convert distances to similarities
  sxmat <- 1 / (1 + dxmat)
  return(sxmat)
}



#' Calculate Weighted Similarity Matrix
#'
#' A wrapper function which calls each of \code{StMatrixCalc(v = t.in)},
#'  \code{SpMatrixCalc(v = p.in, nPeriods = nPeriods.in)},
#' and \code{SxMatrixCalc(A = X.in, XdistMetric = XdistMetric.in)} to generate the three matrices of using the component measures of \code{S_w}. Then
#' generates the final weighted similarity matrix as the sum of each component matrix multiplied by its corresponding \code{weights}.
#' The first value in \code{weights} will be multiplied by \code{S_t}, the second \code{S_p}, and the third \code{S_x}.
#'
#'
#' @param t.in numeric vector of time orders for points in the response series.
#' @param p.in numeric vector of period within a seasonal cycle (ex. 1 for January points in monthly data).
#' @param nPeriods.in numeric scalar indicating the maximum value \code{p.in} could take on (ex. 12 for monthly data).
#' @param X.in numeric vector or matrix of exogenous predictors, where the rows correspond to points in the response series.
#' @param XdistMetric.in character describing the method \code{stats::dist()} should use. This must be one of \code{"euclidean"},
#'  \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"}, or \code{"minkowski"}.
#' @param weights numeric vector where first value represents weight for \code{S_t}, second value the weight for \code{S_p}, and the third value the weight for \code{S_x}.
#'
#' @return numeric matrix of similarities which is calculated using \code{S_w}.
#' @export
#'
#' @seealso
#' * Trupiano (2021) <arXiv:2112.06266v1> for information on the formulation of \code{S_w}.
#' * [StMatrixCalc()] for information on the calculation of \code{S_t}.
#' * [SpMatrixCalc()] for information on the calculation of \code{S_p}.
#' * [SxMatrixCalc()] for information on the calculation of \code{S_x}.
#' @md
#'
#' @examples
#' t <- c(1, 2, 3)
#' p <- c(1, 2, 1)
#' X <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, ncol = 3, byrow = TRUE)
#' SwMatrixCalc(
#'   t.in = t,
#'   p.in = p, nPeriods.in = 2,
#'   X.in = X,
#'   weights = c(1 / 4, 1 / 4, 1 / 2)
#' )
SwMatrixCalc <- function(t.in,
                         p.in, nPeriods.in,
                         X.in, XdistMetric.in = "euclidean",
                         weights = c(1 / 3, 1 / 3, 1 / 3)) {

  # arugment type error checks
  if (!(is.vector(weights, mode = "numeric"))) {
    stop("weights should be a numeric vector")
  } else if (length(weights) > 3) {
    warning("weights vector length > 3, only first three elements will be used")
  } else if (length(weights) < 3) {
    stop("weights vector length < 3
         , if it is desired to exclude a component set weight to 0")
  }

  # The component function calls will handle errors relating to their respective
  # arguments, just handling dimension problems here
  if ((is.vector(t.in)) & (is.vector(p.in)) & (is.vector(X.in))) {
    if ((!(identical(length(t.in), length(p.in)))) |
      (!(identical(length(t.in), length(X.in))))) {
      stop("t.in, p.in, and X.in are all vectors but lengths differ")
    }
  } else if ((is.vector(t.in)) & (is.vector(p.in)) & (is.matrix(X.in))) {
    if ((!(identical(length(t.in), length(p.in)))) |
      (!(identical(length(t.in), dim(X.in)[1])))) {
      stop("t.in, p.in, are vectors, and X.in is matrix
           , but vector lengths and matrix row dimension are not all equal")
    }
  }

  # assigning the respective weights
  alpha <- weights[1]
  beta <- weights[2]
  gamma <- weights[3]

  # call component functions
  St <- StMatrixCalc(v = t.in)

  Sp <- SpMatrixCalc(v = p.in, nPeriods = nPeriods.in)

  Sx <- SxMatrixCalc(A = X.in, XdistMetric = XdistMetric.in)

  # combine component similarity matrices into final similarity matrix
  Sw <- alpha * St + beta * Sp + gamma * Sx

  return(Sw)
}
