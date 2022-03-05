# knnwtsim 1.0.0

## Major Changes
* The function `knn.forecast.boot.intervals()` has been added to provide the capability to generate interval forecasts and simulations with the package. 

* The default value of `max.k` in `knn.forecast.randomsearch.tuning()` has been changed from `NA` to `NULL`, to bring it in line with the approach I took to optional arguments elsewhere in the package. However, passing `max.k = NA` will still work, and behave in the same manner as `max.k = NULL`. 

* Changing the behavior of `max.k` in `knn.forecast.randomsearch.tuning()` when the current or previous default argument is passed to be more dynamic based on the length of the input series. Now `max.k` will be set to `min(floor((length(y.in)) * .4), length(y.in) - val.holdout.len - test.h)` if `NULL` or `NA` is passed. This change was made because the more arbitrary behavior in `knnwtsim 0.1.0` where `max.k` would be set to `min(floor((length(y.in)) * .4), 50)` if `NA` was passed could lead to errors.

* Error handling to throw errors or warnings for conflicting arguments, arguments outside reasonable ranges, and argument types differing from those listed in the help files has been added all user facing functions in the package: `StMatrixCalc()`, `SpMatrixCalc()`, `SxMatrixCalc()`, `SwMatrixCalc()`, `knn.forecast()`,  `knn.forecast.randomsearch.tuning()`, and `knn.forecast.boot.intervals()`.

## Minor Changes

* A `min.k` argument has been added to `knn.forecast.randomsearch.tuning()` which can be used set a floor for the minimum number of nearest neighbors to be proposed in any parameter set to be tested in tuning. By default `min.k = 1`, in line with previous behavior of the function.

* Added references to arXiv:2112.06266v1 to `DESCRIPTION` and help files as needed to provide more information on methodology. 

