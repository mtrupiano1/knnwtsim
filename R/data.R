#' boston_fire_incidents_weekly
#'
#' A dataset which contains the weekly count of fire incidents in the city of
#' Boston, MA, USA, as well as a number of holiday indicators and one indicator
#' for the period where a COVID-19 state of emergency was declared in Massachusetts.
#' Data is present for weeks between 2017-01-01 and 2021-07-25. Derived by Matthew Trupiano
#' from a series of .csv files hosted on \url{https://data.boston.gov/}.
#'
#' @format A dataframe with 239 rows and 16 variables:
#' \describe{
#'           \item{week}{first day of a given week (Sunday), date.}
#'           \item{incidents}{summarized count of fire incidents from the Sunday in the week variable to the Saturday before the next week.}
#'           \item{new.years.ind}{1 if YYYY-01-01 or YYYY-12-31 occur during the week else 0.}
#'           \item{christmas.ind}{1 if YYYY-12-24 or YYYY-12-25 occur during the week else 0.}
#'           \item{thanksgiving.ind}{1 if the holiday Thanksgiving occurs during the week else 0.}
#'           \item{veterans.ind}{1 if YYYY-11-11 occurs during the week else 0.}
#'           \item{indigenous.ind}{1 if the holiday Indigenous Peoples Day occurs during the week else 0.}
#'           \item{labor.ind}{1 if the holiday Labor Day occurs during the week else 0.}
#'           \item{july4.ind}{1 if YYYY-07-04 occurs during the week else 0.}
#'           \item{juneteenth.ind}{1 if YYYY-06-19 occurs during the week and the week st is >= '2020-06-01' else 0.}
#'           \item{memorial.ind}{1 if the holiday Memorial Day occurs during the week else 0.}
#'           \item{patriots.ind}{1 if the holiday Patriot's Day occurs during the week else 0.}
#'           \item{st.patricks.ind}{1 if YYYY-03-17 occurs during the week else 0.}
#'           \item{presidents.ind}{1 if the holiday President's Day occurs during the week else 0.}
#'           \item{mlk.ind}{1 if the holiday Martin Luther King Jr. Day occurs during the week else 0.}
#'           \item{covid.soe.ind}{1 if the week contains days between '2020-03-10' and '2021-06-15' when a state of emergency for COVID-19 was declared in Massachusetts else 0.}
#' }
#'
#'
#' @source \url{https://data.boston.gov/dataset/fire-incident-reporting}
"boston_fire_incidents_weekly"



#' boston_fire_incidents_monthly
#'
#' A dataset which contains the monthly count of fire incidents in the city of
#' Boston, MA, USA, as well as one indicator for the period where a
#' COVID-19 state of emergency was declared in Massachusetts.
#' Data is present for starting from 2017-01-01 to 2021-07-01. Derived by Matthew Trupiano
#' from a series of .csv files hosted on \url{https://data.boston.gov/}.
#'
#' @format A dataframe with 55 rows and 3 variables:
#' \describe{
#'           \item{week}{first day of a given month, date.}
#'           \item{incidents}{summarized count of fire incidents in the month which starts the date of the corresponding month variable.}
#'           \item{covid.soe.ind}{1 if the month contains days between '2020-03-10' and '2021-06-15' when a state of emergency for COVID-19 was declared in Massachusetts else 0.}
#' }
#'
#'
#' @source \url{https://data.boston.gov/dataset/fire-incident-reporting}
"boston_fire_incidents_monthly"


#' boston_911dispatch_weekly
#'
#' A dataset which contains the weekly count of 911 dispatches in the city of
#' Boston, MA, USA, for three City of Boston public safety agencies: Boston Police Department, Boston Fire Department,
#' and Boston Emergency Medical Services. In addition a number of holiday indicators, and dummy variables for months.
#' Data is present for weeks between 2010-10-31 and 2014-04-20. Derived by Matthew Trupiano
#' from a .csv file (911 Daily Dispatch Count By Agency (CSV)) hosted on \url{https://data.boston.gov/}.
#'
#' @format A dataframe with 182 rows and 28 variables:
#' \describe{
#'           \item{week}{first day of a given week (Sunday), date.}
#'           \item{BPD}{summarized count of 911 dispatches from the Sunday in the week variable to the Saturday before the next week for the Boston Police Department.}
#'           \item{EMS}{summarized count of 911 dispatches from the Sunday in the week variable to the Saturday before the next week for the Boston Emergency Medical Services.}
#'           \item{BFD}{summarized count of 911 dispatches from the Sunday in the week variable to the Saturday before the next week for the Boston Fire Department.}
#'           \item{new.years.ind}{1 if YYYY-01-01 or YYYY-12-31 occur during the week else 0.}
#'           \item{christmas.ind}{1 if YYYY-12-24 or YYYY-12-25 occur during the week else 0.}
#'           \item{thanksgiving.ind}{1 if the holiday Thanksgiving occurs during the week else 0.}
#'           \item{veterans.ind}{1 if YYYY-11-11 occurs during the week else 0.}
#'           \item{indigenous.ind}{1 if the holiday Indigenous Peoples Day occurs during the week else 0.}
#'           \item{labor.ind}{1 if the holiday Labor Day occurs during the week else 0.}
#'           \item{july4.ind}{1 if YYYY-07-04 occurs during the week else 0.}
#'           \item{juneteenth.ind}{1 if YYYY-06-19 occurs during the week and the week st is >= '2020-06-01' else 0.}
#'           \item{memorial.ind}{1 if the holiday Memorial Day occurs during the week else 0.}
#'           \item{patriots.ind}{1 if the holiday Patriot's Day occurs during the week else 0.}
#'           \item{st.patricks.ind}{1 if YYYY-03-17 occurs during the week else 0.}
#'           \item{presidents.ind}{1 if the holiday President's Day occurs during the week else 0.}
#'           \item{mlk.ind}{1 if the holiday Martin Luther King Jr. Day occurs during the week else 0.}
#'           \item{jan.ind}{1 if week variable occurs in January else 0.}
#'           \item{feb.ind}{1 if week variable occurs in February else 0.}
#'           \item{mar.ind}{1 if week variable occurs in March else 0.}
#'           \item{apr.ind}{1 if week variable occurs in April else 0.}
#'           \item{may.ind}{1 if week variable occurs in May else 0.}
#'           \item{jun.ind}{1 if week variable occurs in June else 0.}
#'           \item{jul.ind}{1 if week variable occurs in July else 0.}
#'           \item{aug.ind}{1 if week variable occurs in August else 0.}
#'           \item{sep.ind}{1 if week variable occurs in September else 0.}
#'           \item{oct.ind}{1 if week variable occurs in October else 0.}
#'           \item{nov.ind}{1 if week variable occurs in November else 0.}
#'           \item{dec.ind}{1 if week variable occurs in December else 0.}
#'}
#'
#' @source \url{https://data.boston.gov/dataset/911-daily-dispatch-count-by-agency}
"boston_911dispatch_weekly"



#' simulation_master_list
#'
#' A list of 20 lists. Each of the 20 lists contains 31 items including 4 simulated time series.
#' Each series contains an ARIMA component, a periodic component simulated using trig functions, a component determined by a functional relationship
#' to exogenous predictors which we will call the f(x) component, a constant, and finally additional noise generated from either a
#' Gaussian distribution with mean = 0, or Poisson distribution. The 4 series within a given sublist only differ based on the f(x) component of the series. One series, \code{series.mvnormx}, uses
#' a matrix \code{X} generated by \code{MASS::mvrnorm()} with corresponding coefficients for the f(x) component. All other
#' series use piece-wise functional relationships for the f(x) component of the series.
#'
#'
#' @format A list containing 20 sublists each with 31 items:
#' \describe{
#'           \item{series.len}{the number of observations in the simulated time series}
#'           \item{random.seed}{the random seed used in \code{set.seed()} for all random components in the sublist.}
#'           \item{arima.p}{The AR order argument for \code{stats::arima.sim()}.}
#'           \item{arima.d}{The differencing order argument for \code{stats::arima.sim()}.}
#'           \item{arima.q}{The MA order argument for \code{stats::arima.sim()}.}
#'           \item{ar.coefficients}{Coefficients for the AR process in \code{stats::arima.sim()},\code{NULL} if \code{arima.p=0}.}
#'           \item{ma.coefficients}{Coefficients for the MA process in \code{stats::arima.sim()},\code{NULL} if \code{arima.q=0}.}
#'           \item{seasonal.periods}{The number of periods in a full cycle for the periodic component of the series.}
#'           \item{sin.coef}{Coefficient on the sin term of the periodic component of the series.}
#'           \item{cos.coef}{Coefficient on the cos term of the periodic component of the series.}
#'           \item{X.cols}{Number of predictors used to generate the f(x) component of \code{series.mvnormx}.}
#'           \item{X.mu}{The mean vector used in \code{MASS::mvrnorm()} to generate \code{X} for the f(x) component of \code{series.mvnormx}.}
#'           \item{X.Sigma}{The covariance matrix generated by \code{clusterGeneration::rcorrmatrix()} used in \code{MASS::mvrnorm()} to generate \code{X} for the f(x) component of \code{series.mvnormx}.}
#'           \item{X}{The matrix of \code{X.cols} predictors generated by \code{MASS::mvrnorm()} used to generate the f(x) component of \code{series.mvnormx}.}
#'           \item{x.coef}{The vector of \code{X.cols} coefficients corresponding to the predictors of \code{X} used to generate the f(x) component of \code{series.mvnormx}.}
#'           \item{x.chng.mu}{The mean value used in \code{stats::rnorm()} used to generate \code{x.chng}.}
#'           \item{x.chng.sd}{The standard deviation value used in \code{stats::rnorm()} used to generate \code{x.chng}.}
#'           \item{x.chng.coef1}{A coefficient for \code{x.chng} used in all piece-wise functional relationship, f(x), components, as the \code{coef} argument to \code{lin.to.sqrt()} and \code{quad.to.cubic}
#'            and the \code{coef1} argument to \code{ lin.coef.change }.}
#'           \item{x.chng.coef2}{A coefficient for \code{x.chng} used in the piece-wise functional relationship, f(x), component of \code{series.lin.coef.chng.x}, as the \code{coef2} argument to \code{ lin.coef.change }.}
#'           \item{x.chng.break.point}{A value used in two piece-wise functional relationship, f(x), components, as the \code{break.point} argument to \code{quad.to.cubic} and \code{ lin.coef.change }.}
#'           \item{x.chng.break.point.sqrt}{The \code{max()} of \code{x.chng.break.point} and some value > 0. Used in the piece-wise functional relationship, f(x), component of \code{series.lin.to.sqrt.x} , as the \code{break.point} argument to \code{lin.to.sqrt}.}
#'           \item{x.chng}{A vector of observations of a single predictor used to generate the f(x) component of all series other than \code{series.mvnormx}.}
#'           \item{type.noise}{The family of probability distributions to to generate the additional noise component.}
#'           \item{poisson.rate}{The \code{lambda} argument of \code{stats::rpois()} used to generate additional noise, only actually used if \code{type.noise = 'poisson'}. }
#'           \item{norma.sd}{The \code{sd} argument of \code{stats::rnorm()} used to generate additional noise, only actually used if \code{type.noise = 'normal'}. }
#'           \item{constant}{A numeric value which is the constant component of the series.}
#'           \item{series.mvnormx}{A simulated time series generated from the sum of ARIMA, Periodic, f(x), noise, and constant components. In this case f(x) represents linear relationships to the columns of the matrix \code{X}.  }
#'           \item{series.lin.to.sqrt.x}{A simulated time series generated from the sum of ARIMA, Periodic, f(x), noise, and constant components. In this case f(x) represents a linear relationship to a single predictor \code{x.chng} which changes to a \code{sqrt(x.chng)}
#'           relationship when \code{x.chng > x.chng.break.point.sqrt}.}
#'           \item{series.lin.coef.chng.x}{A simulated time series generated from the sum of ARIMA, Periodic, f(x), noise, and constant components. In this case f(x) represents a linear relationship to a single predictor \code{x.chng} which changes coefficient when
#'            \code{x.chng > x.chng.break.point}.}
#'           \item{series.quad.to.cubic.x}{A simulated time series generated from the sum of ARIMA, Periodic, f(x), noise, and constant components. In this case f(x) represents a quadratic relationship to a single predictor \code{x.chng} which changes to a cubic relationship when
#'           \code{x.chng > x.chng.break.point}, in addition a coefficient changes sign at \code{x.chng.break.point}.}
#'           }
#'
#' @details
#' Below we have the functional relationships used for the piece-wise series:
#'
#' \code{lin.to.sqrt <- function(x, break.point, coef){
#'  if (x < break.point) {
#'   out <- coef * x
#' } else {
#'   out <- sqrt(x)
#'  }
#' return(out)
#'  }
#'}
#'
#' \code{quad.to.cubic <- function(x, break.point, coef){
#'  if (x < break.point) {
#'   out <- coef * (x ** 2)
#' } else {
#'   out <- -coef * (x ** 3)
#' }
#' return(out)
#' }
#'}
#'
#' \code{ lin.coef.change <- function(x, break.point, coef1, coef2){
#' if (x < break.point) {
#'   out <- coef1 * x
#' } else {
#'   out <- coef2 * x
#' }
#' return(out)
#' }
#'}
#'
#' @source \url{https://github.com/mtrupiano1/knnwtsim/blob/main/data-raw/simulation_master_list.R}
#'
"simulation_master_list"
