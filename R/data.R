#' boston_fire_incidents_weekly
#'
#' A dataset which contains the weekly count of fire incidents in the city of
#' Boston, MA, USA, as well as a number of holiday indicators and one indicator
#' for the period where a COVID-19 state of emergency was declared in Massachusetts.
#' Data is present for weeks between 2017-01-01 and 2021-07-25. Derived by Matthew Trupiano
#' from a series of .csv files hosted on https://data.boston.gov/
#'
#' @format A dataframe with 239 rows and 16 variables:
#' \describe{
#'           \item{week}{first day of a given week (Sunday), date}
#'           \item{incidents}{summarized count of fire incidents from the Sunday in the week variable to the Saturday before the next week}
#'           \item{new.years.ind}{1 if YYYY-01-01 or YYYY-12-31 occur during the week else 0}
#'           \item{christmas.ind}{1 if YYYY-12-24 or YYYY-12-25 occur during the week else 0}
#'           \item{thanksgiving.ind}{1 if the holiday Thanksgiving occurs during the week else 0}
#'           \item{veterans.ind}{1 if YYYY-11-11 occurs during the week else 0}
#'           \item{indigenous.ind}{1 if the holiday Indigenous Peoples Day occurs during the week else 0}
#'           \item{labor.ind}{1 if the holiday Labor Day occurs during the week else 0}
#'           \item{july4.ind}{1 if YYYY-07-04 occurs during the week else 0}
#'           \item{juneteenth.ind}{1 if YYYY-06-19 occurs during the week and the week st is >= '2020-06-01' else 0}
#'           \item{memorial.ind}{1 if the holiday Memorial Day occurs during the week else 0}
#'           \item{patriots.ind}{1 if the holiday Patriot's Day occurs during the week else 0}
#'           \item{st.patricks.ind}{1 if YYYY-03-17 occurs during the week else 0}
#'           \item{presidents.ind}{1 if the holiday President's Day occurs during the week else 0}
#'           \item{mlk.ind}{1 if the holiday Martin Luther King Jr. Day occurs during the week else 0}
#'           \item{covid.soe.ind}{1 if the week contains days between '2020-03-10' and '2021-06-15' when a state of emergency for COVID-19 was declared in MA else 0}
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
#' from a series of .csv files hosted on https://data.boston.gov/
#'
#' @format A dataframe with 55 rows and 3 variables:
#' \describe{
#'           \item{week}{first day of a given month, date}
#'           \item{incidents}{summarized count of fire incidents in the month which starts the date of the corresponding month variable}
#'           \item{covid.soe.ind}{1 if the month contains days between '2020-03-10' and '2021-06-15' when a state of emergency for COVID-19 was declared in MA else 0}
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
#' from a .csv file (911 Daily Dispatch Count By Agency (CSV)) hosted on https://data.boston.gov/
#'
#' @format A dataframe with 182 rows and 28 variables:
#' \describe{
#'           \item{week}{first day of a given week (Sunday), date}
#'           \item{BPD}{summarized count of 911 dispatches from the Sunday in the week variable to the Saturday before the next week for the Boston Police Department}
#'           \item{EMS}{summarized count of 911 dispatches from the Sunday in the week variable to the Saturday before the next week for the Boston Emergency Medical Services}
#'           \item{BFD}{summarized count of 911 dispatches from the Sunday in the week variable to the Saturday before the next week for the Boston Fire Department}
#'           \item{new.years.ind}{1 if YYYY-01-01 or YYYY-12-31 occur during the week else 0}
#'           \item{christmas.ind}{1 if YYYY-12-24 or YYYY-12-25 occur during the week else 0}
#'           \item{thanksgiving.ind}{1 if the holiday Thanksgiving occurs during the week else 0}
#'           \item{veterans.ind}{1 if YYYY-11-11 occurs during the week else 0}
#'           \item{indigenous.ind}{1 if the holiday Indigenous Peoples Day occurs during the week else 0}
#'           \item{labor.ind}{1 if the holiday Labor Day occurs during the week else 0}
#'           \item{july4.ind}{1 if YYYY-07-04 occurs during the week else 0}
#'           \item{juneteenth.ind}{1 if YYYY-06-19 occurs during the week and the week st is >= '2020-06-01' else 0}
#'           \item{memorial.ind}{1 if the holiday Memorial Day occurs during the week else 0}
#'           \item{patriots.ind}{1 if the holiday Patriot's Day occurs during the week else 0}
#'           \item{st.patricks.ind}{1 if YYYY-03-17 occurs during the week else 0}
#'           \item{presidents.ind}{1 if the holiday President's Day occurs during the week else 0}
#'           \item{mlk.ind}{1 if the holiday Martin Luther King Jr. Day occurs during the week else 0}
#'           \item{jan.ind}{1 if week variable occurs in January else 0}
#'           \item{feb.ind}{1 if week variable occurs in February else 0}
#'           \item{mar.ind}{1 if week variable occurs in March else 0}
#'           \item{apr.ind}{1 if week variable occurs in April else 0}
#'           \item{may.ind}{1 if week variable occurs in May else 0}
#'           \item{jun.ind}{1 if week variable occurs in June else 0}
#'           \item{jul.ind}{1 if week variable occurs in July else 0}
#'           \item{aug.ind}{1 if week variable occurs in August else 0}
#'           \item{sep.ind}{1 if week variable occurs in September else 0}
#'           \item{oct.ind}{1 if week variable occurs in October else 0}
#'           \item{nov.ind}{1 if week variable occurs in November else 0}
#'           \item{dec.ind}{1 if week variable occurs in December else 0}
#'}
#'
#' @source \url{https://data.boston.gov/dataset/911-daily-dispatch-count-by-agency}
"boston_911dispatch_weekly"
