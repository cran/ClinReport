#' datafake
#'
#' Fictive data created only for the purpose of testing the package and showing examples.
#' 
#' @usage data(datafake)
#' 
#' @format A data frame
#' \describe{
#'   \item{y_numeric}{Fake numerical response}
#'   \item{y_logistic}{Fake Logistic response}
#'   \item{y_poisson}{Fake Poisson response}
#'   \item{baseline}{A baseline covariate}
#'   \item{VAR}{A factor}
#'   \item{GROUP}{A fake treatment factor}
#'   \item{TIMEPOINT}{A fake time factor}
#'   \item{SUBJID}{A fake id factor}
#' }
#' 
"datafake"




#' time_to_cure
#'
#' Simulated survival data created only for the purpose of testing the package and showing examples.
#' 
#' @usage data(time_to_cure)
#'
#' @format A data frame
#' #' \describe{
#'  \item{time}{Fake numerical time values}
#' \item{status}{Fake status values, 1=cured 0=not cured}
#' \item{block}{Fake factor}
#' \item{random_number}{Fake factor}
#' \item{Sex}{Fake sex factor}
#' \item{Weight_D0}{Fake weight covariate}
#' \item{Pen}{Fake factor}
#' \item{TCS}{Fake covariate}
#' \item{Group}{Fake treatment group factor}
#' \item{Subjid}{Fake subject Id}
#' }
#' 
"time_to_cure"


#' adverse_event
#'
#' Fake adverse event data example
#' 
#' @usage data(adverse_event)
#'
#' @format A data frame
#' #' \describe{
#' \item{SOCNAME}{Fake System Organ Class variable}
#' \item{HLTNAME}{Fake factor}
#' \item{PTNAME}{Fake Prefered Terms variable}
#' \item{randtrt}{Fake treatment group factor}
#' \item{SUBJID}{Fake subject Id}
#' }
#' 
"adverse_event"
