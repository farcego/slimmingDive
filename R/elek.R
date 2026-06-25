##' Post-processed example data
##'
##' The original seal dataset \code{\link{ele}} provided in
##' slimmingDive after being processed by slimmingDive inmediately
##' before aplying the postKalman function. This dataset is provided for saving
##' time during testing or experimenting with \code{slimmingDive}
##' avoiding the use of the time consuming \code{\link[=kalman]{kalman()}} function.
##' 
##' @format A list with 6 objects:
##' \describe{
##'   \item{Data}{Dataset imputed to the \code{kalman} function}
##'   \item{model}{model definition}
##'   \item{kalman}{kalman expression}
##'   \item{mns}{summaries}
##'   \item{duration}{The time it took to fit the model}
##'   \item{burn.in}{burn.in length, in iterations}
##' ...
##' }
##' @source \url{http://www.imos.org.au}
##' @export
"elek"
