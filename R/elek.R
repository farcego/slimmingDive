##' Post-processed seal
##'
##' The original seal dataset (ele) provided in slimmingDive after
##' being fully processed by slimmingDive (just before aplying the
##' postKalman function). This data set will save time for testing or
##' experimenting by avoiding the use of the time consuming
##' |code{kalman} function.
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
