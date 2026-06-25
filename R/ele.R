##' Example of elephant seal Satellite relayed data
##'
##' Example of a seal dataset as tipically provided by SMRU tags
##' trough the ARGOS satellite platform. There are slight differences
##' between different datasets from SMRU tags. Some variables may be
##' lower or upper case in different data sets. This is a subset
##' containing only the variables needed to process summarized dives
##' in slimmingDive. Dive information belongs to the seal instrumented
##' with ct78-450-12 tag.
##'
##' @format An object of class `data.frame` with 6584 rows and 15 variables:
##' \describe{
##' 
##'   \item{ref}{seal-tag ID code, in this example ct78-450-12}
##'   \item{DE_DATE}{Date}
##'   \item{SURF_DUR}{Surfacing period duration}
##'   \item{DIVE_DUR}{Dive duration}
##'   \item{MAX_DEP}{Maximum depth}
##'   \item{D1}{Depth of the first inflection point}
##'   \item{D2}{Depth of the second inflection point}
##'   \item{D3}{Depth of the third inflection point}
##'   \item{D4}{Depth of the fourth inflection point}
##'   \item{T1}{Time of the first inflection point}
##'   \item{T2}{Time of the second inflection point}
##'   \item{T3}{Time of the third inflection point}
##'   \item{T4}{Time of the fourth inflection point}
##'   \item{lat}{Latitude}
##'   \item{lon}{Longitude}
##' ...
##' }
##' @source \url{http://www.imos.org.au}
"ele"
