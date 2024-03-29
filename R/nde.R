##' Function that calculates the drift rate and the drift segment of a
##' given dive.
##'
##' The selection procedure is based on the 'order' of the generation
##' of the inflection points, plus a number of criteria, that helps to
##' determine if the dive is a drift dive, and, if so, whether is a
##' positive or negative drift, and which one is the drifting
##' segment. The drift rate is then calculated as the difference of
##' depths at the start and end of the drifting segment divided by its
##' duration.
##' @title Drift Rate estimator
##' @param d a single summarized dive row with variables generated by
##'     calling function
##' @param extract flag for determining which output is requested to
##'     the function \cr \itemize{ \item 'nde': call for retrieving
##'     the drift rate estimate \item 'ds': call for retrieving the
##'     drifting segment \item 'both': call for retrieving both values
##'     }
##' @return a numeric value, either the drift rate or the drift
##'     segment, or a vector containing both.
##' @export
NDE <- function(d, extract='nde'){
    a.n <- as.numeric     # remaping as.numeric function
    nde <- NA             # drift estimate
    ds <- 0               # drifting segment

    f <- (a.n(d["D2"]) - a.n(d["D1"]))/(a.n(d["T2"]) - a.n(d["T1"]))
    f <- ifelse(is.finite(f),f,0)
    s <- (a.n(d["D3"]) - a.n(d["D2"]))/(a.n(d["T3"]) - a.n(d["T2"]))
    s <- ifelse(is.finite(s),s,0)
    t <- (a.n(d["D4"]) - a.n(d["D3"]))/(a.n(d["T4"]) - a.n(d["T3"]))
    t <- ifelse(is.finite(t),t,0)
    
    if(d['order']=='2.1.3.4' & a.n(d['mdepthbias']) < 0 ) { # pos
        nde <- s
        ds <- 2
    } else if (d['order']=='2.1.3.4' & a.n(d['mdepthbias']) > 0 ) { # neg
        nde <- f
        ds=1
    } else if (d['order']=='2.1.4.3' & a.n(d['propseg1']) <= 25 &
               1.1* a.n(d['propseg2']) >= a.n(d['propseg3'])) { # pos
        nde <- s
        ds <- 2
    } else if (d['order']=='2.1.4.3' & a.n(d['propseg1']) <= 25 &
               1.1 * a.n(d['propseg2']) < a.n(d['propseg3'])) { # pos
        nde <- t
        ds <- 3
    } else if (d['order']=='2.1.4.3' & a.n(d['propseg1']) > 25) { # neg
        nde <- f
        ds <- 1
    } else if (d['order']=='2.4.1.3' & a.n(d['mdepthbias']) < 0 &
               a.n(d['propseg1']) > a.n(d['propseg3'])) {
        nde <- f
        ds <- 1
    } else if (d['order']=='2.4.1.3' & a.n(d['mdepthbias']) < 0 &
               a.n(d['propseg1']) < a.n(d['propseg3'])) {
        nde <- t
        ds <- 3
    } else if (d['order']=='2.4.1.3' & a.n(d['mdepthbias']) > 0  &
               a.n(d['propseg1']) > a.n(d['propseg2'])) {
        nde <- f
        ds <- 1
    } else if (d['order']=='2.4.1.3' & a.n(d['mdepthbias']) > 0 &
               a.n(d['propseg1']) <= a.n(d['propseg2'])) {
        nde <- s
        ds <- 2
    } else if (d['order']== '3.1.2.4' & a.n(d['avratio']) > 0) {
        nde <- s
        ds <- 2
    } else if (d['order']=='3.1.2.4' & a.n(d['avratio']) < 0) {
        nde <- f
        ds <- 1
    } else if (d['order']=='3.1.4.2' & a.n(d['propseg1']) < 25 &
               a.n(s) < 0 & a.n(t) > 0) {
        nde <- s
        ds <- 2
    } else if (d['order']=='3.1.4.2' & a.n(d['propseg1']) < 25 &
               a.n(s) > 0 & a.n(t) < 0) {
        nde <- t
        ds <- 3
    } else if (d['order']=='3.1.4.2' & a.n(d['propseg1']) < 25 &
               a.n(s) < 0 & a.n(t) < 0 & a.n(d['hp2']) > a.n(d['hp3'])) {
        nde <- s
        ds <- 2
    } else if (d['order']=='3.1.4.2' & a.n(d['propseg1']) < 25 &
               a.n(s) < 0 & a.n(t) < 0 & a.n(d['hp2']) < a.n(d['hp3'])) {
        nde <- t
        ds <- 3
    } else if (d['order']=='3.1.4.2' & a.n(d['propseg1']) > 25) {
        nde <- f
        ds <- 1
    } else if (d['order']=='3.2.1.4') {
        nde <- s
        ds <- 2
    } else if (d['order']=='3.4.1.2' & a.n(d['mdepthbias']) < 0 ) {
        nde <- t
        ds <- 3
    } else if (d['order']=='3.4.1.2' & a.n(d['mdepthbias']) > 0 &
               a.n(d['propseg1']) > a.n(d['propseg2'])) {
        nde <- f
        ds <- 1
    } else if (d['order']=='3.4.1.2' & a.n(d['mdepthbias']) > 0 &
               a.n(d['propseg1']) < a.n(d['propseg2'])) {
        nde <- s
        ds <- 2
    } else if (d['order']=='4.2.1.3' & a.n(d['mdepthbias']) < 0) {
        nde <- t
        ds <- 3
    } else if (d['order']=='4.2.1.3' & a.n(d['mdepthbias']) >= 0) {
        nde <- s
        ds <- 2
    }
    if (extract=='nde'){
        return(-nde)
    } else if (extract=='ds') {
        return(ds)
    } else if (extract=='both'){
        return(c(-nde,ds))
    }
}
