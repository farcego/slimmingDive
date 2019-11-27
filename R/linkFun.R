##' GLM family object to fit mass models to observed sink rate data.
##'
##' This function provides a family object for modelling sink rates of
##' animals of varying condition.
##'
##' The terminal or free settling velocity of a sinking object of
##' density \eqn{\rho}, volume \eqn{V} and cross-sectional area \eqn{A} is
##' \deqn{v = -\left ( \frac{2g V}{C_{d} A} (\rho-1) \right )^{1/2}}{%
##' v = -( 2 g V (\rho-1)/ (Cd A) )^(1/2)}
##'
##' The volume \eqn{V} is modelled in terms of mass \eqn{M}, an
##' initial mass \eqn{M_{0}}{M0} and volume \eqn{V_{0}}{V0}, and the volume
##' \eqn{V_{m}}{Vm} per unit mass of the accreted mass
##' \deqn{V = V_{0} + V_{m}(M-M_{0})}{V = V0 + Vm (M-M0)}.
##'
##' The expression for free settling velocity is rewritten as
##' \deqn{v = -a \left ( R(V/V_{0})(M/V-1) \right )^{1/2}}{%
##' v = -a (R(V/V0) (M/V-1) )^(1/2)}
##' where \eqn{R(V)= k V/A} and \eqn{R(1)=1} and \eqn{a} and \eqn{k}
##' are constants.
##'
##' The family provides two links that relate the expected sink rate
##' to the mass of the animal.  The \code{"dragp"} link corresponds to
##' \eqn{R(z) = z^p}, and the \code{"drag0"} link corresponds to
##' \eqn{R(z) = 1}.
##'
##' If the animal grows as a sphere as it gains condition, then area
##' scales as radius squared and volume as radius cubed, so \eqn{V/A
##' \propto V^{1/3}}{V/A = k V^(1/3)} and \eqn{p=1/3}.  If the animal
##' grows as a cyclinder of constant length as it gains condition,
##' then area scales with radius and volume as radius squared, so
##' \eqn{V/A \propto V^{1/2}}{V/A = k V^(1/2)} and \eqn{p=1/2}.  The
##' \eqn{p=0} case corresponds to an animal that grows only in one
##' dimension as gains condition.
##'
##' @title Sink Rate Modelling
##' @param M0 initial mass of animal
##' @param V0 initial volume of animal
##' @param a a proportionality constant.
##' @param Vm the volume per unit mass of the accreted mass
##' @param p the power in the V/A relation for the dragp link
##' @param link the link function
##' @return an object of class \code{family}.  Essential a gaussian
##' family with a link function that relates the animal's mass to
##' expected drift rate.
##' @export
drift <- function(M0,V0,a,Vm=1.11,p=1/2,link=c("dragp","drag0")) {

  link <- match.arg(link)
  sgn <- function(s) ifelse(s>=0,1,-1)

  if(link=="drag0") {
    linkfun <- function(mu) {
      s <- sgn(mu)
      -(V0-Vm*M0)*(s-(mu/a)^2)/(s*(Vm-1)-Vm*(mu/a)^2)
    }
    linkinv <- function(eta) {
      vol <- V0+Vm*(eta-M0)
      s <- sgn(eta/vol-1)
      -a*s*sqrt(s*(eta/vol-1))
    }
    mu.eta <- function(eta) {
      vol <- V0+Vm*(eta-M0)
      -a/2*((V0-Vm*M0)/vol^2)/sqrt(abs(eta/vol-1))
    }
    valideta <- function(eta) TRUE
  }
  if(link=="dragp") {
    R <- function(r) r^p
    dR <- function(r) p*r^(p-1)
    linkfun <- function(mu) {
      s <- sgn(mu)
      mu1 <- mu/a
      eta <- -(s-mu1^2)*(V0-M0*Vm)/(s*(Vm-1)-Vm*mu1^2)
      for(k in 1:10) {
        vol <- V0+Vm*(eta-M0)
        mu1 <- mu/sqrt(R(vol/V0))
        eta <- -(s-mu1^2)*(V0-M0*Vm)/(s*(Vm-1)-Vm*mu1^2)
      }
     eta
    }
    linkinv <- function(eta) {
      vol <- V0+Vm*(eta-M0)
      s <- sgn(eta/vol-1)
      -a*s*sqrt(R(vol/V0)*abs(eta/vol-1))
    }
    mu.eta <- function(eta) {
      vol <- V0+Vm*(eta-M0)
      r <- R(vol/V0)
      -a/2*(r*(V0-Vm*M0)/vol^2+Vm/V0*(eta/vol-1)*dR(vol/V0))/sqrt(r*abs(eta/vol-1))
    }
    valideta <- function(eta) TRUE
  }


  structure(list(family = "gaussian",
                 link = link,
                 linkfun = linkfun,
                 linkinv = linkinv,
                 variance = function(mu) rep.int(1,length(mu)),
                 dev.resids = function(y, mu, wt) wt*(y-mu)^2,
                 aic = function(y, n, mu, wt, dev) {
                   nobs <- length(y)
                   nobs*(log(dev/nobs*2*pi)+1)+2-sum(log(wt))
                 },
                 mu.eta = mu.eta,
                 initialize = expression({
                   n <- rep.int(1, nobs)
                   mustart <- y
                 }),
                 validmu = function(mu) TRUE,
                 valideta = valideta),
            class = "family")
}


