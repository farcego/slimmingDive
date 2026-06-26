## need to be tested against CODA function. Va a ser llamada dentro de
## las llamadas a la funcion kalman

##' Gelman-Rubin potential scale reduction factor
##'
##' This function calculates the Gelman-Rubin potential scale
##' reduction factor, usually denoted \eqn{\hat{R}}, for a set of
##' posterior MCMC chains. The diagnostic compares the variance within
##' each chain with the variance between chains. It is a measure of
##' chains stability taken as a proxy of convergence to the same
##' posterior distribution, \eqn{\hat{R}} should be close to 1
##' (i.e. 1.1 or closer).
##'
##' Let \eqn{m} be the number of chains, \eqn{n} the number of
##' iterations per chain, and \eqn{\theta_{ij}} the sampled value from
##' iteration \eqn{i} in chain \eqn{j}.
##'
##' The within-chain variance is
##' \deqn{
##' W = \frac{1}{m} \sum_{j = 1}^{m} s_j^2,
##' }
##' where \eqn{s_j^2} is the sample variance of chain \eqn{j}.
##'
##' The between-chain variance is
##' \deqn{
##' B = n \, \mathrm{Var}(\bar{\theta}_{.j}),
##' }
##' where \eqn{\bar{\theta}_{.j}} is the mean of chain \eqn{j}.
##'
##' The estimated marginal posterior variance is
##' \deqn{
##' \widehat{\mathrm{var}}^+ =
##'     \frac{n - 1}{n} W + \frac{1}{n} B.
##' }
##'
##' The potential scale reduction factor is
##' \deqn{
##' \hat{R} =
##'     \sqrt{\frac{\widehat{\mathrm{var}}^+}{W}}.
##' }
##'
##' Values close to 1 indicate approximate convergence. Values
##' substantially larger than 1 suggest that the chains have not mixed
##' well or are still exploring different regions of the posterior.
##'
##' @title GelmanRubin
##' @param Data A list of posterior MCMC chains. Each element must be a
##'     matrix or data frame with iterations in rows and parameters in
##'     columns. All chains must have the same number of iterations and
##'     parameters.
##' @return A \code{data.frame} with one row per parameter and the
##'     following columns:
##'     \describe{
##'         \item{\code{W}}{Average within-chain variance.}
##'         \item{\code{B}}{Between-chain variance.}
##'         \item{\code{varHat}}{Estimated marginal posterior variance.}
##'         \item{\code{Rhat}}{Potential scale reduction factor.}
##'     }
##' @author Fer Arce
GelmanRubin <- function(Data) {
    if (!is.list(Data)) {
        stop("Data must be a list of MCMC chains.")
    }

    chains <- length(Data)

    if (chains < 2) {
        stop("At least two chains are required.")
    }

    if (!all(vapply(Data, function(x) is.matrix(x) || is.data.frame(x),
                    logical(1)))) {
        stop("Each chain must be a matrix or data.frame.")
    }

    n <- nrow(Data[[1]])
    p <- ncol(Data[[1]])

    if (!all(vapply(Data, nrow, integer(1)) == n)) {
        stop("All chains must have the same number of iterations.")
    }

    if (!all(vapply(Data, ncol, integer(1)) == p)) {
        stop("All chains must have the same number of parameters.")
    }

    if (n < 2) {
        stop("Each chain must have at least two iterations.")
    }

    chainVars <- sapply(Data, function(x) apply(x, 2, var))
    W <- rowMeans(chainVars)

    chainMeans <- sapply(Data, colMeans)
    B <- n * apply(chainMeans, 1, var)

    varHat <- ((n - 1) / n) * W + B / n

    Rhat <- sqrt(varHat / W)
    Rhat[W == 0 & varHat == 0] <- NA

    out <- data.frame(W = W, B = B, varHat = varHat, Rhat = Rhat)

    parNames <- colnames(Data[[1]])
    if (!is.null(parNames)) {
        rownames(out) <- parNames
    }

    out
}
