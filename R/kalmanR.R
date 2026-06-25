##' Run the Kalman MCMC model for drift dives
##'
##' Fits the drift-dive state-space model originally implemented in JAGS using
##' a Metropolis-Hastings/Gibbs MCMC sampler. The function estimates latent mass
##' increments and classifies observations as drift or non-drift dives through
##' a latent binary indicator.
##'
##' @param Data A \code{data.frame} containing at least \code{Date} and
##'     \code{NDE}. \code{Date} must be coercible to numeric time and
##'     \code{NDE} must be numeric.. The \{data.frame} must be sorted by Date.
##' @param update Integer. Number of initial MCMC iterations discarded as
##'     burn-in.
##' @param n.iter Integer. Total number of MCMC iterations.
##' @param n.chains Integer. Number of MCMC chains. Currently only a single
##'     chain is implemented and this argument is retained for compatibility.
##' @param n.adapt Integer. Length of the adaptation period. Currently retained
##'     for compatibility but not used.
##' @param parallel Logical. Retained for compatibility but not used.
##' @param thinStep Integer. Thinning interval applied after burn-in.
##'
##' @return An object of class \code{kalman}, a list containing:
##' \describe{
##'   \item{\code{deltaSamples}}{Posterior samples of the latent mass increments.}
##'   \item{\code{zSamples}}{Posterior samples of the latent drift-dive indicator.}
##'   \item{\code{Data}}{The input data ordered by date, with an additional
##'   \code{time} column.}
##'   \item{\code{burnin}}{Number of burn-in iterations.}
##'   \item{\code{n.iter}}{Total number of MCMC iterations.}
##'   \item{\code{thin}}{Thinning interval.}
##' }
##'
##' @details
##' Historically this function has been referred to as a "Kalman" filter within
##' the package. However, the underlying model is not a classical Kalman filter.
##' Instead, it is a Bayesian nonlinear state-space model in which inference is
##' performed using a Metropolis-Hastings/Gibbs MCMC sampler. The name
##' \code{kalman()} is retained for backwards compatibility with previous
##' versions of the package.
##'
##' The latent process is modelled as a random walk with time-dependent process
##' variance. The observation model links latent mass increments to observed
##' drift rates through a nonlinear function, while a latent binary variable
##' assigns observations to either a high- or low-precision observation model.
##'
##' @seealso \code{\link{plotDrift}}
##'
##' @export
kalman <- function(Data, update=4000, n.iter=5000, n.chains=1,
                   n.adapt=1000, parallel = FALSE, thinStep = 10){





    
    ## make time fully numeric
    Data$time <- (as.numeric(Data$Date) - as.numeric(min(Data$Date))) / 3600
    
    ## Constants
    m0 <- 100
    v0 <- 90
    V <- 1.1
    tau0 <- 1
    tau1 <- 10000
    a <- -1.2
    taud <- 1
    p <- 90000 / (90000 + 10000)
    N <- nrow(Data)
    
    ## storage chains
    deltaChain <- matrix(NA, nrow=n.iter, ncol=N)
    zChain <- matrix(NA, nrow=n.iter, ncol=N)
    
    ## Initialize
    delta <- rep(0, N)
    z <- rep(0, N)
    
    ## Helper --> sacar fuera de aqui, en su propia funcion, _i I, _prev P...
    
    ## asi deberia empezar esta funcion... hay que sacar todos estos parameteros de algun modo, y que lo smetamos con un list parameters, a lo jags...
    ## lLl <- function(i,
    ##                           delta_i, #Di
    ##                           delta_prev, #Dp
    ##                           delta_next, # Dn
    ##                           rate_obs, # Ro
    ##                           time,
    ##                           z_i, # Zi
    ##                           m0,
    ##                           v0,
    ##                           V,
    ##                           a,
    ##                           tau0,
    ##                           tau1,
    ##                           taud)

    
    ##' Compute the log-likelihood contribution for a single latent state
    ##'
    ##' Computes the contribution of a single latent mass increment
    ##' (\code{delta[i]}) to the posterior log-likelihood. The contribution
    ##' consists of two components: (1) the random-walk process model linking
    ##' consecutive latent states, and (2) the observation model linking the
    ##' latent state to the observed drift rate.
    ##'
    ##' @param i Integer. Index of the current observation.
    ##' @param delta_i Numeric. Proposed value of the latent mass increment
    ##'     at observation \code{i}. {{seems more like the buoyancxy increment, not the massss, check elsewhere}}
    ##' @param delta_prev Numeric. Latent mass increment at observation
    ##'     \code{i - 1}.
    ##' @param delta_next Numeric. Latent mass increment at observation
    ##'     \code{i + 1}.
    ##' @param rate_obs Numeric. Observed drift rate for observation
    ##'     \code{i}.
    ##' @param time Numeric vector giving the observation times (hours since
    ##'     the first observation).
    ##' @param z_i Integer (0 or 1). Latent indicator specifying whether the
    ##'     observation belongs to the high-precision (\code{1}) or
    ##'     low-precision (\code{0}) observation model.
    ##'
    ##' @details
    ##' The process model assumes that the latent mass increments follow a
    ##' Gaussian random walk whose precision depends on the elapsed time
    ##' between consecutive observations. The observation model computes the
    ##' expected drift rate from the current latent state through the
    ##' nonlinear buoyancy model
    ##'
    ##' \deqn{
    ##' \rho = \frac{m_0 + \delta}{v_0 + V\delta}
    ##' }
    ##'
    ##' and
    ##'
    ##' \deqn{
    ##' \mu = a\,s\,\sqrt{|\rho - 1|}
    ##' }
    ##'
    ##' where \eqn{s} is the sign of \eqn{\rho - 1}. The observation variance
    ##' depends on the latent state indicator \code{z_i}.
    ##'
    ##' This function returns only the log-likelihood contribution associated
    ##' with the current latent state and is intended for use within the
    ##' Metropolis-Hastings update of the Kalman MCMC sampler.
    ##'
    ##' @return A numeric scalar giving the log-likelihood contribution of the
    ##' current latent state.
    ##'
    ##' @keywords internal
    lLl <- function(i, delta_i, delta_prev, delta_next, rate_obs, time, z_i){
        logp_delta <- 0
        if (i > 1) {
            dt_prev <- time[i] - time[i-1]
            taudt_prev <- taud / dt_prev
            logp_delta <- logp_delta + dnorm(delta_i, mean=delta_prev, sd=sqrt(1/taudt_prev), log=TRUE)
        }
        if (i < N) {
            dt_next <- time[i+1] - time[i]
            taudt_next <- taud / dt_next
            logp_delta <- logp_delta + dnorm(delta_next, mean=delta_i, sd=sqrt(1/taudt_next), log=TRUE)
        }
        rho <- (m0 + delta_i) / (v0 + V * delta_i)
        s <- ifelse(rho > 1, 1, ifelse(rho < 1, -1, 0))
        mu <- a * s * sqrt(abs(rho - 1))
        tauObs <- tau0 + tau1 * z_i
        logp_obs <- dnorm(rate_obs, mean=mu, sd=1/sqrt(tauObs), log=TRUE)
        return(logp_delta + logp_obs)
    }
    
    ## MCMC
    for (iter in 1:n.iter){
        for (i in 1:N){
            delta_current <- delta[i]
            proposal <- delta_current + rnorm(1, 0, 0.1)
            delta_prev <- if (i > 1) delta[i-1] else 0
            delta_next <- if (i < N) delta[i+1] else 0
            logLik_current <- lLl(i, delta_current, delta_prev, delta_next, Data$NDE[i], Data$time, z[i])
            logLik_proposal <- lLl(i, proposal, delta_prev, delta_next, Data$NDE[i], Data$time, z[i])
            log_alpha <- logLik_proposal - logLik_current
            if (log(runif(1)) < log_alpha){
                delta[i] <- proposal
            }
        }
        
        for (i in 1:N){
            rho <- (m0 + delta[i]) / (v0 + V * delta[i])
            s <- ifelse(rho > 1, 1, ifelse(rho < 1, -1, 0))
            mu <- a * s * sqrt(abs(rho - 1))
            logp_z0 <- dnorm(Data$NDE[i], mean=mu, sd=1/sqrt(tau0), log=TRUE) + log(1-p)
            logp_z1 <- dnorm(Data$NDE[i], mean=mu, sd=1/sqrt(tau0+tau1), log=TRUE) + log(p)
            p_z1 <- 1 / (1 + exp(logp_z0 - logp_z1))
            z[i] <- rbinom(1, size=1, prob=p_z1)
        }
        
        deltaChain[iter, ] <- delta
        zChain[iter, ] <- z
        
        if (iter %% 500 == 0){
            cat('Iteration', iter, 'of', n.iter, '\n')
        }
    }
    
    ## Post-processing
    burn <- min(floor(update), n.iter - 100)  # never larger than n.iter - 100
    if (burn >= (n.iter - 100)) burn <- 0  # if somehow invalid
    ## thinStep <- 10
    keep <- seq(from=burn+1, to=n.iter, by=thinStep)

    deltaSamples <- deltaChain[keep, , drop=FALSE]
    zSamples <- zChain[keep, , drop=FALSE]
    
    res <- list(
        deltaSamples = deltaSamples,
        zSamples = zSamples,
        Data = Data,
        burnin = burn,
        n.iter = n.iter,
        thin = thinStep
    )
    
    attr(res, 'update.type') <- 'kalmanR'
    class(res) <- c('kalman', class(res))
    
    return(res)
}
