kalman <- function(Data, update=4000, n.iter=5000, n.chains=1,
                   n.adapt=1000, parallel = FALSE, thinStep = 10){

    ## Sort by date
    Data <- Data[order(Data$Date), ]
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
    logLikelihood <- function(i, delta_i, delta_prev, delta_next, rate_obs, time, z_i){
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
            logLik_current <- logLikelihood(i, delta_current, delta_prev, delta_next, Data$NDE[i], Data$time, z[i])
            logLik_proposal <- logLikelihood(i, proposal, delta_prev, delta_next, Data$NDE[i], Data$time, z[i])
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
