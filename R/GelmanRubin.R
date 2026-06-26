##' Function for calculating the Gelman rubin potential scale reduction factor
##' 
##' this functions mplements a smallish version of the gelman rubin diagnosis test. is faster than the one on coda package. It has pending some stuff, like what to do with binary variables, i mean, for calculatin the potential scale reduction factor when is 0/0
##' @title GelmanRubin
##' @param Data a list with posterior MCMC chains
##' @return 
##' @author fernando Arce
GelmanRubin <- function(Data){
    chains <- length(Data)                  # number of chains
    V <- sapply(Data,apply,2,var)        # variance from each chain
    V <- cbind(V,rowMeans(V))               #averaged variance from each chain
    vtot <- apply(do.call(rbind, Data),2,var) # variance of the mixed chains
    V <- cbind(V,vtot)
    gr <- sqrt(V[,5]/V[,4])                 # potential scale reduction factor
    V <- cbind(V,gr)
    chain.names <- paste('v',1:chains,sep='')
    colnames(V) <- c(chain.names,'av','vtot','gr')
    ## for convinience return a data.frame
    v <- as.data.frame(V)
    return(v)
}
