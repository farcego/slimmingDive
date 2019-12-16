##' Function to retrieve latent variables of the BSM (values that were
##' generated onboard the tag but has not been transmitted)
##'
##' This function is based on the recursively application of linear
##' models through each summarized dive to get the order in which the
##' inflection points have been selected by the BSM. This function,if
##' requested, will return the smallest residual that may or may not
##' be the residual of the last point. It usually is, but not
##' always. This functions works at dive level, and asume some naming
##' conventions from the SMRU generated files. The column names
##' requested are: D1,D2,D3,D4,T1,T2,T3,T4 and DIVE_DUR. This funcion
##' is not intended to be used directly by an end user, but if needed
##' to be applied directly trough a data.frame object, function RBSM
##' should be called via apply if want to be applied to a full
##' data.frame.
##' @title Reverse Broken Stick Algorithm
##' @param dive A dive profile summarized by a Broken Stick Algorithm
##'     and transmitted via ARGOS (SMRU format)
##' @param retrieve character determining what we want to have
##' returned of: \cr
##' \itemize{
##' \item order: It returns the order of selection of the BSM
##' inflection points
##' \item minresid: It returns the smallest residual
##' \item both: it returns both
##' \item all: returns all four residuals and the order
##' }
##' @return the exact content will depended on the flag used with
##'     retrieve (see above). The returned values are stored in a
##'     vector. If more than one outcome is requested via both,
##'     minresid will be of class character
##' @export
RBSM <- function(dive,retrieve='order'){
    nd <- matrix(NA,ncol=6,nrow=6)
    colnames(nd) <- c('depth','time','order','resid','tmp.resd',
                      'pred.depth')
    nd[,1] <-c(10,as.numeric(dive[c('D1','D2','D3','D4')]),10)
    nd[,2] <- c(0,as.numeric(dive[c('T1','T2','T3','T4','DIVE_DUR')]))
    ## first point and itsa residual
    nd[which.max(nd[,1]),3] <- 1
    nd[which.max(nd[,1]),4] <- max(nd[,1])
    ## second point
    sp <- which(nd[,3]>0)
    ## first subData'point''before-after'
    if (length(1:sp[1]) == 2) {
        nd[1:sp[1],6] <- nd[1:sp[1],1]
        nd[1:sp[1],5] <- 0
    } else {
        sd1 <- nd[c(1,sp[1]),]
        lmsd1 <- lm(sd1[,1]~sd1[,2])
        nd[1:sp[1],6] <- lmsd1$coefficients[1] +
            lmsd1$coefficients[2]*nd[1:sp[1],2]
    }
    if (length(sp[1]:6) == 2) {
        nd[sp[1]:6,6] <- nd[sp[1]:6,1]
        nd[sp[1]:6,5] <- 0
    } else {
        sd2 <- nd[c(sp[1],6), ]
        lmsd2 <- lm(sd2[,1]~sd2[,2])
        nd[sp[1]:6,6] <- lmsd2$coefficients[1] +
            lmsd2$coefficients[2]*nd[sp[1]:6,2]
    }    
    nd[,5] <- round(abs(nd[,6] - nd[,1]),4)
    nd[which.max(nd[,5]),3] <- 2
    nd[which.max(nd[,5]),4] <- nd[which.max(nd[,5]),5]
    ## third point
    sp <- which(nd[,3]>0)

    if (length(1:sp[1]) == 2) {
        nd[1:sp[1],6] <- nd[1:sp[1],1]
        nd[1:sp[1],5] <- 0
    } else {
        sd1 <- nd[c(1,sp[1]),]
        lmsd1 <- lm(sd1[,1]~sd1[,2])
        nd[1:sp[1],6] <- lmsd1$coefficients[1] +
            lmsd1$coefficients[2]*nd[1:sp[1],2]
    }
    if (length(sp[1]:sp[2]) == 2) {
        nd[sp[1]:sp[2],6] <- nd[sp[1]:sp[2],1]
        nd[sp[1]:sp[2],5] <- 0
    } else {
        sd2 <- nd[c(sp[1],sp[2]), ]
        lmsd2 <- lm(sd2[,1]~sd2[,2])
        nd[sp[1]:sp[2],6] <- lmsd2$coefficients[1] +
            lmsd2$coefficients[2]*nd[sp[1]:sp[2],2]
    }
    if (length(sp[2]:6) == 2) {
        nd[sp[2]:6,6] <- nd[sp[2]:6,1]
        nd[sp[2]:6,5] <- 0
    } else {
        sd2 <- nd[c(sp[2],6), ]
        lmsd2 <- lm(sd2[,1]~sd2[,2])
        nd[sp[2]:6,6] <- lmsd2$coefficients[1] +
            lmsd2$coefficients[2]*nd[sp[2]:6,2]
    }
    nd[,5] <- round(abs(nd[,6] - nd[,1]),4)
    nd[which.max(nd[,5]),3] <- 3
    nd[which.max(nd[,5]),4] <- nd[which.max(nd[,5]),5]
    ## last point
    last <- which(is.na(nd[,3]))[2]
    sd1 <- nd[c(last-1,last+1),]
    lmsd1 <- lm(sd1[,1]~sd1[,2])
    nd[last,6] <- lmsd1$coefficients[1] +
        lmsd1$coefficients[2]*nd[last,2]
    nd[last,3] <- 4
    nd[last,4] <- round(abs(nd[last,6] - nd[last,1]),4)   
    nd <- nd[-c(1,6),]
    ## I have to generalize resids
    bsmorder <- c(nd[,3])
    bsmstring <- paste(bsmorder,collapse='.')
    bsmresids <- c(nd[,4])
    retained <- c(bsmstring)
    if (retrieve=='order') {
        return(retained)
    } else if (retrieve=='resid'){
        return(min(bsmresids))
    } else if (retrieve=='both'){
        return(c(retained,round(min(bsmresids),3)))
    } else if (retrieve=='all'){
        return(c(retained,round(bsmresids,3)))
    }
}
