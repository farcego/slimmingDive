## library(slimmingDive)
## source('R/linkFun.R')




daysTemp <- function(Data){
        Data$date <- Data$Date
        Data$fday <- as.numeric(difftime(Data$Date,min(Data$Date),  units='secs'))/86400
        Data$nday <- floor(Data$fday)
        Data$ref <- as.character(Data$ref)
    return(Data)
}






SingleDay <- function(test, date = 'date'){
    test$date <- substr(as.character(test[,date]), start = 1, stop = 10)
    test$date <- as.POSIXct(strptime(test[,date], format = '%Y-%m-%d'))
    return(test)
}


MakePeriods <- function(test, days = 5){
    test$periods <- 1
    ##if(nrow(test) < 10) next
    for(i in 2:nrow(test)){
        if(test$breaks[i] < days){
            test$periods[i] <- test$periods[i-1]
        } else {
            test$periods[i] <- test$periods[i-1]+1
        }
    }
    return(test)
}


PostKalProc <- function(Data, days = 10, zeta = 0.5){
    Data <- lapply(Data, function(fo) fo <- fo[fo$zetas > zeta, ])
    keep <- sapply(Data, nrow)
    Data <- Data[keep > 20]
    Data <- lapply(Data, function(fo){
        fo$breaks <- c(0,diff(fo$fday))
        return(fo)
    }
    )
    Data <- lapply(Data, SingleDay, date = 'date')
    Data <- lapply(Data, MakePeriods, days = days) # here is for changing days for breaking periods
    sapply(Data, function(fo) max(fo$periods))
    return(Data)
}


## MakeTheGam <- function(test, plot=TRUE){
##     fit <- gam(rate ~ s(Date), data=test,
##                family=drift(M0=100,V0=90,a=1.2,link="dragp"))
##     ##     DtPred <- readRDS(paste(paste('~/phd/data/bsam/',deploy,'/',deploy,'_hbsam_full_0.25.RDS', sep = '')))
##     ## DtPred <- DtPred[[1]]
##     ## DtPred <- as.data.frame(DtPred)
##     ## DtPred$ref <- as.character(DtPred$id)
##     ## DtPred <- DtPred[DtPred$ref == unique(test$ref),]
##     ## DtPred <- DtPred[DtPred$date >= min(test$Date) & DtPred$date <= max(test$Date), ]
##     ## new <- data.frame(Date = DtPred$date)
##     new <- data.frame(Date = seq(min(test$Date), max(test$Date), by = 60*60*6))
##     out <- predict(fit,newdata = new, type="response")
##     outd <- data.frame(pred=as.numeric(out), time=new)
##     ##plot(rate~time, data=d,pch=16,cex=1,ylab="Rate",xlab="Hours",ylim=c(-.4,.4))
##     return(outd)
## }

## function in progress


MakeTheGam <- function(test, dates = NULL){
    fit <- gam(rate ~ s(Date), data=test,
               family=drift(M0=100,V0=90,a=1.2,link="dragp"))
    if (is.null(dates)){
        new <- data.frame(Date = seq(min(test$Date), max(test$Date), by = 60*60*6))
    } else {
        DtPred <- dates[dates >= min(test$Date) & dates <= max(test$Date)]
        new <- data.frame(Date = DtPred)
    }
    out <- predict(fit,newdata = new, type="response")
    outd <- data.frame(pred=as.numeric(out), time=new)
    ##plot(rate~time, data=d,pch=16,cex=1,ylab="Rate",xlab="Hours",ylim=c(-.4,.4))
    return(outd)
}




##' function for fitting a gam with a custom link function
##'
##' This function fit a gam with a custom link function. It allows to
##' get estimates of Drift rate at any given time, provided that the
##' seal is drifting. This function may break the drifting trajectory
##' into sub-units on the basis of lack of Drift dives.
##' @title makeTheGam
##' @param Data drifta
##' @param dates dates
##' @param zetas zetas
##' @return a list with predicted values
##' @author Fer Arce
makeTheGam <- function(Data, dates = NULL, zetas = .5){
    
    if('data.frame' %in% class(Data))
        Data <- list(Data)

    for(i in 1:length(Data)){
        Data[[i]] <- Data[[i]][order(Data[[i]]$Date), ]
        Data[[i]] <- Data[[i]][!duplicated(Data[[i]]$Date), ]
    }
    Data <- lapply(Data, daysTemp)

    ## works with lists
    Data <- PostKalProc(Data, zeta = zetas)

    ##    dateS <- seq(min(test[[1]]$Date), max(test[[3]]$Date), by = 60*60*12)

    Gams <- list()
    for(i in 1:length(Data)){
        test <- Data[[i]]
        test$rate <- test$NDE
        test$time <- test$day
        test <- split(test, test$periods)
        minm = 10
        test <- test[sapply(test, nrow) > minm]
        ## test$Date is the good date
        ## need to set up a new dates for test
        Gamss <- lapply(test, MakeTheGam, dates = dates)
        for (l in 1:length(Gamss)){
            ##Gamss[[l]]$ref <- foca          #
            ## Gamss[[l]]$date <- inicio +Gamss[[l]]$time*86400
            Gamss[[l]]$dif <- c(NA, diff(Gamss[[l]]$pred))
        }
        Gams <- c(Gams, list(Gamss))
    }
    Gams <- Gams[[1]]
    return(Gams)
}
