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

##' Function for breaking the drift trajectories on the basis of lack
##' of data, not haul outs, for making periods and better fit
##' smoothing functions
##'
##' It creates 
##' @title 
##' @param test a post-processed seal
##' @param days ammount of days without drift dives
##' @return 
##' @author Fer Arce
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
    ##'%out%' <- Negate('%in%')
    Data <- lapply(Data, function(fo) fo <- fo[fo$zetas > zeta, ])
    keep <- sapply(Data, nrow)
    Data <- Data[keep > 20]
    Data <- lapply(Data, function(fo){
        fo$breaks <- c(0,diff(fo$fday))
        return(fo)
    }
    )
    Data <- lapply(Data, SingleDay, date = 'date')
    ##Data <- lapply(Data, makeDate)
    Data <- lapply(Data, MakePeriods, days = days) # here is for changing days for breaking periods
    sapply(Data, function(fo) max(fo$periods))
    return(Data)
}


MakeTheGam <- function(test, plot=TRUE){
    library(gam)
    fit <- gam(rate ~ s(Date), data=test,
               family=drift(M0=100,V0=90,a=1.2,link="dragp"))
    ##     DtPred <- readRDS(paste(paste('~/phd/data/bsam/',deploy,'/',deploy,'_hbsam_full_0.25.RDS', sep = '')))
    ## DtPred <- DtPred[[1]]
    ## DtPred <- as.data.frame(DtPred)
    ## DtPred$ref <- as.character(DtPred$id)
    ## DtPred <- DtPred[DtPred$ref == unique(test$ref),]
    ## DtPred <- DtPred[DtPred$date >= min(test$Date) & DtPred$date <= max(test$Date), ]
    ## new <- data.frame(Date = DtPred$date)
    new <- data.frame(Date = seq(min(test$Date), max(test$Date), by = 60*60*6))
    out <- predict(fit,newdata = new, type="response")
    outd <- data.frame(pred=as.numeric(out), time=new)
    ##plot(rate~time, data=d,pch=16,cex=1,ylab="Rate",xlab="Hours",ylim=c(-.4,.4))
    return(outd)
}






