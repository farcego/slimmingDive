library(slimmingDive)
source('linkFun.R')




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

##' Function for breaking the drift trajectories on the basis of lack of data, not haul outs
##'
##' .. content for \details{} ..
##' @title 
##' @param test 
##' @param days 
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



data(elek)

Data <- postKalman(elek)
if('data.frame' %in% class(Data))
    Data <- list(Data)

for(i in 1:length(Data)){
    Data[[i]] <- Data[[i]][order(Data[[i]]$Date), ]
    Data[[i]] <- Data[[i]][!duplicated(Data[[i]]$Date), ]
}
Data <- lapply(Data, daysTemp)

## works with lists
Data <- PostKalProc(Data, zeta = .5)


Gams <- list()
for(i in 1:length(Data)){
    test <- Data[[i]]
    ## test <- Data[[i]]
    ## foca <- unique(test$ref)
    ## ts <- min(test$date)
    ## test$d <- test$fday - min(test$fday)
    ## test$dd <- floor(test$d)
    ## ##foca <- unique(test$ref)
    ## inicio <- min(test$date)
    test$rate <- test$NDE
    test$time <- test$day
    test <- split(test, test$periods)
    minm = 10
    test <- test[sapply(test, nrow) > minm]
    ## test$Date is the good date
    Gamss <- lapply(test, MakeTheGam)
    for (l in 1:length(Gamss)){
        Gamss[[l]]$ref <- foca
        ## Gamss[[l]]$date <- inicio +Gamss[[l]]$time*86400
        Gamss[[l]]$dif <- c(NA, diff(Gamss[[l]]$pred))
    }
    Gams <- c(Gams, list(Gamss))
}

str(gams)


gams <- Gams[[1]]

st <- min(gams[[1]]$Date)

plot(0,0, col = 'white', ylim = c(-.45,.3), xlim = c(st, st + 360*86400))


po <- do.call(rbind, Data)

plot(as.numeric(po$Date), po$NDE, ylim = c(-.45, .2))
lapply(gams, function(fo) points(fo$Date, fo$pred, lwd = 4, lty = 1,type = 'l', col = 'red'))


g <- list()
for(i in 1:length(Gams)){
    g[[i]] <-  do.call(rbind, Gams[[i]])
}

g <- g[[1]]

plot(g$pred, g$date)
