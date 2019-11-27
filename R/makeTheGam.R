
library(gam)
source('~/phd/scripts/spoon/drift1.r')
f <- drift(M0=100,V0=90,a=1,link="dragp")
f$linkinv(100)


## MakeTheGam <- function(d, plot=TRUE){
##     fit <- gam(rate ~ s(time), data=d,
##                family=drift(M0=100,V0=90,a=1.2,link="dragp"))
##     new <- data.frame(time = floor(min(d$time)):max(d$time))
##     out <- predict(fit,newdata = new, type="response")
##     outd <- data.frame(pred=as.numeric(out), time=new)
##     ##plot(rate~time, data=d,pch=16,cex=1,ylab="Rate",xlab="Hours",ylim=c(-.4,.4))
##     return(outd)
## }


## temporarily moddified for changing the predicted values of gams. Now I have to predict the values while collating bathy and lat and lon


MakeTheGam <- function(test, plot=TRUE){
    fit <- gam(rate ~ s(Date), data=test,
               family=drift(M0=100,V0=90,a=1.2,link="dragp"))
    DtPred <- as.data.frame(readRDS(paste(paste('~/phd/data/bsam/',deploy,'/',deploy,'_behav.RDS', sep = ''))))
    DtPred$ref <- as.character(DtPred$ref)
    DtPred <- DtPred[DtPred$ref == unique(test$ref),]
    DtPred <- DtPred[DtPred$date >= min(test$Date) & DtPred$date <= max(test$Date), ]
    new <- data.frame(Date = DtPred$date)
    out <- predict(fit,newdata = new, type="response")
    outd <- data.frame(pred=as.numeric(out), time=new)
    ##plot(rate~time, data=d,pch=16,cex=1,ylab="Rate",xlab="Hours",ylim=c(-.4,.4))
    return(outd)
}
