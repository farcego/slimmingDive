##' Function that takes a subset of variables and do some recoding and dive removal for filtering purposes
##'
##' This functions keeps the variables needed to apply the subsequents procedures of this package.
##' It will make the Date of class POSIXct if supplied as a factor and will make the ref (id of the tag) of type character. It also sorts the rows by tag id (ref) and by date (Date).
##' It also removes dives not reaching 100 meter depth and shorter than 300 seconds.
##' @title formatDives
##' @param Data an object of class daata.frame
##' @return a data.frame containing a subset of the variables needed to keep processing the data.frame
formatDives <- function(Data, format){
    '%out%' <- Negate('%in%')
    Data <- Data[,c("ref", "DE_DATE", "SURF_DUR", "DIVE_DUR","MAX_DEP","D1","D2",
                    "D3", "D4","T1", "T2","T3","T4")]
    if ('POSIXct' %out% class(Data$DE_DATE)){
        Data$Date <- as.POSIXct(strptime(as.character(Data$DE_DATE), format = '%d/%m/%y %H:%M:%S'))
    } else {
        Data$Date <- Data$DE_DATE
    }
    Data$DE_DATE <- NULL
    Data$ref <- as.character(Data$ref)
    Data$t1 <- Data$T1
    Data$t2 <- Data$T2
    Data$t3 <- Data$T3
    Data$t4 <- Data$T4
    Data$T1 <- (Data$t1*Data$DIVE_DUR) / 100
    Data$T2 <- (Data$t2*Data$DIVE_DUR) / 100
    Data$T3 <- (Data$t3*Data$DIVE_DUR) / 100
    Data$T4 <- (Data$t4*Data$DIVE_DUR) / 100
    Data <- Data[order(Data$ref,Data$Date), ]
    Data <- Data[Data$DIVE_DUR > 300 & Data$MAX_DEP > 100
               & Data$T1 > 0, ]
    return(Data)
}
