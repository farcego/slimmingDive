##' Function that takes a subset of variables and do some recoding and
##' dive removal for filtering purposes
##'
##' This functions keeps the variables needed to apply the subsequents
##' procedures of this package.  It will make the Date of class
##' \code{POSIXct} if supplied as a factor and will make the
##' \code{ref} (id of the tag) of type character. It also sorts the
##' rows by \code{ref} and by \code{Date}.  It also removes any
##' duplicated dive and those not reaching 100 meter depth and shorter
##' than 300 seconds.
##' @title formatDives
##' @param Data an object of class daata.frame
##' @param min.dur minimun duration of a dive to be considered as a potential drift dive (in seconds)
##' @param min.dep minimum 'maximum depth for a given dive, acting as a threshold
##' @return a data.frame containing a subset of the variables needed
##'     to keep processing the data.frame
##' @examples
##' data(ele)
##' formatDives(ele)
formatDives <- function(Data, min.dur = 500, min.dep = 100){
    '%out%' <- Negate('%in%')
    ## New addition, it potentially may break code, 21/nov/2019
    names(Data) <- tolower(names(Data))
    Data$ref <- as.character(Data$ref)
    Data <- Data[, c('ref', 'de_date', 'surf_dur',
                     'dive_dur', 'max_dep', 'd1',
                     'd2', 'd3','d4','t1','t2','t3',
                     't4', 'lat','lon')]
    Data <- Data[Data$dive_dur > min.dur & Data$max_dep > min.dep, ]
    names(Data) <- c('ref', 'DE_DATE', 'SURF_DUR',
                     'DIVE_DUR', 'MAX_DEP', 'D1',
                     'D2', 'D3','D4','T1','T2','T3',
                     'T4', 'lat','lon')
    ## end of the addition
    Data$ref <- as.character(Data$ref) #keep for R versions lower than 4.* just in case
    ## new deletion, 21/nov/2019
    ## Data <- Data[,c("ref", "DE_DATE", "SURF_DUR", "DIVE_DUR","MAX_DEP","D1","D2",
    ##                 "D3", "D4","T1", "T2","T3","T4")]
    ## 
    ## end of deletion
    
    if ('POSIXct' %out% class(Data$DE_DATE)){
        Data$Date <- as.POSIXct(strptime(as.character(Data$DE_DATE),
                                         format = '%d/%m/%y %H:%M:%S'))
    } else {
        Data$Date <- Data$DE_DATE
    }
    Data$DE_DATE <- NULL
    
    
    Data$t1 <- Data$T1
    Data$t2 <- Data$T2
    Data$t3 <- Data$T3
    Data$t4 <- Data$T4
    Data$T1 <- (Data$t1*Data$DIVE_DUR) / 100
    Data$T2 <- (Data$t2*Data$DIVE_DUR) / 100
    Data$T3 <- (Data$t3*Data$DIVE_DUR) / 100
    Data$T4 <- (Data$t4*Data$DIVE_DUR) / 100
    Data <- Data[order(Data$ref,Data$Date), ]
    ## Data <- Data[Data$DIVE_DUR > 300 & Data$MAX_DEP > min.dep
    ##              & Data$T1 > 0, ]
    Data <- Data[Data$T1 > 0, ]
    Data <- Data[Data$T1 < Data$T2, ]
    Data <- Data[Data$T2 < Data$T3, ]
    Data <- Data[Data$T3 < Data$T4, ]
    ## Data <- Data[order(Data$Date), ]
    Data <- Data[!duplicated(Data), ]
    return(Data)
}
