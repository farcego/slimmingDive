##' Get the time-stamp at maximum depth
##'
##' Get the time at which the maximum depth of a given dive is
##' reached.
##' @title maximum time
##' @param x a given dive containing the times and depths of the
##'     broken-stick algoritm.
##' @return the time when the maximi depth is reached. A \code{vector}
##'     of class code{numeric} and length one.
maxTime <- function(x){
    data.depths <- x[c('D1','D2','D3','D4')]
    data.times <- x[c('T1','T2','T3','T4')]
    l <- which.max(data.depths)
    l <- as.numeric(l)
    maxi.time <- as.numeric(data.times[l])
    return(maxi.time)
}



##' Function for generating the variables required to apply the filtering process. This function has been tested with tables extracted from the Integrated Marine Observing System (IMOS, Australia) freely available data, and it should work with any Sea-mammal Research Unit (SMRU, St. Andrews, Scotland) provided file. please contact the author if it does not work with your data to find a custom adaptation suitable for your data.
##'
##' Newly computed variables: \cr
##' \itemize{
##' \item descspeed: initial descending speed (from surface to the 1 infl point)
##' \item ascspeed: last ascending speed (from the 4 infl point to the surface)
##' \item daratio: the ratio between both speeds
##' \item max.depth: maximum depth (it's duplicated but both names are required)
##' \item min.depth: minimum depth of the inflection points
##' \item max.time: time spend before reaching max.depth
##' \item avratio: Deviation of the max.depth point from the middle of the inflected dive
##' \item hp1:  length of the first segment
##' \item hp2:  length of the second segment
##' \item hp3:  length of the third segment
##' \item Sa: for getting avratio
##' \item Sb: for getting avratio
##' \item d1: ratio between max.depth and the depth at the given inflection point (=1 if the given inflection point holds the max.depth). This calculates the first
##' \item d2: same ratio for the second inflection point
##' \item d3: same ratio for the third inflection point
##' \item d4: same ratio for the fourth inflection point
##' \item modres1: residual from a fitted linear model along the inflection points.Value for the first inflection point
##' \item modres2: for the second inflection point
##' \item modres3: for the third inflection point
##' \item modres4: for the fourth inflection point
##' \item propseg1: proportion of the dive along the first segment
##' \item propseg2: proportion of the dive along the second segment
##' \item propseg3: proportion of the dive along the third segment
##' }
##' @title New variable generator
##' @param Data an object of class 'data.frame' containing at least the Broken-stick output ([T1...T4], [D1...D4]), dive duration (DIVE_DUR)
##' @param t Logical. wheter t1...t4 exist on the Data or not. If FALSE, they are computed
##' @return this function returns the provided dataset with newly added columns named as variables described above, with the calculations.
NewVarsVect <- function(Data = Data, t = FALSE){
    a.n <- as.numeric
    if (t == FALSE){
        Data$t1 <- (Data$T1 / Data$DIVE_DUR) * 100
        Data$t2 <- (Data$T2 / Data$DIVE_DUR) * 100
        Data$t3 <- (Data$T3 / Data$DIVE_DUR) * 100
        Data$t4 <- (Data$T4 / Data$DIVE_DUR) * 100
    }
    Data$descspeed <- Data$D1/Data$T1 
    Data$ascspeed <- Data$D4/(Data$DIVE_DUR - Data$T4)
    Data$daratio <- Data$descspeed/Data$ascspeed
    Data$max.depth <- apply(Data[c('D1','D2','D3','D4')],1,'max')
    Data$min.depth <- apply(Data[c('D1','D2','D3','D4')],1,'min')
    Data$max.time <- apply(Data,1,maxTime)
    Data$avratio <- numeric(nrow(Data))
    Data$avratio <- apply(Data,1,AvRatio)
    Data$mdr <- numeric(nrow(Data))
    Data$mdr <- apply(Data,1,ModRes)
    Data$sp1 <- (Data$D2 - Data$D1) / (Data$T2 - Data$T1)
    Data$sp2 <- (Data$D3 - Data$D2) / (Data$T3 - Data$T2)
    Data$sratio <- Data$descspeed/((Data$D2-Data$D1)/(Data$T2-Data$T1))
    Data$d1 <- Data$D1/Data$max.depth
    Data$d2 <- Data$D2/Data$max.depth
    Data$d3 <- Data$D3/Data$max.depth
    Data$d4 <- Data$D4/Data$max.depth
    Data$meand <-apply(Data[c('d1','d2','d3','d4')],1,mean)
    Data$sdd <- apply(Data[c('d1','d2','d3','d4')],1,sd)
    Data$propseg1 <- Data$t2 - Data$t1
    Data$propseg2 <- Data$t3 - Data$t2
    Data$propseg3 <- Data$t4 - Data$t3
    Data$mrratio <- as.numeric(Data$minresid) / as.numeric(Data$max.depth)
    Data$mdepthr <- mean(c(Data$D1,Data$D2,Data$D3,Data$D4))/Data$max.depth
    Data$mdepthr1 <- apply(Data,1,Mdepthr)
    Data$mdepthbias <- (Data$max.time - (Data$DIVE_DUR/2)) / Data$DIVE_DUR
    Data$hp1 <- sqrt((abs(Data$D1-Data$D2))^2 + (Data$T2 - Data$T1)^2)
    Data$hp2 <- sqrt((abs(Data$D2-Data$D3))^2 + (Data$T3 - Data$T2)^2)
    Data$hp3<- sqrt((abs(Data$D3-Data$D4))^2 + (Data$T4 - Data$T3)^2)
    return(Data)
}

##' MdepthR
##'
##' calculates mdepthr
##' @title MdepthR
##' @param x a vector
##' @return a value
Mdepthr <- function(x){
    a.n <- as.numeric
    dd <- a.n(x[c('D1','D2','D3','D4')])
    mdept <- mean(dd)/a.n(x['max.depth'])
    return(mdept)
}

    

##' Function for calculating the avratio of a summarized dive.
##'
##' It actually looks bugged. that's probably why It has not been properly working for dive selection, but it may affect other functions like.
##' Yes, it is used in in NDE local function. It should calculate the deviaion of the deepest point from the center of the dive
##' @title Mid point ratio
##' @param x A summarized dive profile (summarized by the Broken Stick algorithm)
##' @return  a numeric value
AvRatio <- function(x){
    avratio=numeric()
    a.n <- as.numeric
    x['Sa'] <- (a.n(x['max.depth']) / a.n(x['max.time']))
    x['Sb'] <- (a.n(x['max.depth']) / (a.n(x['DIVE_DUR']) - a.n(x['max.time'])))
    if (x['Sa'] > x['Sb']){
        avratio = a.n(x['Sa']) / a.n(x['Sb']) - 1
    } else if (x['Sa'] < x['Sb']){
        avratio = (-(a.n(x['Sb']) / a.n(x['Sa']))) + 1
    } else {
        avratio=1
    }
    return(avratio)
}





## function for generating the residuals of a fitted linear model to the BSM points
##'
##' ModRes generates the residuals of a linear model by fitting the depth points (D1..D4) vs the time points (T1..T4). While the dimension may not be of importance, the sign of the residuals may be for some criteria. It only returns the residual values, not the full object of class 'lm'.
##' @title Least square residuals
##' @param x a summarized dive
##' @param res  wich residual is requested \cr
##' \itemize{
##' \item If {1,2,3,4} it will return the residual for the {1,2,3,4} inflection points
##' \item {5} all residuals pasted into a string separated by dots
##' }
##' @return A numeric value if a single residual is requested, or a character string if all are requested
ModRes <- function(x,res=5){
    a.n <- as.numeric
    dd <- a.n(x[c('D1','D2','D3','D4')])
    dt <- a.n(x[c('T1','T2','T3','T4')])
    tmp.mod <- lm(dd~dt)
    tmp.res <- tmp.mod$residuals
    if (res == 1){
        return(a.n(tmp.mod$residual[1]))
    } else if (res == 2){
        return(a.n(tmp.mod$residual[2]))
    } else if (res == 3){
        return(a.n(tmp.mod$residual[3]))
    } else if (res == 4){
        return(a.n(tmp.mod$residual[4]))
    } else if (res == 5){
        paste(a.n(tmp.mod$residuals),collapse='.')
    }
}
