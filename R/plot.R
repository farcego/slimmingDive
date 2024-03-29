##' Change the background of the plot
##'
##' This function takes the 'actual' range of the plotting region (R
##' adds 4% extra to the range of the variables) and draw a rectangle
##' with the desired color. This function is made to be called by
##' \code{panel.first} argument inside plot function.  It should take
##' any color in any format supported by R
##' @title BGC: back-ground-color
##' @param col name of the color in any format supported by R.
##' @param alpha numeric, from 0 to 1. It sets the transparency of the
##'     color
##' @return it will add colour to the background of a plot
##' @noRd
BGC <- function(col = 'bisque', alpha = 1){
    rect(par('usr')[[1]],
         par('usr')[[3]],
         par('usr')[[2]],
         par('usr')[[4]],
         col = adjustcolor(col, alpha.f = alpha))
    abline(h = c(-.2,0,.2), lty = c(2,1,2), lwd = 2,  col = 'white')
}


##' Function for plotting Drift rate time series using pre-determined
##' graphical parameters
##'
##' This function wraps some code to make nice looking plots for the
##' vignette. I can also be used to visualize the output of slimmingDive.
##' @title plotDrift
##' @param Data a seal dataset in \code{data.frame} format
##' @param ID \code{logical}. If true it will print axis and the tag id
##'     on the plot
##' @param ... aditional arguments to be pasted to the generic \code{base::plot}
##'     function
##' @return a plot of the drift rate time series.
##' @export
plotDrift <- function(Data, ID = FALSE, ...){
    plot(Data$Date, Data$NDE, col = 'black', panel.first = BGC(),
         ylim = c(-.45,.3), bg = adjustcolor('cornflowerblue', .9),
         xlim = c(min(Data$Date), min(Data$Date) + 86400*320),
         pch = 21, ...)
    if (ID){
        text(245/2, .25, unique(Data$ref), cex = 2.5)
        mtext(c('-0.2', '0', '0.2'), line = .2, side = 2,
              at = c(-.2, 0, .2), las = 2, cex = 1.5)
        axis(1, pos = par('usr')[3], cex = 2)
    }
}
