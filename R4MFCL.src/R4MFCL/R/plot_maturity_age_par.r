#' Plot of maturity at age ogive from the par file read in by read.par
#' @param pars list of output of read.par
#' @param ylab string of desired y-axis label on plot
#' @param xlab string of desired x-axis label on plot.
#' @param modlab vector of strings for labeling maturities
#' @param LegLoc location of legend can be string or x and y coordinates
#'
#' @export
plot_maturity_age_par <-  function( pars,ylab,xlab="Age (quarters)",modlab=c("Par1","Par2"),LegLoc="bottomright"){


    Mat=list()
    for(i in 1:length(pars)){
        if(pars[[i]]$nSp >1) stop("Not capable of multisex/multispecies plots yet")
        Mat[[i]]=pars[[i]]$maturity
    }

    ## get the maximum value across the runs
    maxy= max(sapply(Mat,max))

    if(length(pars)==1){
        plot(1:length(Mat[[1]]), Mat[[1]], type="n", ylab=ylab, xlab=xlab, ylim=c(0,maxy),las=1)
        lines(1:length(Mat[[1]]), Mat[[1]], lwd=2, col="black")
    } else{
        plot(1:length(Mat[[1]]), Mat[[1]], type="n", ylab=ylab, xlab=xlab, ylim=c(0,maxy),las=1)
        for(i in 2:length(pars)){
            lines(1:length(Mat[[i]]), Mat[[i]], lwd=2, col=i)
        }
        lines(1:length(Mat[[1]]), Mat[[1]], lwd=2, col="black")
        legend(LegLoc, lwd=2, col=1:length(Mat), lty=c(1,1), legend=modlab,bty="n")
    }
}
