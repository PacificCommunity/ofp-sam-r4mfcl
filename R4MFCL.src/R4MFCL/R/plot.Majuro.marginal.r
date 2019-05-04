plot.Majuro.marginal <- function(xdat,ydat,xmax=1,ymax=2, GreenSB=c(0.2,1), greencol="forestgreen",ptscol="dodgerblue",pch1=19,cex1=1,xdat2=NULL,ydat2=NULL,ptscol2="springgreen1",pch2=19,cex2=1,legendargs=NULL,ps=16,xdat3=NULL,ydat3=NULL,ptscol3="mediumorchid",pch3=19,cex3=1, SBname='SBname'){
    ## MTV create a Majuro plot for the terminal year across the grid
    ## Takes the SB/SBMSY as the x-value and the F/FMSY as the y-value and plots the points on the majuro background and creates a histgram
    ## can take up to two different groups of points to be plotted in different colors and different point characters if so desired

    zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
    layout(zones, widths=c(7/8,1/8), heights=c(1/8,7/8),respect=TRUE)
    par(mai=c(1.25,1.25,0,0),ps=ps)
    plot(c(0,0), c(0,0), ylim=c(0,ymax), xlim=c(0,xmax), axes=F,type="n", ylab="", xlab="")
    axis(1)
    axis(2,las=1)
    ## create red region
    polygon(c(0,0.2,0.2,0,0), c(0,0,ymax,ymax,0), col="red",border=NA)
    ## Create orange region
    polygon(c(0.2,xmax,xmax,0.2,0.2), c(1,1,ymax,ymax,1), col="orange",border=NA)
    ## Create green region based on user defined min and max SB/SBF0 and the color of the region
    polygon(x=c(GreenSB[1],GreenSB[2],GreenSB[2],GreenSB[1],GreenSB[1]),y=c(0,0,1,1,0),col=greencol,border=NA)
    ##border for red zone
    segments(0.2,0,0.2,ymax,lwd=3)
    segments(0,1,xmax,1, lwd=3,lty=4)
    ## Add in labels for the axes
    ##mtext(side =1, at=0.2, "SB=20%SBF0", line=2.5, cex=.7)
    mtext(side =1, at=0.01, expression('SB<20%SB'['F=0']), line=3)#, cex=.7)
    mtext(side =1, at=0.6, expression("SB>20%SB"['F=0']), line=3)#, cex=.7)
    mtext(side =1, bquote('SB'[.(SBname)]*' /SB'['F=0']), line=5.5, cex=1.25)
    mtext(side =2, at=1, expression('F=F'['MSY']), line=3)#, cex=.7)
    mtext(side =2, at=0.35, expression('F<F'['MSY']), line=3)#, cex=.7)
   mtext(side =2, at=mean(c(ymax,1)), expression('F>F'['MSY']), line=3)#, cex=.7)
    mtext(side =2, expression('F'['recent']*' /F'['MSY']), line=5.5, cex=1.25)


    ## Add in the points for the user input data
    points(xdat,ydat,col=ptscol,pch=pch1,cex=cex1)
    ## Add in the second set of points if they are provided
    if(!is.null(xdat2)&!is.null(ydat2)){ points(xdat2,ydat2,col=ptscol2,pch=pch2,cex=cex2)}
    if(!is.null(xdat3)&!is.null(ydat3)){ points(xdat3,ydat3,col=ptscol3,pch=pch3,cex=cex3)}
    ## If there is a legend desired create one
    if (!is.null(legendargs)){
        ## Legend args are all input by the user to be what they want so can easily include the 2 groups
        ## legendargs=list(x=0.7,y=1.8,legend=c('New Growth','Old Growth'),bty='n',pch=c(pch1,pch2),col=c(ptscol,ptscol2),cex=c(cex1,cex2))
        do.call(legend,legendargs)
    }
    ## If there is no second data points just create normal bar plots
    xhist = hist(xdat, plot=FALSE,breaks=seq(from=0,to=xmax,length.out=20))
    yhist = hist(ydat, plot=FALSE,breaks=seq(from=0,to=ymax,length.out=20))
    if(is.null(xdat2)&is.null(xdat3)){
        ## Add in the histogram on the top axis
        topx = max(xhist$counts)
        topy = max(yhist$counts)
        par(mai=c(0,1.25,.05,.05))
        barplot(xhist$counts, axes=FALSE, ylim=c(0, topx), space=0,col=ptscol)
        ## Add in the histogram of the points on the right axis
        par(mai=c(1.25,0,.05,.05))
        barplot(yhist$counts, axes=FALSE, xlim=c(0, topy), space=0, horiz=TRUE,col=ptscol)
    } else if (is.null(xdat3)&is.null(ydat3)){
        ## If second dataset is provided make a stacked barplot of the two data sources
        xhist2=hist(xdat2, plot=FALSE,breaks=seq(from=0,to=xmax,length.out=20))
        yhist2=hist(ydat2, plot=FALSE,breaks=seq(from=0,to=ymax,length.out=20))
        topx = max(c(xhist$counts+xhist2$counts))
        topy = max(c(yhist$counts+yhist2$counts))
        ## Add in the histogram on the top axis
        par(mai=c(0,1.25,.05,.05))
        barplot(matrix(c(xhist$counts,xhist2$counts),nrow=2,byrow=TRUE), axes=FALSE, ylim=c(0, topx), space=0,col=c(ptscol,ptscol2))
        ## Add in the histogram of the points on the right axis
        par(mai=c(1.25,0,.05,.05))
        barplot(matrix(c(yhist$counts,yhist2$counts),nrow=2,byrow=TRUE), axes=FALSE, xlim=c(0, topy), space=0,col=c(ptscol,ptscol2),horiz=TRUE)
    } else {
        ## If third dataset is provided make a stacked barplot of the three data sources
        xhist2=hist(xdat2, plot=FALSE,breaks=seq(from=0,to=xmax,length.out=20))
        yhist2=hist(ydat2, plot=FALSE,breaks=seq(from=0,to=ymax,length.out=20))
        xhist3=hist(xdat3, plot=FALSE,breaks=seq(from=0,to=xmax,length.out=20))
        yhist3=hist(ydat3, plot=FALSE,breaks=seq(from=0,to=ymax,length.out=20))

        topx = max(c(xhist$counts+xhist2$counts+xhist3$counts))
        topy = max(c(yhist$counts+yhist2$counts+yhist3$counts))
        ## Add in the histogram on the top axis
        par(mai=c(0,1.25,.05,.05))
        barplot(matrix(c(xhist$counts,xhist2$counts,+xhist3$counts),nrow=3,byrow=TRUE), axes=FALSE, ylim=c(0, topx), space=0,col=c(ptscol,ptscol2,ptscol3))
        ## Add in the histogram of the points on the right axis
        par(mai=c(1.25,0,.05,.05))
        barplot(matrix(c(yhist$counts,yhist2$counts,yhist3$counts),nrow=3,byrow=TRUE), axes=FALSE, xlim=c(0, topy), space=0,col=c(ptscol,ptscol2,ptscol3),horiz=TRUE)

    }
}

