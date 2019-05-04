plot.Majuro.easier.marginal <- function(dat,SBFactor,SBname='SBname', FFactor, SepFactor=NULL,xmax=1,ymax=2,cols=c("dodgerblue","springgreen1","mediumorchid"),pchs=c(19,19),cexs=1,legendargs=NULL,ps=16, GreenSB=c(0.2,1), greencol="forestgreen"){
    ## MTV create a Majuro plot for the terminal year across the grid
    ## Takes a data.frame that includes the SB/SBMSY and the F/FMSY as a column. The data frame can also contain a column which the points will be separated by in color, point character and size if desired. The name of the columns in the dataframe for the SB/SBMSY, F/FMSY, and separating variable are used to extract the data and should be entered as a string. SBname is the subscript of SB on the x-axis that is entered as a string. The length of cols, pchs, and cexs must be the same length as the number of factors in the separator column, if it is provided. A legend can be created if you provide all of the necessary inputs to legendargs as a list. The points are separated by turning the separator column into a factor so the order by default will be alphabetically or numerically sorted
    ## You can control the size of the text by the ps variable, by default it is set to 16. A green region can be created by a vector for the minimum and maximum x values to GreebSB, the default is the entire region below the F/FMSY=1 and greater that SB/SBF=0 20%. To have no green region set GreenSB to c(0,0), or set the color to transparent


    ## Error checking
    ## Make sure that dat is a dataframe
    if (!is.data.frame(dat)){stop("dat must be entered as a data.frame")}
    ## Make sure that SB factor, SB name and F factor are strings
    if(typeof(SBFactor)!="character"){stop("SBFactor, The name of the column in the data.frame for the Spawning Biomass (x-axis) must be entered as a string")}
    if(typeof(FFactor)!="character"){stop("FFactor, The name of the column in the data.frame for the Fishing mortality (y-axis) must be entered as a string")}
    if(!(typeof(SBname)%in% c("character","NULL"))){stop("SBname must be NULL or a string that will appear in the x-axis label")}

    ## If Sepfactor is not Null make sure that it is a string and then assign it to the Separator as a factor
    if(!is.null(SepFactor)){
        if(typeof(SepFactor)!="character"){stop("SepFactor, The name of the column of the data.frame that you will separate the points by needs to be a string")}
        Separator=as.factor(with(dat,get(SepFactor)))
        ## Calculate the number of different factors
        NSeps=nlevels(Separator)
        ## check to make sure that the length of cols, cexs and pchs is the same as the number of factors in Separtor
        if (length(cols)!=NSeps){stop("cols must be the same length as the number of factors in the separator column")}
        if (length(cexs)!=NSeps){stop("cexs must be the same length as the number of factors in the separator column")}
        if (length(pchs)!=NSeps){stop("pchs must be the same length as the number of factors in the separator column")}
    }

    ## Extract the SB factor from the outcomes dataframe and assigne to xdat. the name of the factor must be a string
    xdat=with(dat,get(SBFactor))
    ## Extract the F factor from the outcomes dataframe and assigne to ydat. the name of the factor must be a string
    ydat=with(dat,get(FFactor))

    ## Setup the layout for the marginal plot and create the background colors based on the input or default xmax and ymax.
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

    if (is.null(SepFactor)){           #If separating along factor separate by color, cex and pch if desired
        points(xdat,ydat, col=cols[1], cex=cexs[1],pch=pchs[1])
    } else {                            #If Not separating all first color cex, and pch
        points(xdat,ydat,
               col=cols[Separator],
               cex=cexs[Separator],
               pch=pchs[Separator]
               )
    }

    ## If there is a legend desired create one
    if (!is.null(legendargs)){
        ## an example of what
        ## legendargs=list(x=0.7,y=1.8,legend=c('New Growth','Old Growth'),bty='n',pch=c(19,16),col=c('blue','green'),cex=c(1,2))
        do.call(legend,legendargs)
    }

    ## If there is no separation factor create a bar plot with only one color
    if(is.null(SepFactor)){
        xhist = hist(xdat, plot=FALSE,breaks=seq(from=0,to=xmax,length.out=21))$counts
        yhist = hist(ydat, plot=FALSE,breaks=seq(from=0,to=ymax,length.out=21))$counts
        ## Add in the histogram on the top axis
        topx = max(xhist)
        topy = max(yhist)
        par(mai=c(0,1.25,.05,.05))
        barplot(xhist, axes=FALSE, ylim=c(0, topx), space=0,col=cols[1])
        ## Add in the histogram of the points on the right axis
        par(mai=c(1.25,0,.05,.05))
        barplot(yhist, axes=FALSE, xlim=c(0, topy), space=0, horiz=TRUE,col=cols[1])
    } else {
        ## If a separating factor is provided make a stacked barplot on both the x and y axis
        ## create a matrix to store the counts of the two axes for each factor
        xcounts=matrix(nrow=NSeps,ncol=20)
        ycounts=matrix(nrow=NSeps,ncol=20)
        ## Loop over the different factors in the Separator column and count the number of points in each group along both axes
        for (i in 1:NSeps){
            xcounts[i,]=hist(xdat[as.numeric(Separator)==i], plot=FALSE,breaks=seq(from=0,to=xmax,length.out=21))$counts
            ycounts[i,]=hist(ydat[as.numeric(Separator)==i], plot=FALSE,breaks=seq(from=0,to=ymax,length.out=21))$counts
        }
        ## Calculate the maxumum value in the 20 groups for both axes
        topx = max(colSums(xcounts))
        topy = max(colSums(ycounts))
        ## Add in the histogram on the top axis that is colored by the factor
        par(mai=c(0,1.25,.05,.05))
        barplot(xcounts, axes=FALSE, ylim=c(0, topx), space=0,col=cols)
        ## Add in the histogram of the points on the right axis that is colors by the factor
        par(mai=c(1.25,0,.05,.05))
        barplot(ycounts, axes=FALSE, xlim=c(0, topy), space=0,col=cols,horiz=TRUE)
    }
}

