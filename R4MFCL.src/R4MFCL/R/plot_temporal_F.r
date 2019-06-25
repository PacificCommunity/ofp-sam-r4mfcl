#' Plots annual F by year for adults and juveniles (as defined by the maturity ogive in the par file
#'
#' @param plotrep plot*.rep file read in by read.rep that contains the fishing mortality rates
#' @param par *.par file read in by read.par that contains the maturity at age for the model
#' @param dome binary to calculate matuirty at 1 after the maximum for domed maturity
#' @param ymax maximum value for y axis
#' @param French binary for if the axes are in French
#'
#' @export

plot_temporal_F  <-  function(plotrep=read.rep(baserep), par=read.par(par),dome=TRUE,ymax=NULL,French=FALSE){
   ##time steps
    nyr=plotrep$nTimes
    ##first year
    year1=plotrep$Year1
    ##number of time steps per year
    tsteps=plotrep$nRecs.yr
    ##number regions
    nreg=plotrep$nReg
    ##number of age classes
    nages=plotrep$nAges
    ##number of fisheries
    nfish=plotrep$nFisheries
    ##fishery incidents
    fish1=plotrep$nRlz.fsh
    ##fishery incidents times
    fish2=plotrep$Rlz.t.fsh

    Fj = Fm=list()
    year=trunc(seq(year1,length=nyr,by=1/tsteps))

    if(French) {
        leglab=c("adulte","juv\u{E9}nile")
        myxlab= "Ann\u{E9}es"
        myylab="Taux annuel de mortalit\u{E9}"
    }else {
        leglab=c("adult","juvenile")
        myxlab="Year"
        myylab="Annual fishing mortality"
    }
    ##=======================================================================
    ## Population number by age (across), year (or time pd) (down) and region
    ##=======================================================================
    agemat=plotrep$NatYrAgeReg
    ##=======================================================================
    ## Fishing mortality by age (across), year (or time pd) (down) and region
    ##=======================================================================
    Fmat=plotrep$FatYrAgeReg
    ##
    a=apply(Fmat*agemat, c(1,2), sum)/apply(agemat,c(1,2), sum)
    ## maturity at age
    mat=par$maturity
    ## need to carry across from maximum for the maturity scheduled which is dome-shaped
    if(dome)
    {
        tmp=rev(order(mat))[1] # find the maximum and make the rest also = 1
        mat[tmp:length(mat)]=1
    }
    ## Calculate the mature and juvenile Fs
    nadult=t(t(apply(agemat,c(1,2), sum)) * mat)
    x=apply(nadult*a,1,sum)/apply(nadult,1,sum)
    Fm=aggregate(x, list(year), sum)[,-1]
    njuv=t(t(apply(agemat,c(1,2), sum)) * 1-mat)
    x=apply(njuv*a,1,sum)/apply(njuv,1,sum)
    Fj=aggregate(x, list(year), sum)[,-1]
    year=sort(unique(year))

    if(is.null(ymax)){
        ymax=range(0,Fm, Fj)
    } else{
        ymax=range(0,ymax)
    }
    ## Make plot
    plot(year,Fm,type='l',ylim=ymax,ann=F,axes=F,lwd=2)
    lines(year,Fj,lwd=2,lty=1,col=2)
    box()
    legend("topleft",lty=1,lwd=2,col=1:2,legend=leglab,y.intersp=1.5,cex=1.1,bty="n")
    box(bty='l',lwd=1,cex.axis=.7)
    axis(2,lwd=1,cex.axis=0.8,las=1)
    axis(1,lwd=1,cex.axis=0.8)
    mtext(side=2, outer=T, myylab, cex=1, line =-1.5)
    mtext(side=1, outer=T, myxlab, cex=1, line =-2.5)
}
