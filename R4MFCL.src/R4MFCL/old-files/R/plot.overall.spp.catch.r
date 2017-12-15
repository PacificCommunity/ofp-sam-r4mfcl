plot.overall.spp.catch <- function(x=BET3d_catch,fleets=BET_fleets,YLIM=200,specs=gearspecs)
{
# Stacked bar plot for catches used catch.rep files
dimnames(x[[2]]) <- list(fleets$gear,NULL)
    windows(12, 8)
    par(mfrow=c(1,1))
    # get the fleets for the ith region
    regfleets <- fleets$gear
    y <- x[[2]]
    my_fleets <- unique(regfleets)
    tmp_mat <- matrix(NA,ncol=length(my_fleets),nrow=length(unique(trunc(x[[3]]))))

            for(j in 1:length(my_fleets))
            {
                #if only one fishery for a specific gear then no need for apply
                if(length(dimnames(y)[[1]][dimnames(y)[[1]]==my_fleets[j]])>1)
                {
                tmp_mat[,j] <- aggregate(apply(y[dimnames(y)[[1]]==unique(regfleets)[j],],2,sum),by=list(trunc(x[[3]])),sum,na.rm=T)$x
                }
                else{
                tmp_mat[,j] <- aggregate(y[dimnames(y)[[1]]==unique(regfleets)[j],],by=list(trunc(x[[3]])),sum,na.rm=T)$x
                }
            }
        #maxy <- (max(apply(tmp_mat,1,sum))/1000)/0.9

        lnames <- specs$gearEN[match(my_fleets,specs$code)]
        cls <- specs$cls[match(my_fleets,specs$code)]

        tmp <- barplot(height=t(tmp_mat)/1000,names.arg=unique(trunc(x[[3]])),legend.text=lnames,
        beside=F,col=cls, ylim=c(0,YLIM),
        las=1,args.legend=list(x=10,y=YLIM,bty="n"))
        box(bty="l")
        mtext(1,outer=T,text="Years",line=-2.5)
        mtext(2,outer=T,text="Catch (000s MT)",line=-1)
        #mtext(3,outer=T,text="OVERALL",line=-3)
}
