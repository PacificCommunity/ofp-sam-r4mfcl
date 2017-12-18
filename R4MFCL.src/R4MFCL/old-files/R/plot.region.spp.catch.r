plot.region.spp.catch <- function(x=BET3d_catch,maxy=120,fleets=BET_fleets,N.cols=3,Reg=9,specs=gearspecs)
{
dimnames(x[[2]]) <- list(fleets$gear,NULL)
plotwidth = ifelse(N.cols==3, 14, 12)
windows(plotwidth, 12)
par(mfrow=c(3,N.cols))

all_fleets <- unique(fleets$gear)   #kludge for legend
all_lnames <- specs$gearEN[match(all_fleets,specs$code)]
all_cls <- specs$cls[match(all_fleets,specs$code)]

    for(i in 1:Reg)
    {
    # get the fleets for the ith region
    regfleets <- fleets$gear[match(fleets$regions,i,nomatch=0)>0]
    y <- x[[2]][match(fleets$regions,i,nomatch=0)>0,]
    my_fleets <- unique(regfleets)
    tmp_mat <- matrix(NA,ncol=length(my_fleets),nrow=length(unique(trunc(x[[3]]))))

#    # Need to set order of gears for plot
    ref.order <- c("LL","PS","PHID","OTH")
    my_fleets <- my_fleets[order(match(my_fleets,ref.order))]
    
            for(j in 1:length(my_fleets))
            {
                # if only one fishery in a region then no dimnames(y)[[1]] so need special calculation
                if(length(regfleets) ==1)
                {
                tmp_mat[,j] <- aggregate(y,by=list(trunc(x[[3]])),sum,na.rm=T)$x
                }
                else{
                    #if only one fishery for a specific gear then no need for apply
                    if(length(dimnames(y)[[1]][dimnames(y)[[1]]==my_fleets[j]])>1)
                    {
                    tmp_mat[,j] <- aggregate(apply(y[dimnames(y)[[1]]==my_fleets[j],],2,sum),by=list(trunc(x[[3]])),sum,na.rm=T)$x
                    }
                    else{
                    tmp_mat[,j] <- aggregate(y[dimnames(y)[[1]]==my_fleets[j],],by=list(trunc(x[[3]])),sum,na.rm=T)$x
                    }
                }
            }

        
        cls <- specs$cls[match(my_fleets,specs$code)]
#gear.cols = c("green","blue","yellow","pink"); names(gear.cols) <- c("LL","PS","PHID","OTH")

        tmp <- barplot(height=t(tmp_mat)/1000,names.arg=unique(trunc(x[[3]])),
        beside=F,col=cls, ylim=c(0,maxy),las=1)
        
        if(i==1) legend("topleft",legend=all_lnames,fill=all_cls,bty="n",cex=1)
        box(bty="l")
        title(main=paste("Region",i))
    }
mtext(1,outer=T,text="Years",line=-2.5)
mtext(2,outer=T,text="Catch (000s MT)",line=-1.5)
        #mtext(3,outer=T,text=paste("REGION",i),line=-3)
}
