plot.obspredcpue.betyft.ggplot <- function(plotrep=baserep,frqfile=basefrq,fisheries=c(1,2,4,7,10,12),
                                    parfile=basepar,fleetlabs=BET_fleets$fnames,XLIM=c(1950,2011),
                                    addLine=TRUE)
{
###########################################################################
# Adam's code turned into a function
# 14/7/2011
## plot LL CPUE obs vs expected
#
# SJDM 20/06/2014 - butchered in a non-efficient way and plotted using ggplot to see if looks less shit

## expected is the observed but with effort modified by the effort devs
    tmp <- readLines(plotrep)

    line1 <- grep("# Number of time periods", tmp)
    tstep <- scan(plotrep, nlines=1, skip = line1)
#first year
    line1 <- grep("# Year 1", tmp)
    year1 <- scan(plotrep, nlines=1, skip = line1)
#number regions
    line1 <- grep("# Number of regions", tmp)
    nregion <- scan(plotrep, nlines=1, skip = line1)
#number of age classes
    line1 <- grep("# Number of age classes", tmp)
    nage <- scan(plotrep, nlines=1, skip = line1)
#number of fisheries
    line1 <- grep("# Number of fisheries", tmp)
    nfish <- scan(plotrep, nlines=1, skip = line1)
##fishery incidents
    line1 <- grep("# Number of realizations", tmp)
    fish1 <- scan(plotrep, nlines=1, skip = line1)
##fishery incidents times
    line1 <- grep("# Time of each ", tmp)
    fish2 <- scan(plotrep, nlines=nfish, skip = line1)


# effort deviation coefficients
    line1 <- grep("# effort deviation coefficients", readLines(parfile))[1]
    edevs <- readLines(parfile)[(line1+1):(line1+nfish)]


    frq <- read.frq(frqfile)
    
    dat.tmp <- data.frame(time=0,cpue.obs=0,cpue.pred=0,fshry=0)
    
    for(i in fisheries){
        time <- frq$mat[,1][frq$mat[,4]==i] +  frq$mat[,2][frq$mat[,4]==i]/12
        catch <- frq$mat[,5][frq$mat[,4]==i]
        effort <- frq$mat[,6][frq$mat[,4]==i]
        effort <- ifelse(effort == -1, NA, effort)
        normeffort <- effort/mean(effort, na.rm = T)
        cpue.obs <- catch/(normeffort)
        edevs2 <- as.numeric(unlist(strsplit(edevs[i], split="[[:blank:]]+"))[-1])   #from Nick
        cpue.pred <- catch/(normeffort * exp(edevs2))

        years <- year1 + seq(1,tstep, 1)/4 - 0.125

        dat.tmp <- rbind(dat.tmp,data.frame(time,cpue.obs,cpue.pred,fshry=fleetlabs[i]))
    }
    
    dat.tmp <- dat.tmp[-1,]
    dat.tmp <- dat.tmp[dat.tmp$time > XLIM[1] & dat.tmp$time < XLIM[2],]

    # Produce and print plot
    p <- ggplot(dat.tmp, aes(x=time, y=cpue.obs/1000)) + geom_point(colour="red", alpha=0.5)
        p <- p + facet_wrap(~ fshry, ncol=2, scales="free_y")
        p <- p + xlab("") + ylab("CPUE / 1000")
        if(addLine == TRUE) p <- p + geom_line(data=dat.tmp, aes(x=time, y=cpue.obs/1000), colour='red', size=0.6, alpha=0.3)
        p <- p + geom_line(data=dat.tmp, aes(x=time, y=cpue.pred/1000), colour='black', size=0.6)
    print(p)

#    ggsave(p, file=plotname, width=plot.wdth, height=plot.hgt)
}