#' Function to plot cpue fits
#'
#' @param plotrep output of read.rep
#' @param frq output of read.frq
#' @param par outputs from read.par
#' @param fisheries vector of positive integer, fisheries to be included into plot
#' @param fleetlabs vector of string, names of fisheries
#' @param XLIM XLIM
#' @param addLine LOGICAL add lines of predicted CPUEs
#' @param fac.levels fac.levels
#' @param ncol number of plots in horizontal direction
#' @importFrom ggplot2 ggplot theme_set theme_bw xlab facet_wrap geom_line geom_point theme ylab ylim element_blank scale_y_continuous
# ' @importFrom scales
#' @export
plot_cpue.fit.gg <- function(rep,
                          frq,
                          par,
                          fisheries=c(1,2,4,7,10,12),
                          fleetlabs,
                          XLIM=c(1950,2011), addLine=TRUE,
                          fac.levels=c("P-ALL-1","P-ALL-2","P-ALL-3","S-ID.PH-4","S-ASS-ALL-5"),
                          ncol=2,freeY=FALSE,annual=FALSE)
{
###########################################################################
# Adam's code turned into a function
# 14/7/2011
## plot LL CPUE obs vs expected
#
# SJDM 20/06/2014 - butchered in a non-efficient way and plotted using ggplot to see if looks less shit
#require(ggplot2)
#require(scales)

theme_set(theme_bw())

## expected is the observed but with effort modified by the effort devs
    tstep = rep$nTimes
#first year

    year1 =rep$Year1
#number regions

    nregion = rep$nReg
#number of age classes
    nage = rep$nAges
#number of fisheries
    nfish = rep$nFisheries
##fishery incidents
    fish1 = rep$nRlz.t.fsh
##fishery incidents times

    fish2 = rep$Rlz.t.fsh

# effort deviation coefficients
    edevs = par$effdevcoffs

    dat.tmp = data.frame(time=0,cpue.obs=0,cpue.pred=0,fshry=0)

    for(i in fisheries){
      time <- frq$mat[,1][frq$mat[,4]==i] +  frq$mat[,2][frq$mat[,4]==i]/12
      if(frq$version==6){
        catch <- frq$mat[,5][frq$mat[,4]==i]
        effort <- frq$mat[,6][frq$mat[,4]==i]
      }else{
        catch <- frq$mat[,13][frq$mat[,4]==i]
        effort <- frq$mat[,14][frq$mat[,4]==i]

        if(!is.null(frq$struct$nsp) & frq$struct$nsp){
          ndat<-length(catch)/frq$struct$nsp
          catch<-catch[1:ndat*2-1]
          effort<-effort[1:ndat*2-1]
          time<-time[1:ndat*2-1]
        }
      }
      effort <- ifelse(effort == -1, NA, effort)
      normeffort <- effort/mean(effort, na.rm = T)
      cpue.obs <- catch/(normeffort)
      edevs2 <- edevs[i,!is.na(edevs[i,])]## as.numeric(unlist(strsplit(edevs[i], split="[[:blank:]]+"))[-1])   #from Nick
      cpue.pred <- catch/(normeffort * exp(edevs2))

      years <- year1 + seq(1,tstep, 1)/4 - 0.125

      dat.tmp <- rbind(dat.tmp,data.frame(time,cpue.obs,cpue.pred,fshry=fleetlabs[i]))
    }

    dat.tmp <- dat.tmp[-1,]
    dat.tmp <- dat.tmp[dat.tmp$time > XLIM[1] & dat.tmp$time < XLIM[2],]
    dat.tmp$fshry <- factor(dat.tmp$fshry, levels = fac.levels)

    if(annual){

        dat.tmp %<>% mutate(time=trunc(time))%>% group_by(!!sym("fshry"),!!sym("time")) %>% summarise(cpue.obs = mean(cpue.obs, na.rm=TRUE),cpue.pred=mean(cpue.pred,na.rm=TRUE))
    }
    #cat("L63;") #;browser()
    # Produce and print plot
        p <- ggplot(dat.tmp, aes_string(x="time", y="cpue.obs/1000")) + geom_point(colour="#6699CC", alpha=0.8)
    if (freeY){ p <- p + facet_wrap(~ fshry,scales="free_y",ncol=ncol)
    } else { p <- p + facet_wrap(~ fshry,ncol=ncol)
    }
        p <- p + xlab("") + ylab("CPUE / 1000") +ylim(0,max(max(dat.tmp$cpue.obs/1000),max(dat.tmp$cpue.pred/1000))*1.1)
        if(addLine == TRUE) p <- p + geom_line(data=dat.tmp, aes_string(x="time", y="cpue.obs/1000"), colour="red", size=0.6, alpha=0.3)
        p <- p + geom_line(data=dat.tmp, aes_string(x="time", y="cpue.pred/1000"), colour="black", size=0.6, alpha=0.7)
        p <- p + geom_point(data=dat.tmp, aes_string(x="time", y="cpue.pred/1000"), colour="black", size=1, alpha=0.7)
        p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ,axis.text=element_text(size=14),axis.title=element_text(size=16,face='bold'),strip.text.x = element_text(size = 14),legend.title=element_blank(),panel.spacing.x=unit(5,"mm"))
        ## p <- p + scale_y_continuous(breaks=seq(0,20,4))
        print(p)
    return(invisible(p))
}
