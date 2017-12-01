
#'
#' @importFrom ggplot2 ggplot geom_area aes_string guides guide_legend position_stack
#' @importFrom ggplot2 scale_fill_manual scale_fill_discrete theme labs element_text scale_color_manual scale_color_discrete
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#'
#' @export
#'
plot_biomass.stacked.gg <- function(plotrep=read.rep(baserep), pmain="Run 3d", type="SSB", maxylim=NULL,
                                        lgposi=c(0.9, 0.93), reg.cols = NULL, tit.colour=grey(0.4),reverse=TRUE,fill=TRUE,
                                        alpha=1,plot=TRUE,reg.labels=NULL,femaleOnly=TRUE)
{
#  require(ggplot2)
#  require(tidyr)
#  require(magrittr)
#  require(scales)
## Quick remedy for lgposi
if(length(lgposi)==1 && lgposi=="topright"){lgposi<-c(0.9,0.93);cat("lgposi was changed from topright to c(0.9,0.95)")}
if(length(lgposi)==1 && lgposi=="topleft"){lgposi<-c(0.1,0.93);cat("lgposi was changed from topright to c(0.1,0.95)")}
## check lgposi since legend.position of ggplot only accepts
## ("none", "left", "right", "bottom", "top", or two-element numeric vector)
if(length(lgposi)==1 && match(lgposi, c("none", "left", "right", "bottom", "top"), nomatch = 0) == 0){
  stop("lgposi only accepts (\"none\", \"left\", \"right\", \"bottom\", \"top\", or two-element numeric vector)")
}
# Number of years
  nyr <- plotrep$nTimes

#first year
  year1 <- plotrep$Year1

#number of time steps per year
  tsteps <- plotrep$nRecs.yr
  year <- trunc(seq(year1,length=nyr,by=1/tsteps))

  if(type=="SSB")
  {
     #    B <- plotrep$AdultBiomass/1000
    B <- if(plotrep$nSp==1 ||  !femaleOnly){
          plotrep$AdultBiomass/1000
        }else{
          plotrep$AdultBiomass[,which(plotrep$regSpPtr==which(plotrep$spSexPtr==1))]/1000
        }
 #    cat("L36 in plot.biomass.stacked.gg.r\n");browser()
         textlab <- "Spawning potential"
  }else{
    if(type=="REC"){
      B <- plotrep$Recruitment*tsteps/1000000
      textlab <- "Recruitment (millions of fish)"
    }else{
      B <- plotrep$TotBiomass/1000
      textlab <- "Total biomass (1'000's mt)"
    }
  }
    ##--- aggregate by year
if(plotrep$nReg > 1){
  Bout <- aggregate(B,list(year),mean)
} else {
  stop("This model only has one region so will look pretty stupid, that's why I'm not going to let you plot it")
}

#browser()
titles <- paste("Region",seq(1,(ncol(Bout)-1)))
if(is.null(maxylim)){
  maxylim <- max(apply(Bout[,2:ncol(Bout)],1,sum))
}
yr <- Bout[,1]
cols<-if(!is.null(reg.cols)){
  reg.cols[1:(ncol(Bout)-1)]
}else{
  NULL
}

Bout.stacked<-Bout
for(i in 3:ncol(Bout)){
  Bout.stacked[,i]<-Bout.stacked[,i-1]+Bout.stacked[,i]
}
dimnames(Bout)[[2]]<-
dimnames(Bout.stacked)[[2]]<-c("Year",paste0("Region",1:(ncol(Bout.stacked)-1)))
cat("L76 in plot.biomass.stacked\n") # ;browser()
Bout.stacked.long<-Bout %>% gather(key=Region,value=val,-Year)
 Bout.stacked.long %>% ggplot()->plt
# cat("L79 in plot.biomass.stacked\n");browser()
 plt<-plt+xlab("Year")+ylab(textlab)
if(fill){
  plt<-plt+geom_area(aes(x=Year,y=val,fill=Region),position=position_stack(reverse=reverse),alpha=alpha)
  if(!is.null(cols) && is.null(reg.labels))plt<-plt+scale_fill_manual(values=cols)
  if(is.null(cols) && !is.null(reg.labels))plt<-plt+scale_fill_discrete(labels=reg.labels)
  if(!is.null(cols) && !is.null(reg.labels))plt<-plt+scale_fill_manual(values=cols, labels=reg.labels)
}else{
  plt<-plt+geom_line(aes(x=Year,y=val,color=Region),position=position_stack(reverse=reverse))
  if(!is.null(cols) && is.null(reg.labels))plt<-plt+scale_color_manual(values=cols)
  if(is.null(cols) &&  !is.null(reg.labels))plt<-plt+scale_color_discrete(labels=reg.labels)
  if(!is.null(cols) && !is.null(reg.labels))plt<-plt+scale_color_manual(values=cols, labels=reg.labels)
}

plt<-plt+theme(legend.position = lgposi,legend.box=("vertical"))
if(reverse)plt<-plt+guides(fill = guide_legend(reverse=TRUE))
plt<-plt+guides(fill=guide_legend(title=NULL))
xlimits<-range(Bout[,1]) ;xlimits[1]<-floor(xlimits[1]/10)*10 ; xlimits[2]<-(floor(xlimits[2]/10)+1)*10 ; # +xlim(range(Bout[,1]))
ylimits<-
plt<-plt+ylim(0,maxylim)+scale_x_continuous(breaks=seq(xlimits[1],xlimits[2],by=10),limits=xlimits)
if(!is.null(pmain)){
  plt<-plt+labs(title=pmain)+theme(plot.title=element_text(hjust = 0.5,color=tit.colour))
}
if(plot)print(plt)
return(invisible(plt))
}