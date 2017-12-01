#'
#' @importFrom ggplot2 ggplot geom_line xlim geom_point ylim xlab ylab theme scale_color_gradient aes_string labs theme_set theme_bw
#'
#' @export
#'
plot_SRR.gg <- function(repfile=read.rep(baserep), legopt=list(locy=22,locx1=150,xincr=1,yincr=0.5), annual=FALSE,
                     xlabel="Spawning biomass", l.overide=NULL,plot=TRUE,LEGEND=TRUE){
  # default colors is black,red,green3,cyan,magenda,yellow,gray,
#  require(ggplot2)
  theme_set(theme_bw())
#  library(RColorBrewer)
#  colors = brewer.pal(8, "Dark2")
  ymult <- round(length(repfile$Obs.SB)/length(unique(floor(repfile$yrs)))) # Round in case partial years in model
  year <- repfile$Year1+seq(length(repfile$Obs.SB))/ymult - (repfile$yrs[1]-repfile$Year1)

  if(annual & ymult > 1){
      OBSsb <- aggregate(repfile$Obs.SB/1000,by=list(trunc(year)),mean)[,2]
      OBSr <- aggregate(repfile$Obs.R/1000000,by=list(trunc(year)),sum)[,2]
      pmult <- ymult
      year <- unique(trunc(year))
  } else {
      OBSsb <- repfile$Obs.SB/1000
      OBSr <- repfile$Obs.R/1000000
      pmult <- 1
  }
  a <- length(OBSr)

    cols <- hsv(0.75, 1:a/a, 0.8, 1)
  Obs<-data.frame(SSB=OBSsb,Rec=OBSr,Cols=cols,year=year,stringsAsFactors=FALSE)
  Pred<-data.frame(SSB=repfile$Pred.SB/1000,Rec=repfile$Pred.R/1000000*pmult)
  lmult <- pmult
  if(annual & ymult == 1) lmult <- 4
  if(!is.null(l.overide)) lmult <- l.overide
 # cat("L21 in plot.SRR.gg.r\n");browser()

  plt<-ggplot(data=Pred)+geom_line(aes_string(x="SSB",y="Rec"))+xlim(0,max(Obs$SSB))+ylim(0,max(Obs$Rec))
  plt<-plt+geom_point(data=Obs,aes_string(x="SSB",y="Rec",colour="year"))  
  plt<-plt+xlab(xlabel)+ylab("Recruit (milions)")+scale_color_gradient(low = cols[1], high = rev(cols)[1])
  plt<-plt+ theme(legend.position=ifelse(!LEGEND,"none","right")) +labs(colour="") # +guides(colour=guide_legend(title=NULL)) # +labs(title="")

#  cat("L46\n");browser()
  if(plot)print(plt)
  return(invisible(plt))
}

