#' Making time series trend of depletion by region
#'
#' @param plotrep output of read.rep
#' @param type string if type=="SSB" depletion in terms of SSB is used, otherwise depeletion in terms of total biomass is calculated
#' @param plot.layout plot.layout
#' @param legpos position of legend
#' @param mainleg mainleg
#' @param legplot legplot
#' @param windows windows
#' @param plot logical, make plot?
#' @param verbose Do verbose?
#' @param ylab ylab
#' @param female calculation only based on female biomass
#' @param ncol number of plots in horizontal order 
#' @importFrom magrittr "%>%"
#' @importFrom graphics box
#' @export
plot_depletion.regional.gg <- function(plotrep,
                                    type="SSB", 
                                    plot.layout=c(5,2), 
                                    legpos="bottomleft", 
                                    mainleg="topleft", legplot=5,
                                            windows=c(11,1),ylab="text",
                                    plot=TRUE,ncol=NA,verbose=TRUE,female=TRUE)
{
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
 theme_set(theme_bw())
# Dimensioning
#time steps
nyr <- plotrep$nTimes
#first year
year1 <- plotrep$Year1
#number of time steps per year
tsteps <- plotrep$nRecs.yr
#number regions
nreg <- plotrep$nReg

year <- trunc(seq(year1,length=nyr,by=1/tsteps))


# reading in the numbers
if(type=="SSB")
{
  B <- plotrep$AdultBiomass
  Bnof <- plotrep$AdultBiomass.nofish
  textlab <- ylab  #"Depletion(Adult biomass)"
}
else
{
  B <- plotrep$TotBiomass
  Bnof <- plotrep$TotalBiomass.nofish
  textlab <- ylab #"Total biomass (1000's mt)"
}

  if(nSp>1 & female){
    B<-B[,(nReg/2+1):nReg]
    Bnof<-Bnof[,(nReg/2+1):nReg]
  }

  ##-- add on totals  and divide by 1000
  B <- cbind(B,apply(B,1,sum)) /1000
  Bnof <- cbind(Bnof,apply(Bnof,1,sum))  /1000

  ##--- aggregate by year
  B <- aggregate(B,list(year),mean)
  #year <- B[,1]
  #B <- B[,-1]
  
  Bnof0<-Bnof <- aggregate(Bnof,list(year),mean)
    ## Compare SBf0 from the way which cause an error for annual model
  #Bnof.error<-aggregate(Bnof,list(year),mean)
  #SBf0.error<-Bnof.error[(nyr-windows[1]):(nyr-windows[2]+1),]%>% colMeans()
  # 2018-06-25 YT Need to make special treatments for annual model
  #Bnof0<-Bnof <-if(tsteps>1){
  #  aggregate(Bnof,list(year),mean)
  #}else{
  #  Bnof
  #}
#  require(magrittr)
  SBf0<-Bnof[(nyr-windows[1]):(nyr-windows[2]+1),]%>% colMeans()
	cat("Correct SBf0\n");print(SBf0)
	#cat("Incorrect SBf0\n");print(SBf0.error)
  cat("L80\n") #;browser()
#  B$type<-"Fished"
#  SBf0$type<-"Unfished"
  Depletion<-B
  if(verbose)cat("L84 ;\n") #;browser()
  Depletion[,2:dim(Depletion)[2]]<-
    t(apply(Depletion[,2:dim(Depletion)[2]],1,"/",SBf0[2:dim(Depletion)[2]]))
	cat("L87\n")#;browser()
  colnames(Depletion)[1]<-"Year"
  nRegPlot<-ifelse(nSp==1,nReg,ifelse(female,nReg/nSp,nReg))
  colnames(Depletion)[1+1:nRegPlot]<-paste0('Region',1:nRegPlot)
  colnames(Depletion)[nRegPlot+2]<-"Overall"
  Depletion.long<-Depletion %>% gather(key="Region",value="Depletion",-Year)
  if(verbose)cat("L80 ;\n") #;browser()

  plt<-Depletion.long  %>% ggplot()+geom_line(aes_string(x="Year",y="Depletion"))
  plt<-plt+facet_wrap(~Region,ncol=ifelse(is.na(ncol),plot.layout[2],ncol))+labs(y=textlab)
  if(plot)plot(plt)
  return(invisible(plt))
  if(0){
  nplt <- nreg+1
#  opar <- par(mfrow=c(4,2),mar=c(2,4,1,2)+.1,lwd=.5,xpd=T, omi=c(0,0.2,0,0))
  #on.exit(par(opar))

  labs <- c(paste("Region",seq(nplt-1)),"Overall")
  year <- B[,1]

  #par(mfrow=plot.layout, mar=c(2,4,1,2)+.1, lwd=.5, xpd=T, omi=c(0,0.2,0,0))
  
  for(i in 1:nplt){
    ymax<-max(B[,i+1]/SBf0[i+1])*1.1
    plot(year,B[,i+1]/SBf0[i+1],type='l',ylim=c(0,ymax),ann=F,axes=F,lwd=2,col=1)
#    lines(year,B[,i+1],lwd=2,lty=1)

#    lines(year,Bnof[,i+1],lwd=2,lty=1,col=2)

    box()
    axis(2,lwd=.1,cex.axis=1.2)
    if(i %in% (nplt - (1:plot.layout[2] - 1))) axis(1,lwd=.1,cex.axis=1.2) else axis(1,labels=F,lwd=.1)
    legend(legpos,legend=labs[i],cex=1,bty="n")
    
    if(i == legplot) legend(mainleg, lty=1, lwd=2, col=rev(1:2), cex=0.8, bty="n", legend=rev(c("Fished","Unfished")), y.intersp=1.5)
}

mtext(side=2, outer=T, text=textlab, line=0)
  }
}
