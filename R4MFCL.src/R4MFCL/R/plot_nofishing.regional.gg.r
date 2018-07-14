#' Plot time series of biomass with and without fishing
#'
#' 
#' @param plotrep output of read.rep
#' @param type string if SSB, SSB (AdultBiomass) otherwise total biomass
#' @param plot.layout plot.layout in one page
#' @param legpos position of legend
#' @param mainleg mainleg
#' @param legplot legplot
#' @param YLAB YLAB
#' @param plot logical if print plot
#' @param female calculation only based on female biomass
#' @param ncol number of plots in horizontal order 
#' @param free_y logical use same y-axis across plots
#' @param verbose, make verbose?
#' 
#'
#' @importFrom ggplot2 facet_wrap 
#' @export
plot_nofishing.regional.gg <- function(plotrep,
                          type="SSB", 
                          plot.layout=c(5,2), 
                          legpos="bottomleft", 
                          mainleg="topleft", 
                          legplot=5,
                          YLAB=NULL,
                          plot=TRUE,
                          female=TRUE,
                          verbose=TRUE,
                          ncol=NA,
                          free_y=TRUE)
{
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------

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

nSp<-ifelse(is.null(plotrep$nSp),1,plotrep$nSp)
nReg<-plotrep$nReg
# reading in the numbers
if(type=="SSB")
{
  #cat("L36 ; \n");browser()
  B <- plotrep$AdultBiomass
  
  Bnof <- plotrep$AdultBiomass.nofish
  textlab <- ifelse(!is.null(YLAB),YLAB,"Adult biomass (1000's mt)")
}
else
{
  B <- plotrep$TotBiomass
  Bnof <- plotrep$TotalBiomass.nofish
  textlab <- ifelse(!is.null(YLAB),YLAB,"Total biomass (1000's mt)")
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
  B$type<-"Fished"
  #year <- B[,1]
  #B <- B[,-1]
  Bnof <- aggregate(Bnof,list(year),mean)
  Bnof$type<-"Unfished"
  
  B0<-rbind(B,Bnof)
  colnames(B0)[1]<-"Year"
  nRegPlot<-ifelse(nSp==1,nReg,ifelse(female,nReg/nSp,nReg))
  colnames(B0)[1+1:nRegPlot]<-paste0('Region',1:nRegPlot)
  colnames(B0)[nRegPlot+2]<-"Overall"
  B0 %<>% gather(key="Region",value="B",-!!sym("Year"),-!!sym("type"))
  if(verbose)cat("L72 ;\n") #;browser()
  plt<-ggplot()+geom_line(data=B0,aes_string(x="Year",y="B",colour="type"))
  if(free_y){
  	plt<-plt+facet_wrap(~Region,ncol=ifelse(is.na(ncol),plot.layout[2],ncol),scales="free_y")+labs(y=textlab)
  }else{
  	plt<-plt+facet_wrap(~Region,ncol=ifelse(is.na(ncol),plot.layout[2],ncol))+labs(y=textlab)
  }
  
  if(plot)plot(plt)
  return(invisible(plt))

}
