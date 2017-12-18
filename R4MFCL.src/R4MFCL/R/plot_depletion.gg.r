#' Plot of temporal changes of depletion using ggplot2
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes_string geom_line ylim ylab xlab theme theme_bw
#' @param plotrep outputs of read.rep
#' @param refpoint refpoint
#' @param refyear refyear
#' @param type string either "SSB", anything else (total biomass)
#' @param by.region LOGICAL if plot depletion by region
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param femaleOnly LOGICAL plot depletion of female population if nSp==2
#' @param verbose verbose?
#' @param plot LOGICAL send to plot to graphics device? 
#' @importFrom rlang sym
#' @export 
plot_depletion.gg <- function(plotrep,
refpoint,refyear= 2015.5,
  type="SSB",
  by.region=FALSE,
  xlab="Year",
  ylab="Depletion",
  femaleOnly=TRUE,
  verbose=TRUE,
  plot=TRUE)
{
##-----------------------------------------------------------------------
# Tidied up a little SJH 16/7/2011
##-----------------------------------------------------------------------
#### SJH 7/3/2014 7:26:34 PM make a standalone plot with the stock status on it
#  require(R4MFCL)
#  require(ggplot2)
#  require(reader)
#  require(stringr)
#  require(magrittr)
#  require(data.table)
#  require(dplyr)
#  require(tidyr)
  theme_set(theme_bw())
 cat("L25 starting plot.depletion.gg;")
# Dimensioning
#time steps
  nyr <- plotrep$nTimes
#first year
  year1 <- plotrep$Year1
#number of time steps per year
  tsteps <- plotrep$nRecs.yr
#number regions
  nreg <- plotrep$nReg
  cat("L35;")# ;browser()
  year <- trunc(seq(year1,length=nyr,by=1/tsteps))

# reading in the numbers
  if(type=="SSB")
  {
    B <- plotrep$AdultBiomass
    Bnof <- plotrep$AdultBiomass.nofish
    if(plotrep$nSp==1 ||  !femaleOnly){
      B <- plotrep$AdultBiomass
      Bnof <- plotrep$AdultBiomass.nofish
    }else{
      B <- plotrep$AdultBiomass[,which(plotrep$regSpPtr==which(plotrep$spSexPtr==1))]
      Bnof <- plotrep$AdultBiomass.nofish[,which(plotrep$regSpPtr==which(plotrep$spSexPtr==1))]
    }
    textlab <- "Proportion of total spawning potential - SB/SB(F=0)"
  }else{
    B <- plotrep$TotBiomass
    Bnof <- plotrep$TotalBiomass.nofish
    textlab <- "Proportion of total biomass"
  }
  ##-- add on totals  and divide by 1000
  B<-B/1000
  Bnof<-Bnof/1000
  ##--- aggregate by year
  if(verbose)cat("L60;") # ;browser()
  B <- if(nreg>1){
					aggregate(B,list(year),mean)
				}else{
					t(rbind(Year=year, Depletion=as.vector(B)))
				}
  Bnof <-if(nreg>1){
					aggregate(Bnof,list(year),mean)
				}else{
					t(rbind(Year=year, Depletion=as.vector(Bnof)))
				}
  nplt <- nreg+1
  
  if(verbose)cat("L73;") #;browser()
  if(by.region & nreg>1){
    dimnames(B)[[2]]<-c("Year",paste0("Region",1:(ncol(B)-1)))
    dplt<-B
    dplt[,2:ncol(dplt)]<-dplt[,2:ncol(dplt)]/Bnof[,2:ncol(dplt)]
    cat("L73;")#;browser()
    dplt.long<-dplt %>% gather(key="Region",value="Depletion",-!!sym("Year"))
    dplt.long %>% ggplot()->plt
    plt<-plt+geom_line(aes_string(x="Year",y="Depletion",color="Region"))+ylim(c(0,1))+ylab(ylab)+xlab(xlab)
    labs <- c(paste("Region",seq(nplt-1)),sep="_")
  }else{
#WCPO plot
  # par(mfrow=c(1,1))
    if(nreg>1){
    	B<-cbind(Year=B[,1],Depletion=apply(B[,2:ncol(B)],1,sum))
    	Bnof<-cbind(Year=Bnof[,1],Depletion=apply(Bnof[,2:ncol(Bnof)],1,sum))
    }
  #dimnames(B)[[2]]<-c("Year",paste0("Region",1:(ncol(B)-1)))
    dplt<-B
    dplt[,2]<-dplt[,2]/Bnof[,2]
    cat("L93;")#;browser()
  #  dplt.long<-dplt %>% gather(key=Region,value=Depletion,-Year)
    dplt.long<-as.data.frame(dplt)
    dplt.long %>% ggplot()->plt
    plt<-plt+geom_line(aes_string(x="Year",y="Depletion"))+ylim(c(0,1))+ylab(ylab)+xlab(xlab)
  #  labs <- c(paste("Region",seq(nplt-1)),sep="_")  
  	if(verbose)cat("L99\n") 
    # browser()   
  }
  if(verbose)cat("L102\n")
  if(plot)print(plt)
  return(invisible(plt))
}




