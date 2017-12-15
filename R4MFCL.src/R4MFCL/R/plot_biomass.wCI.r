#' Time series trend of "biomass" (SSB or total biomass recruitment) with confidense boounds (2SDs)
#' 
#' @param varfile filename of var file
#' @param years range of years 
#' @param btype string one of  "SSB", "Recruitment" or anything else (total biomass)
#' @param divide values of "biomass" be diveded by divide
#' @param ylab string label for y-axis
#' @param col   col
#' @param alpha alpha 
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon ylab ylim scale_x_continuous aes_string
#' @importFrom magrittr "%>%"
#' @export
plot_biomass.wCI<-function(varfile="swoModel3_2017071707AM_13flv16.var",
  years=1952:2015,btype="SSB",divide=1000000,ylab="Spawning biomass(1000 t)",col=1:2,alpha=0.2){

  .<-"XXXXX"

  vardata<-readLines(varfile)

  vardata[4:2214]%>% sapply(.,"trimws") %>% lapply(.,"strsplit",split=" +") %>% sapply(.,"[[",1,simplify="array")->var.table

  var.df<-data.frame(no=sapply(var.table,"[",1),val1=sapply(var.table,"[",2),val3=sapply(var.table,"[",3),ID=sapply(var.table,"[",4))
  cat("btype=",btype,"\n")
  if(btype=="SSB"){
    xx<-var.df[65:128,2:3]
    lnSSB=as.numeric(paste(xx[,2]))
    lnSSBup<-lnSSB+as.numeric(paste(xx[,1]))*2
    lnSSBlow<-lnSSB-as.numeric(paste(xx[,1]))*2
    ymax<-max(exp(lnSSBup)/divide)*1.1
    B<-exp(as.numeric(paste(lnSSB)))/divide
    pl.dat<-data.frame(year=years,B=B,UCI=exp(lnSSBup)/divide,LCI=exp(lnSSBlow)/divide)
    ylab=ylab
  }else if(btype=="Recruitment"){
    xx<-var.df[584:647,2:3]
    ylab="Recruitment(million fish)"
    lnRec=as.numeric(paste(xx[,2]))
    lnRec.sd=as.numeric(paste(xx[,1]))
    lnReclow<-lnRec-lnRec.sd*2
    lnRecup<-lnRec+lnRec.sd*2
    ymax<-max(exp(lnRecup)/divide)*1.1
    ylab<-"Recruitment(million fish)"
    B<-exp(as.numeric(paste(lnRec)))/divide
    pl.dat<-data.frame(year=years,B=B,UCI=exp(lnRecup)/divide,LCI=exp(lnReclow)/divide)
  }else{
    xx<-var.df[1:64,2:3]
    lnTotB=as.numeric(paste(xx[,2]))
    lnTotB.sd<-as.numeric(paste(xx[,1]))
    lnTotBup<-lnTotB+lnTotB.sd*2
    lnTotBlow<-lnTotB-lnTotB.sd*2
    ymax<-max(exp(lnTotBup)/divide)*1.1
    B<-exp(as.numeric(paste(lnTotB)))/divide
    pl.dat<-data.frame(year=years,B=B,UCI=exp(lnTotBup)/divide,LCI=exp(lnTotBlow)/divide)
    ylab<-"total biomass(1000t)"
  }
#  grid()
  pl<-ggplot(data=pl.dat,aes_string(x="years",y="B"))+geom_ribbon(aes_string(ymin="LCI",ymax="UCI"),alpha=alpha)
  pl<-pl+ylab(ylab)+ylim(0,ymax)+geom_line()+scale_x_continuous(breaks=seq(1950,2010,by=10),limits=c(1950,2020))
  return(invisible(pl))
}