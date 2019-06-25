##
## plot.catch.fit.gg(plotrepfile=rep,fleetlabs)
#' Making plots of fits of predicted catch to observed catch
#' @param plotrepfile outputs of read.rep
#' @param par  outputs of read.par
#' @param frq outputs of read.frq
#' @param fleetlabs vector of string of name of sisheries
#' @param plot.layout not used
#' @param yrlims range of years to be plotted
#' @param line.col color of lines
#' @param pnt.col color of points
#' @param n.col ncol
#' @param plot.log plot.log
#' @param lnsize lnsize
#' @param nbrks nbrks
#' @param yaxe label for caption on x-axis
#' @param plot LOGICAL if plot be sent to graphics device
#' @param verbose verbose or not
#' @param rm.fish fisheries to remove from plots
#'
#' @importFrom ggplot2 ggplot theme theme_set theme_bw scale_y_continuous aes_string scale_x_continuous
#' @importFrom ggplot2  facet_wrap geom_line geom_point xlab ylab element_blank
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom scales pretty_breaks alpha log_trans
#' @export
plot_catch.fit.gg <- function(plotrepfile,
                              par=NULL,
                              frq=NULL,
                              fleetlabs,
                              plot.layout=c(10,4),
                              yrlims=c(1950,2015),
                              line.col=alpha("red", 0.6),
                              pnt.col=alpha("black", 0.6),
                              n.col=4,
                              plot.log=FALSE,
                              lnsize = 1,
                              nbrks=c(3,3),
                              yaxe = "Annual catch (1,000's mt or No. fish)",
                              plot=TRUE, verbose=FALSE,
                              rm.fish=NULL){

#  require(ggplot2)
#  require(reshape2)
#  require(dplyr)
#  library(scales)

  theme_set(theme_bw())
  if(verbose)cat("L28;")
  nfish <- plotrepfile$nFisheries
  Ntimes<- dim(plotrepfile$Rlz.t.fsh)[2]
  nSp   <- plotrepfile$nSp
  if(!is.null(par)){
    if(par$pfl[41]==1){
      yaxe<-"Annual catch (1,000's mt)"
      if(!is.null(frq) & !sum(frq$dflag[1,])){
        yaxe = "Annual catch (1,000's  No. fish)"
      }
    }
  }
  year.tmp <- t(plotrepfile$Rlz.t.fsh)
  dim(year.tmp) <- c(prod(dim(year.tmp)),1)
  if(verbose)cat("L21;")
  co.tmp <- t(plotrepfile$ObsCatch/1000)/nSp
  if(nSp>1)cat("This code is compatible to 2 sex model with sex agregated catch but not compatible to multi-spcies model with species specifc catch \n")
  dim(co.tmp) <- c(prod(dim(co.tmp)),1)
  if(verbose)cat("L25;")
  cp.tmp <- t(plotrepfile$PredCatch/1000)
  dim(cp.tmp) <- c(prod(dim(cp.tmp)),1)

  alltimes <- data.frame(Year = sort(unique(floor(year.tmp))))
  #cat("L25\n");browser()
   if(verbose)cat("L31;")
#   browser()
  if(length(fleetlabs)==plotrepfile$nFisheries/nSp){
    Fsh<-rep(fleetlabs, rep(Ntimes, length(fleetlabs)))
  }else if(length(fleetlabs)>=rep$nFisheries/nSp){
    rFsh<-rep(fleetlabs[1:rep$nFisheries], rep(Ntimes, rep$nFisheries))
  }else{
    stop("length of fleetlabsis too short")
  }
   if(verbose)cat("L39;")
  dat <- data.frame(yrqtr = year.tmp, Year = floor(year.tmp), co = co.tmp, cp = cp.tmp, Fsh = Fsh)

  dat.pl <- dat %>% group_by(!!!syms(c("Fsh", "Year"))) %>%
      summarise(ObsCatch = sum(!!sym("co"), na.rm = TRUE),PreCatch = sum(!!sym("cp"), na.rm = TRUE))

  plt.dat <- merge(dat.pl, alltimes, by = "Year", all.y = TRUE)

  plt.dat$ObsCatch <- ifelse(plt.dat$ObsCatch == 0, NA, plt.dat$ObsCatch)
  plt.dat$PreCatch <- ifelse(plt.dat$PreCatch == 0, NA, plt.dat$PreCatch)

    if(!is.null(rm.fish)) plt.dat=plt.dat%>%filter(!(Fsh%in%rm.fish))
  if(verbose)cat("L41 in plot.catch.fit.gg.r\n") #;browser()
  pl <- ggplot(plt.dat, aes_string(x = "Year", y = "PreCatch")) + geom_line(colour = line.col, size = lnsize) +
               facet_wrap(~ Fsh, ncol = n.col, scales="free_y") +
               xlab("") + ylab(yaxe) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               geom_point(aes_string(x = "Year", y = "ObsCatch"), colour = pnt.col) + scale_x_continuous(breaks=pretty_breaks(n=nbrks[1]))

#  pl <- ggplot(plt.dat, aes(x = Year, y = ObsCatch)) + geom_point(colour = pnt.col, size = lnsize) +
#               facet_wrap(~ Fsh, ncol = n.col, scales="free_y") +
#               xlab("") + ylab(yaxe) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#               geom_point(aes(x = Year, y = PreCatch),shape="4", colour = line.col) + scale_x_continuous(breaks=pretty_breaks(n=nbrks[1]))

  if(plot.log)  pl <- pl + scale_y_continuous(trans=log_trans(), breaks=c(0.1,1,10,100,1000,10000))
  if(!plot.log) pl <- pl + scale_y_continuous(limits = c(0, NA), breaks=pretty_breaks(n=nbrks[2]))
  if(plot)print(pl)
  return(invisible(pl))
}









