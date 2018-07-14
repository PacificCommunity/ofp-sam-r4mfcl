#' Making plots of catchability deviations
#' @param parfile outputs from read.par
#' @param plotrepfile outputs of read.rep
#' @param fleetlabs vector of strings, names of fishery
#' @param line.col color of lines to be plotted, either name of color or rgb
#' @param n.col number of columns of plot
#' @param lnsize line size of lines
#' @param nbrks number of breaks in Y-axis
#' @param annual logical if annual average be plotted
#' @param ylim default=NULL ylim
#' @param plot logical if print plot
#' 
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot geom_line aes_string xlab ylab
#' @importFrom ggplot2 scale_y_continuous element_blank facet_wrap theme
#' @importFrom scales pretty_breaks 
#' @importFrom scales alpha
#' @importFrom stats as.formula
#' @importFrom rlang sym syms 
#' @export
#'
#'
#plot_q <- function(parfile=read.par(basepar), plotrepfile=read.rep(baserep), fleetlabs=spp_fleets$fnames, line.col=alpha("red", 0.6),
#                   n.col=4, lnsize=1, nbrks=4, annual=TRUE){
plot_q <- function(parfile, 
                   plotrepfile, 
                   fleetlabs, 
                   line.col=NULL, # =alpha("red", 0.6),
                   n.col=4, 
                   lnsize=1, 
                   nbrks=4, 
                   annual=TRUE,
                   ylim=NULL,
                   plot=TRUE){


#  require(scales)
#  require(magrittr)
  
  theme_set(theme_bw())
  if(is.null(line.col))line.col<-alpha("red", 0.6) 
  nfish <- plotrepfile$nFisheries
  Ntimes <- dim(plotrepfile$Rlz.t.fsh)[2]
  
  year.tmp <- t(plotrepfile$Rlz.t.fsh)
  dim(year.tmp) <- c(prod(dim(year.tmp)),1)
  
  co.tmp <- t(plotrepfile$qAtAge)
  dim(co.tmp) <- c(prod(dim(co.tmp)),1)
  
  alltimes <- data.frame(Year = sort(unique(year.tmp)))
  
  dat <- data.frame(Year = year.tmp, co = co.tmp, Fsh = rep(fleetlabs, rep(Ntimes, length(fleetlabs))))

  fish.keep <- fleetlabs[which(parfile$ffl[,10] == 1)]
  dat <- dat[dat$Fsh %in% fish.keep,]
  
  plt.dat <- merge(dat, alltimes, by = "Year", all.y = TRUE) 
  plt.dat$Fsh <- factor(plt.dat$Fsh, levels = fish.keep)
  
  if(annual)
  {
    plt.dat$Year <- floor(plt.dat$Year)
    plt.dat %<>% group_by(!!!syms(c("Year", "Fsh"))) %>% summarise(co = mean(!!sym("co")))
  }
  
  pl <- ggplot(plt.dat, aes_string(x = "Year", y = "co")) + 
               geom_line(colour = line.col, size = lnsize) +
  #             facet_wrap(~ !!sym("Fsh"), ncol = n.col, scales="free_y") +
               facet_wrap(as.formula(substitute("~Fsh")), ncol = n.col, scales="free_y") +
               xlab("") + 
               ylab("Catchability coefficient (q)") + 
               scale_y_continuous(breaks=pretty_breaks(n=nbrks)) +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  if(!is.null(ylim))pl<-pl+ylim(ylim)
  if(plot)print(pl) 
  return(invisible(pl))
}
