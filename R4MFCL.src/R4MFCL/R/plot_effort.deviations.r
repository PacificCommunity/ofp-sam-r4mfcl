#' Plots of effor deviations
#'
#' @param plotrepfile outputs of read.rep
#' @param frqfile outputs of read.frq
#' @param fleetlabs vector of string names of fisheries
#' @param ylimit range of y-axis
#' @param fishplot vector of intergers indicationg, which  fishery needs to include in plot
#' @param nclm number of columns of plot
#' @param spandef parameter to control loess smoothing
#'
#' @importFrom ggplot2 ggplot facet_wrap geom_point aes_string geom_abline stat_smooth theme element_blank
#' @importFrom ggplot2 xlab ylab ylim theme_set theme_bw
#' @export
plot_effort.deviations <- function(plotrepfile, 
                                   frqfile, 
                                   fleetlabs,
                                   ylimit=c(-2,2), fishplot=1:33, nclm = 4, spandef = 0.8)
{

#  require(mgcv)
#  require(scales)

  theme_set(theme_bw())

  nfish <- plotrepfile$nFisheries
  year.tmp <- plotrepfile$Rlz.t.fsh
  co.tmp <- plotrepfile$qEdevAtAge
  cp.tmp <- plotrepfile$qAtAge
  no.rows <- ceiling(length(fishplot)/3)
  labs <- fleetlabs

  j <- 0   # Probably a better way to do this but I need a counter for the loop below

  plotdat <- data.frame(year = NA, res = NA, fshnm = NA)

  for(i in fishplot) {

  j <- j + 1
  effort <- if(frq$version<=6){ # YT 2018-07-05 check frq version since version 9 have more columns before effort
  	frqfile$mat[,"effort"][frqfile$mat[,"fishery"]==i]
  	}else if(frq$version==9){
  		if(is.null(frq$struct$nsp)| frq$struct$nsp==1){
  			frqfile$mat[,"effort"][frqfile$mat[,"fishery"]==i]
  		}else{
  			frqfile$mat[,"effort"][frqfile$mat[,"fishery"]==i & frqfile$mat[,"Sex1"]==1]
  		}
  	}else{ 
  		stop("frq version is ",frq$version," I do not know how to dealwith")
  	}
  year <- year.tmp[i,!is.na(year.tmp[i,])]
  cp <- cp.tmp[i,!is.na(cp.tmp[i,])]
  co <- co.tmp[i,!is.na(co.tmp[i,])]
  res <- log(co/cp)
  res[effort==-1] <- NA
  
  tmpdat <- data.frame(year = year, res = res, fshnm = labs[i])
  
  plotdat <- rbind(plotdat, tmpdat)

  }

  plotdat <- plotdat[-1,]
  plotdat$res[plotdat$res == 0] <- NA

# Produce and print plot
  p <- ggplot(plotdat, aes_string(x="year", y="res")) + geom_point(colour="#6699CC", alpha=0.8)
  p <- p + facet_wrap(~ fshnm, ncol=nclm)
  p <- p + xlab("") + ylab("Effort deviations") + ylim(ylimit)
  p <- p + geom_abline(intercept = 0, slope = 0, colour = "red", alpha=1/2)
  p <- p + stat_smooth(method = "loess", formula = y ~ x, span = spandef, se = FALSE, show_guide = FALSE, fullrange = TRUE, size = 0.8, col = alpha("black", 0.6))
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  print(p)

}
