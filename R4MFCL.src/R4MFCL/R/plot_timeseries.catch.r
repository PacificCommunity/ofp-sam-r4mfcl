#' Plots of catch time series by fishery
#' @param catdat string, file name of catch.rep
#' @param repfile outputs of read.rep
#' @param nocls number of columns of plots
#' @param gear vector of string of gear type by fishery
#' @param region vector of integer indicating region of each fishery
#' @param all.regions LOGICAL if TRUE summarize by year and gear type, otherwise by year, gear type and region
#' @param leg.txt.sz leg.txt.sz
#' @param leg.box.sz leg.box.sz
#' @param brwidth brwidth
#' @param legpos position of legend
#' @param collist list of color to be used
#' 
#' @importFrom ggplot2 theme_set theme_bw ggplot xlab ylab scale_y_continuous 
#' @importFrom ggplot2 geom_bar guides guide_legend facet_wrap scale_fill_manual element_blank scale_fill_manual
#' @importFrom ggplot2 aes_string unit element_text
#' @importFrom ggplot2 scale_fill_grey scale_fill_hue
#' @importFrom magrittr "%<>%"
#' @importFrom reshape2 melt
#' @importFrom dplyr summarise group_by
#' @importFrom rlang quo 
#' @importFrom rlang quos  syms
#' @export
#'
plot_timeseries.catch = function(catdat = "ALB15/catch.rep", repfile = read.rep("ALB15/plot-12.par.rep"), nocls = NULL,
                                 gear = c("L","L","L","L","L","L","L","L","T","T","T","O","O","O"), region = c(1:8,3,6,8,3,6,8),
                                 all.regions = TRUE, leg.txt.sz = 12, leg.box.sz = 0.4, brwidth = 0.9, legpos = c(0.05, 0.9),
                                 collist = setNames(c("darkslateblue","firebrick3","lawngreen"), c("L","T","O")))
{

#require(reshape2)
#require(ggplot2)
#require(dplyr)
#require(magrittr)
#require(grid)

  theme_set(theme_bw())
  
  nfsh <- length(gear)
  
  dat <- matrix(scan(catdat, skip=3), ncol = nfsh)
  dat <- as.data.frame(dat)
  
  names(dat) <- 1:nfsh
  
  dat$yrqtr <- repfile$yrs
  
  dat <- melt(dat, measure.vars = 1:(dim(dat)[2]-1), id.vars = "yrqtr", variable.name = "fsh", value.name = "catch")
  dat$fsh <- as.numeric(as.character(dat$fsh))
  dat$gear <- gear[dat$fsh]
  dat$region <- paste("Region", region[dat$fsh])
  dat$year <- floor(dat$yrqtr)
  
  if(all.regions){
      dat %<>% group_by(!!!syms(c("year","gear"))) %>% summarise(tcatch = !!sym("sum(catch)"))
  } else {
      dat %<>% group_by(!!!syms(c("year","region","gear"))) %>% summarise(tcatch = !!sym("sum(catch)"))  
  }
  
  pl <- ggplot(dat, aes_string(x = "year", y = "tcatch/1000", fill = "gear")) + geom_bar(stat="identity", width=brwidth) +
               geom_bar(stat="identity", width=brwidth, colour="black", show_guide=FALSE) + 
               scale_fill_manual(name = "gear", values = collist) +# scale_colour_manual(name = "gear", values = collist) +
            #   scale_fill_hue()+
            #   scale_fill_grey(start=0,end=0.9)+
               xlab('Year') + scale_y_continuous(expand = c(0.01,0), name = "Catch (1,000's mt)") +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.text = element_text(size=leg.txt.sz),
                     legend.title = element_blank(),
                     legend.position = legpos,
                     legend.key.size =  unit(leg.box.sz, "cm")) +
                guides(fill = guide_legend(reverse=TRUE))
  
  if(!all.regions) pl <- pl + facet_wrap(~ region, ncol=nocls)
  
  print(pl)

}
