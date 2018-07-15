#' Function to plot selectivity at mean length at age 
#'
# ' @param filename file name of "selectivity-multi-sex", default : "selectivity-multi-sex"
#' @param fishlab vector of string names of fisheries
#' @param rep outputs of read.rep
#' @param xlab caption of x-axis
#' @param ylab caption of y-axis
#' @param ncol number of columns of plots
#' @param dir horizontal order or not
#' @param multi logical
#' @param modelnames vector of character
#' @param plot LOGICAL if plot be sent ot graphics device
#' @param verbose verbose or not
#' @param Age, logical if TRUE x-axis is age, if FALSE x-axis is length
#' @param size size of lines 
#' @param option option parameter for viridis::scale_color_viridis, default="D" (="viridis")
#' @param Gender logical if symbols to represent gender to be plotted
#' @importFrom ggplot2 ggplot theme_set theme_bw geom_line aes_string geom_point facet_wrap guides labs ylab 
#' @importFrom tidyr gather separate unite
#' @importFrom dplyr mutate
#' @importFrom magrittr '%>%'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym syms
#' @importFrom stringr str_pad
#' @importFrom viridis scale_colour_viridis scale_fill_viridis
#' @export
#'
plot_selectivity <- function(fishlab,
                                    rep=read.rep("plot-09.par.rep"),
                                    xlab=NA,
                                    ylab="Selectivity",
                                    ncol=NULL,
                                    dir="h",
                                    plot=TRUE,
                                    verbose=TRUE,
                                    Age=TRUE,
                                    multi=FALSE,modelnames=NULL,
                                    size=2,option="D",
                                    Gender=TRUE){
  if(verbose)cat("Starting plot_selectivityatLength\n")

  #with_palette(viridis)
  theme_set(theme_bw())
  x <- ifelse(Age,"Age","mean.LatAge")
  if(is.na(xlab))xlab<-ifelse(Age,"Age","cm")
  if(!multi){
    nSp<-rep$nSp
    if(verbose)cat("L40;") #;browser()
    SelAtAge.long <- rep$SelAtAge.long
    p<-rep$SelAtAge.long %>% ggplot(aes_string(x=x,y="selex"))
    p<-p+xlab(xlab)+ylab(ylab)
    if(nSp>1){
      p <- p + geom_line(aes_string(color = "Gender") , size = size ) +
        geom_point(aes_string( shape = "Gender") )
    }else{
      p<-p+geom_line()+geom_point(size=1)
    }
     p <- p +  scale_fill_viridis(discrete=TRUE,alpha=0.8)
  }else{
    SelAtAge.long <- lapply(1:length(rep),function(i){
      SelAtAge<-rep[[i]]$SelAtAge.long
      
      SelAtAge%<>% mutate(model=modelnames[i])
      
      SelAtAge %>% unite(col="model_Gender",c("model","Gender") , remove = FALSE) 
      return(SelAtAge)
    }) %>% do.call("rbind", . )
    if(verbose)cat("L60;") #;browser()
    p <- SelAtAge.long %>% ggplot(aes_string(x = x,y = "selex"))
    p <- p + xlab(xlab) + ylab(ylab)
    p <- p + geom_line(aes_string(color = "model",linetype="model") , size = size) 
    p <- p + guides(colour=guide_legend(title=NULL,legend.position="bottom"),linetype=guide_legend(title=NULL,legend.position="bottom"))
    if(Gender){
      p <- p + geom_point(aes_string( shape = "Gender",fill="Gender") )
      p <- p +  scale_fill_viridis(discrete=TRUE,alpha=0.8)
    }
    # nSps <- sapply(rep,"[[","nSp")
    # if(all(nSps == 1))p <- p + labs(colour = "") + guides(color = FALSE ) 
  }
  if(verbose)cat("L69;") #;browser()
  p <- p + facet_wrap(~Fishery,ncol = ncol , dir = dir) + scale_colour_viridis(discrete=TRUE,option=option)
  if(!multi & nSp == 1)p <- p + labs(color = "") + guides(color = FALSE ) 
  if(plot)print(p)
#  browser()
  return(invisible(p))
}