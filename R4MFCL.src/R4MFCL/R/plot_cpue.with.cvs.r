#' Make plot of CPUE time series with associated CVs
#' @param repfile outputs of read.rep
#' @param frqfiles outputs of read.frq
#' @param fleetlabs vector of string, fleet names
#' @param nfish vector of fishery number
#' @param plot.layout not used
#' @param n.cols number of columns of plot
#' @param plot.annual LOGICAL if summarise annually
#' @param fac.levels dac.levels
#' @param plot logical if plot be sent to graphics device (default=TRUE) 
#' @param verbose verbose?
#' @importFrom ggplot2 geom_point geom_line facet_wrap theme element_blank theme_set theme_bw aes_string
#' @importFrom magrittr "%<>%" "%>%"
#' @importFrom dplyr filter mutate summarise mutate
#' @importFrom rlang enquo syms  expr parse_expr
#' @importFrom stats setNames
#' @export
plot_cpue.with.cvs <- function(repfile=read.rep("ALB15/plot-12.par.rep"), frqfiles=read.frq("ALB15/alb.frq"),
                               fleetlabs=paste("Region",1:8), nfish=1:8, plot.layout=c(3,3), n.cols=3,
                               plot.annual=TRUE, fac.levels=c("P-ALL-1","P-ALL-2","P-ALL-3","S-ID.PH-4","S-ASS-ALL-5"),plot=TRUE,verbose=TRUE)
{

# require(dplyr)
# require(magrittr)
 #   nfish<-enquo(nfish)
    theme_set(theme_bw())
    if(verbose)cat("L27 in plot_cpue.with.cvs\n") #;browser()
    tmp <- if(repfile$nRecs.yr==4){
              data.frame(yrqtr = rep(repfile$yrs, each=length(nfish)), fishery = nfish)
           }else if(repfile$nRecs.yr==1){
              data.frame(yrqtr = rep( rep(repfile$yrs,each=4)+(1:4)/4-0.125, each=length(nfish)),fishery = nfish)
           }

    mat <- as.data.frame(frqfiles$mat)
    mat$se[mat$effort == -1] <- NA
    mat$effort[mat$effort == -1] <- NA  
    if(verbose)cat("L37 ; ") # ;browser()
    mat %<>% filter('%in%'(!!sym("fishery"), nfish)) %>% 
       # mutate(cpue = "/"(!!!syms(c("catch","effort"))), cvs = 1/sqrt(2*(!!sym("se"))), yrqtr = eval(parse_expr("year+ (qtr- 0.5)/12")))
     mutate(cpue = eval(parse_expr("catch/effort")), cvs = 1/sqrt(2*(!!sym("se"))), yrqtr = eval(parse_expr("year+ (qtr- 0.5)/12")))
    
    if(verbose)cat("L40 ; ") # ;browser()
    fshmeans <- aggregate(mat$cpue, list(mat$fishery), mean, na.rm=TRUE)
    mat$cpue <- mat$cpue/fshmeans[match(mat$fishery, fshmeans[,1]),2]

     mat %<>% mutate(LL = eval(parse(text="exp(log(cpue) - 2*cvs)")), UL = eval(parse(text="exp(log(cpue) + 2*cvs)")))
 
    pldat <- merge(mat, tmp, by=c("yrqtr","fishery"), all.y=TRUE)
    if(verbose)cat("L46 ;") #;browser()
    pldat$fishery <- factor(fleetlabs[pldat$fishery], levels = fac.levels)
    if(verbose)cat("L48 ;") #;browser()
    pldat$years <- floor(pldat$yrqtr)

    if(plot.annual){
    pldat %<>% group_by(!!sym("fishery"), !!sym("years")) %>% 
                summarise(cpue = mean(!!sym("cpue"), na.rm=TRUE),
                                                          LL = mean(!!sym("LL"), na.rm=TRUE),
                                                          UL = mean(!!sym("UL"), na.rm=TRUE))
        pldat$yrqtr <- pldat$years
    }

    pl <- ggplot(pldat, aes_string(x="yrqtr", y="LL")) + geom_point(size=0.5, colour="grey") + geom_line(size=0.8, colour="grey") +
                 facet_wrap(~ fishery, ncol=n.cols) + xlab("Year") + ylab("CPUE") +
                 geom_line(aes_string(x="yrqtr", y="UL"), colour="grey", size=0.7) + geom_point(aes_string(x="yrqtr", y="UL"), colour="grey", size=0.5) +
                 geom_line(aes_string(x="yrqtr", y="cpue"), colour="black", size=0.7) + geom_point(aes_string(x="yrqtr", y="cpue"), colour="black", size=0.5) +
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    if(plot)print(pl)
    return(invisible(pl))

}











