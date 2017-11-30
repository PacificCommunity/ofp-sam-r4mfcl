#' @importFrom ggplot2 geom_point geom_line facet_wrap theme element_blank theme_set theme_bw aes_string
#' @importFrom magrittr "%<>%" "%>%"
#' @importFrom dplyr filter_ mutate mutate_  summarise_
#' @importFrom stats setNames
#' @export
plot_cpue.with.cvs <- function(repfile=read.rep("ALB15/plot-12.par.rep"), frqfiles=read.frq("ALB15/alb.frq"),
                               fleetlabs=paste("Region",1:8), nfish=1:8, plot.layout=c(3,3), n.cols=3,
                               plot.annual=TRUE, fac.levels=c("P-ALL-1","P-ALL-2","P-ALL-3","S-ID.PH-4","S-ASS-ALL-5"))
{

# require(dplyr)
# require(magrittr)

theme_set(theme_bw())

    tmp <- if(repfile$nRecs.yr==4){
              data.frame(yrqtr = rep(repfile$yrs, each=length(nfish)), fishery = nfish)
           }else if(repfile$nRecs.yr==1){
              data.frame(yrqtr = rep( rep(repfile$yrs,each=4)+(1:4)/4-0.125, each=length(nfish)),fishery = nfish)
           }

    mat <- as.data.frame(frqfiles$mat)
    mat$se[mat$effort == -1] <- NA
    mat$effort[mat$effort == -1] <- NA
#    mat %<>% filter(fishery %in% nfish) %>% mutate(cpue = catch/effort, cvs = 1/sqrt(2*se), yrqtr = year + (qtr - 0.5)/12)
    mat %<>% filter_("fishery %in% nfish") %>% 
    					mutate_(.dot=list(setNames(quote(catch/effort),"cpue"), 
    														setNames(quote(1/sqrt(2*se)),"cvs"), 
    														setNames(quote(year + (qtr - 0.5)/12),"yrqtr")))

    fshmeans <- aggregate(mat$cpue, list(mat$fishery), mean, na.rm=TRUE)
    mat$cpue <- mat$cpue/fshmeans[match(mat$fishery, fshmeans[,1]),2]

 #   mat %<>% mutate(LL = exp(log(cpue) - 2*cvs), UL = exp(log(cpue) + 2*cvs))
    mat %<>% mutate_(.dots=list(setNames(quote(exp(log(cpue) - 2*cvs)),"LL"), setNames(quote(exp(log(cpue) + 2*cvs)),"UL")))


    pldat <- merge(mat, tmp, by=c("yrqtr","fishery"), all.y=TRUE)
    pldat$fishery <- factor(fleetlabs[pldat$fishery], levels = fac.levels)
    pldat$years <- floor(pldat$yrqtr)
#    cat("L27\n");browser()
    if(plot.annual){
    #    pldat %<>% group_by(!!"fishery, years") %>% summarise(cpue = mean(cpue, na.rm=TRUE),
    #                                                      LL = mean(LL, na.rm=TRUE),
    #                                                      UL = mean(UL, na.rm=TRUE))
        pldat %<>% group_by(!!"fishery, years") %>% summarise_(.dot=list(setNames(quote(mean(cpue, na.rm=TRUE)),"cpue"),
                                                          setNames(quote(mean(LL, na.rm=TRUE)),"LL"),
                                                          setNames(quote(mean(UL, na.rm=TRUE)),"UL")))
        pldat$yrqtr <- pldat$years
    }

    pl <- ggplot(pldat, aes_string(x="yrqtr", y="LL")) + geom_point(size=0.5, colour="grey") + geom_line(size=0.8, colour="grey") +
                 facet_wrap(~ fishery, ncol=n.cols) + xlab("Year") + ylab("CPUE") +
                 geom_line(aes_string(x="yrqtr", y="UL"), colour="grey", size=0.7) + geom_point(aes_string(x="yrqtr", y="UL"), colour="grey", size=0.5) +
                 geom_line(aes_string(x="yrqtr", y="cpue"), colour="black", size=0.7) + geom_point(aes_string(x="yrqtr", y="cpue"), colour="black", size=0.5) +
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    print(pl)

}











