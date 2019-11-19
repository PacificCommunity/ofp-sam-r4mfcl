plot.regional.recruitment.gg <- function(plotrep = read(baserep), annual = TRUE, divisor = 1000000, fillcol = "#6699CC", varfile = NULL,
                                      Ncols = 2, nbrks = 3, brwidth = 1, p.type = "line", lnsize = 1.5, y.lab = "Recruitment (millions)",overall=TRUE,textsize=12)
{
    if(!overall%in%c(TRUE,FALSE,"only"))stop('overall must be either TRUE, FALSE , or "only"')

    require(ggplot2)
    require(magrittr)
    require(dplyr)
    require(reshape2)
    require(scales)

    theme_set(theme_bw())

    # Dimensioning
    #time steps
    nyr <- plotrep$nTimes
    #first year
    year1 <- plotrep$Year1
    #number of time steps per year
    tsteps <- plotrep$nRecs.yr
    #number regions
    nreg <- plotrep$nReg

    if(annual)
    {
        year <- trunc(seq(year1, length=nyr, by=1/tsteps) + 0.125)
    } else {
        year <- seq(year1, length=nyr, by=1/tsteps) + 0.125
    }

    # reading in the numbers from plotrep
    rec <- as.data.frame(plotrep$Recruitment)
    names(rec) <- paste0("R", 1:dim(rec)[2])
    if (overall!=FALSE) rec %<>% mutate(Overall = apply(rec, 1, sum), year = year)
    plrec <- rec  %>% mutate(year = year)%>%
                     melt(id=c("year"), variable.name = "Region", value.name = "Recruitment") %>%
        group_by(year, Region) %>% summarise(Recruitment = sum(Recruitment)/divisor)
    if(overall=="only") plrec%<>%filter(Region=="Overall")

    pl <- ggplot(plrec, aes(x = year, y = Recruitment))

    if(p.type == "bar") pl <- pl + geom_bar(stat="identity", colour = fillcol, fill = fillcol, width = brwidth)
    if(p.type == "line") pl <- pl + geom_line(colour = fillcol, size = lnsize)

    pl <- pl + facet_wrap(~ Region, ncol = Ncols, scales = "free_y") +
                          xlab("Year") + ylab(y.lab) + expand_limits(y=0) +
                          scale_y_continuous(breaks = pretty_breaks(n = nbrks)) +
                          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    print(pl)


    if(!is.null(varfile))
    {

        if(annual)
        {

          Rtot <- varfile$ln_abs_recr

          plbounds <- data.frame(LL = exp(Rtot[, 2] - 2*Rtot[, 1])/1000000,
                                 UL = exp(Rtot[, 2] + 2*Rtot[, 1])/1000000,
                                 year = year,
                                 Region = "Overall") %>% group_by(year, Region) %>% summarise(LL = sum(LL), UL = sum(UL))

        } else {

            Rtot <- varfile$ln_abs_recr
            plbounds <- data.frame(LL = exp(Rtot[,2]-2*Rtot[,1]) /1000000,
                                   UL = exp(Rtot[,2]+2*Rtot[,1]) /1000000,
                                   year = year,
                                   Region = "Overall")
        }


        plfull <- merge(plrec, plbounds, by.x = c("year","Region"), all.x = TRUE)

        pl <- ggplot(plfull, aes(x = year, y = Recruitment))

        if(p.type == "bar") pl <- pl + geom_bar(stat="identity", colour = fillcol, fill = fillcol, width = brwidth)
        if(p.type == "line")
        {
            pl <- pl + geom_ribbon(aes(ymin = LL, ymax = UL), fill ="#56B4E9", alpha = 0.45) + geom_line(colour = fillcol, size = lnsize)
        }
            pl <- pl + facet_wrap(~ Region, ncol = Ncols, scales = "free_y") +
                       xlab("Year") + ylab(y.lab) + expand_limits(y=0) +
                       scale_y_continuous(breaks = pretty_breaks(n = nbrks)) +
                       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text=element_text(size=textsize))
        print(pl)
    }

#       # now get them into the same format as the B
#       R.ub <- aggregate(R.ub,list(year),sum)
#       R.lb <- aggregate(R.lb,list(year),sum)

}




