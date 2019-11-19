plot.regional.biomass.gg <- function(plotrep = read(baserep), annual = TRUE, divisor = 1000, fillcol = "#6699CC",
                                      Ncols = 2, nbrks = 3, brwidth = 1, p.type = "line", lnsize = 1.5, biomass = "AdultBiomass",
                                      y.lab = "Biomass (1,000's of mt)", varfile=NULL,overall=TRUE)
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
    bio <- as.data.frame(plotrep[[biomass]])
    names(bio) <- paste0("R", 1:dim(bio)[2])
    if(overall!=FALSE) bio %<>%mutate(Overall = apply(bio, 1, sum))
    plbio <- bio %>% mutate( year = year) %>%
                     melt(id=c("year"), variable.name = "Region", value.name = "Biomass") %>%
                     group_by(year, Region) %>% summarise(Biomass = mean(Biomass)/divisor)

    if(overall=="only") plbio%<>%filter(Region=="Overall")
    pl <- ggplot(plbio, aes(x = year, y = Biomass))

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

        SBtot <- varfile$adult_rbio

        plbounds <- data.frame(mu = SBtot[, 2],
                               SD = SBtot[, 1],
                               year = floor(plotrep$yrs),
                               Region = "Overall")
        plbounds$var <- plbounds$SD^2

        ann.SB <- aggregate(plbounds$mu, by = list(plbounds$year), mean)
        ann.var <- aggregate(plbounds$var, by = list(plbounds$year), sum)

        plbounds <- data.frame(LL = exp(ann.SB$x - 2*sqrt(ann.var$x))/1000000,
                               UL = exp(ann.SB$x + 2*sqrt(ann.var$x))/1000000,
                               year = unique(year),
                               Region = "Overall")

      } else {

        SBtot <- varfile$adult_rbio
        plbounds <- data.frame(LL = exp(SBtot[,2]-2*SBtot[,1]) /1000000,
                               UL = exp(SBtot[,2]+2*SBtot[,1]) /1000000,
                               year = year,
                               Region = "Overall")

      }

      plfull <- merge(plbio, plbounds, by.x = c("year","Region"), all.x = TRUE)

      pl <- ggplot(plfull, aes(x = year, y = Biomass))

      if(p.type == "bar") pl <- pl + geom_bar(stat="identity", colour = fillcol, fill = fillcol, width = brwidth)
      if(p.type == "line")
      {
        pl <- pl + geom_ribbon(aes(ymin = LL, ymax = UL), fill ="#56B4E9", alpha = 0.45) + geom_line(colour = fillcol, size = lnsize)
      }
      pl <- pl + facet_wrap(~ Region, ncol = Ncols, scales = "free_y") +
        xlab("Year") + ylab(y.lab) + expand_limits(y=0) +
        scale_y_continuous(breaks = pretty_breaks(n = nbrks)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      print(pl)
    }

}




