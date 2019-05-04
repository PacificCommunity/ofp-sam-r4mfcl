plot.length.comp.availability = function(frq = readfrq, bordercol="#6699CC", fillcol="#6699CC", Ncols = 3, nbrks = 3,
                                         annual = FALSE, fleetlabs = fltlabs, divisor = 1000,Max1000=FALSE)
{
    require(magrittr)
    require(dplyr)
    require(scales)

    theme_set(theme_bw())

    tmp <- as.data.frame(frq$mat)
    tmp <- tmp[tmp[,8] != -1,]
    tmp$Samples <- apply(tmp[,8:dim(tmp)[2]], 1, sum)
    tmp$yrqtr <- tmp$year + (tmp$qtr + 1)/12 - 0.125

    if(annual)
    {

        pldat <- tmp[, c("year","fishery","Samples")] %>% group_by(fishery, year) %>% summarise(Samples = sum(Samples)) %>% rename(yrqtr = year)

    } else {

        pldat <- tmp[, c("yrqtr","fishery","Samples")]

    }
    if (Max1000==FALSE){
        pldat$fishery <- factor(fleetlabs[pldat$fishery], levels = fleetlabs)
        pldat$Samples <- pldat$Samples/divisor

        pl <- ggplot(pldat, aes(x = yrqtr, y = Samples)) + geom_bar(stat="identity", colour = fillcol, fill = fillcol) +
            facet_wrap(~ fishery, ncol = Ncols, scales = "free_y") +
            xlab("Year") + ylab("Samples (No. fish x 1,000)") +
            scale_x_continuous(breaks = pretty_breaks(n = nbrks)) +
            scale_y_continuous(breaks = pretty_breaks(n = nbrks)) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        print(pl)
    } else {
        if(divisor!=1){warning("Max1000 is TRUE and divisor does not equal 1. Make sure that you want to plot the effective sample size")}
        pldat$fishery <- factor(fleetlabs[pldat$fishery], levels = fleetlabs)
        pldat$Samples <- ifelse(pldat$Samples>1000,1000,pldat$Samples)/divisor

        pl <- ggplot(pldat, aes(x = yrqtr, y = Samples)) + geom_bar(stat="identity", colour = fillcol, fill = fillcol) +
            facet_wrap(~ fishery, ncol = Ncols, scales = "free_y") +
            xlab("Year") + ylab("Effective Sample Size") +
            scale_x_continuous(breaks = pretty_breaks(n = nbrks)) +
            scale_y_continuous(breaks = pretty_breaks(n = nbrks)) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        print(pl)
    }
}





