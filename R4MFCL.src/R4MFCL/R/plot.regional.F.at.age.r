# Plotting function that shows F-at-age by region
plot.regional.F.at.age <- function(plotrep = readrep, nbrks = 3, ncls =2, lnsize = 0.5, alph = 0.7, facet.scales = "free_y")
{
  
    require(data.table)
    require(magrittr)
    require(dplyr)
    require(scales)
    require(reshape2)

    # 1st get the whole area estimates and manipulate
    Fage.t <- plotrep$FbyAgeYr    # F-at-age aggreagated over regions
    colnames(Fage.t) <- 1:plotrep$nAges
    Fage.t <- as.data.frame(Fage.t) %>% mutate(alltimes = plotrep$yrs, Region = "Total") %>%
              melt(id.vars = c("alltimes","Region"), variable.name = "Age.class", value.name = "Fmort")
    
    # Now get the region-specific estimates and manipulate
    Fage.r <- plotrep$FatYrAgeReg     # F-at-age at the regional level
    Fage.r <- as.data.frame(Fage.r)   # Convert to really wide data.frame
    colnames(Fage.r) <- paste(rep(1:plotrep$nReg, rep(plotrep$nAges, plotrep$nReg)), rep(1:plotrep$nAges, plotrep$nReg))
    Fage.r$alltimes <- plotrep$yrs
    Fage.r %<>% melt(id.vars = c("alltimes"), variable.name = "RegAge", value.name = "Fmort") %>%
                mutate(Age.class = gsub(".* ", "", RegAge), Region = gsub(" .*", "", RegAge)) %>% select(-RegAge)
    
    # Join together and make sure age class levels are set so appear in legend and colour spectrum correctly
    pldat <- rbind(Fage.r, Fage.t)
    pldat$Age.class <- factor(pldat$Age.class, levels = 1:plotrep$nAges)
    
    pl <- ggplot(pldat, aes(x = alltimes, y = Fmort, colour = Age.class)) + geom_line(size = lnsize) +
                 facet_wrap(~ Region, ncol = ncls, scales = facet.scales) + xlab("Year") + ylab("Instantaneous fishing mortality") +
                 scale_y_continuous(breaks=pretty_breaks(n=nbrks)) +
                 scale_colour_manual(name = "Age class", values = alpha(topo.colors(n = plotrep$nAges), alph)) +
                 guides(colour = guide_legend(override.aes = list(size = 2))) +
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    print(pl)
}

