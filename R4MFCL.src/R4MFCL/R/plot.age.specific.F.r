plot.age.specific.F = function(plotrep=read.rep("ALB15/plot-12.par.rep"), years.keep = c(1965,1975,1985,1995),
                               plotcol="#6699CC", nbrks=c(3,4))
{

  require(reshape2)
  require(magrittr)
  require(dplyr)
  require(grid)
  require(scales)

  theme_set(theme_bw())

# Construct plot of numbers at age
    Nage <- plotrep$NatYrAgeReg
    Nage <- melt(Nage, varnames=c("time","age","region"))
    Nage$alltimes <- plotrep$yrs[Nage$time]
    Nage$year <- floor(Nage$alltimes)

    Nage %<>% group_by(year, age) %>% summarise(sumN = sum(value)) %>% 
                                      mutate(freq = sumN/sum(sumN)) %>%
                                      filter(year %in% years.keep)

    pl1 <- ggplot(Nage, aes(x=age, y=freq)) + geom_bar(stat="identity", colour=plotcol, fill=plotcol) +
                  facet_wrap(~year, ncol=1) + xlab("Age class") + ylab("Relative frequency") +
                  scale_y_continuous(breaks=pretty_breaks(n=nbrks[1])) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Construct plot of fishing mortality at age
    Fage <- plotrep$FbyAgeYr
    Fage <- as.data.frame(Fage)
    Fage$alltimes <- plotrep$yrs

    Fage <- melt(Fage, id.vars="alltimes", variable.name="age", value.name="fmort")

    Fage$age <- as.numeric(Fage$age)
    Fage$year <- floor(Fage$alltimes)
    Fage %<>% group_by(year, age) %>% summarise(muF = mean(fmort)) %>% filter(year %in% years.keep)

    pl2 <- ggplot(Fage, aes(x=age, y=muF)) + geom_line(size=1) +
           facet_wrap(~year, ncol=1) + xlab("Age class") + ylab("Instantaneous fishing mortality") +
           scale_y_continuous(breaks=pretty_breaks(n=nbrks[2])) +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Set up new plot using grid package
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2,2)))
    
    print(pl1, vp = viewport(layout.pos.row=1:2, layout.pos.col=1))

    print(pl2, vp = viewport(layout.pos.row=1:2, layout.pos.col=2))
    
}
