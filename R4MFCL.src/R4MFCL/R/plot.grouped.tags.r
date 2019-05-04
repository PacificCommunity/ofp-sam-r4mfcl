plot.grouped.tags = function(tagfl=readtagrep, mix.time=1, remove.mix=TRUE, xaxe="Year", yaxe="Number of tag returns",
                             ln.sz=0.7, ln.col=alpha("black", 0.7), pt.sz=2, pt.col=alpha("red", 0.7), all.fishies=TRUE,
                             Ncols=2, grpnms=paste("Grp", 1:19), keepGrps=c(1,2,4,5), fsh.grps=1:23, fac.levels=paste("Grp", 1:19))
{
  
  theme_set(theme_bw())
  require(scales)
  require(magrittr)
  require(dplyr)
  
  tmp <- tagfl$auxdat
  if(!remove.mix) mix.time <- 0
  
  if(all.fishies)
  {
    
      # All recaptures
      tmp.pl2 <- tmp %>% filter(t2rec > mix.time) %>% group_by(Year = ctime) %>%
                         summarise(obs = sum(orec), pre = sum(prec)) %>% as.data.frame()
      
      all.yr <- data.frame(Year = seq(min(tmp.pl2$Year), max(tmp.pl2$Year), 0.25))
      
      tmp.pl2 <- merge(tmp.pl2, all.yr, by = "Year", all.y = TRUE)
      tmp.pl2[is.na(tmp.pl2)] <- 0

      pl <- ggplot(tmp.pl2, aes(x = Year, y = pre)) + geom_line(size = ln.sz, colour = ln.col) +
                   geom_point(aes(x = Year, y = obs), size = pt.sz, colour = pt.col) #+ facet_wrap(~ pro, ncol = 2, scales = "free_y")

  } else {
    
    # Fisheries separated
    tmp.pl2 <- tmp %>% mutate(fry = fsh.grps[fry]) %>% filter(t2rec > mix.time, fry %in% keepGrps) %>% group_by(Year = ctime, fry) %>%
                       summarise(obs = sum(orec), pre = sum(prec)) %>% mutate(Fishery = grpnms[fry]) %>% as.data.frame()
    
    all.yr <- data.frame(Year = rep(seq(min(tmp.pl2$Year), max(tmp.pl2$Year), 0.25), length(unique(tmp.pl2$Fishery))))
    all.yr$Fishery <- rep(unique(tmp.pl2$Fishery), rep(length(unique(all.yr$Year)), length(unique(tmp.pl2$Fishery))))
    
    tmp.pl2 <- merge(tmp.pl2, all.yr, by = c("Year","Fishery"), all.y = TRUE)
    tmp.pl2[is.na(tmp.pl2)] <- 0
    
    tmp.pl2$Fishery <- factor(tmp.pl2$Fishery, levels = fac.levels)

    pl <- ggplot(tmp.pl2, aes(x = Year, y = pre)) + geom_line(size = ln.sz, colour = ln.col) +
                 geom_point(aes(x = Year, y = obs), size = pt.sz, colour = pt.col) + facet_wrap(~ Fishery, ncol = Ncols, scales = "free_y")   
    
  }
  
  pl <- pl + xlab(xaxe) + ylab(yaxe) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  print(pl)  
}







