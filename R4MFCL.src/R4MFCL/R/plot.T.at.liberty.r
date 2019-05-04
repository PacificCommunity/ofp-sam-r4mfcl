plot.T.at.liberty = function(tagfl=readtagrep, logscl=FALSE, by.program=FALSE, fac.plot=TRUE, xaxe="Periods at liberty (quarters)",
                             yaxe="Number of tag returns", ncols=2)
{
  
    # This could use some work - especially the constant on the log version...
  
    require(scales)
    require(ggplot2)
    require(magrittr)  
    theme_set(theme_bw())
    
    tmp <- tagfl$auxdat
    
    
    if(by.program)
    {
      
      tmp.pl <- tmp %>% group_by(Programme = prog, tal = t2rec) %>% summarise(obs = +sum(orec), pre = sum(prec)) %>%
                        mutate(relobs = obs/max(obs), relpre = pre/max(pre)) %>% as.data.frame()
      
      if(logscl) tmp.pl %<>% mutate(relobs = log((obs + 0.5)/max((obs + 0.5))), relpre = log((pre + 0.5)/max((pre +0.5))), obs = log(obs + 0.5), pre = log(pre + 0.5))

        if(fac.plot)
        {
            pl <- ggplot(tmp.pl, aes(x = tal, y = pre)) + geom_line(size = 1, colour = alpha("black", 0.7)) +
                         geom_point(aes(x = tal, y = obs), size = 1.8, colour = alpha("red", 0.7)) + facet_wrap(~ Programme, ncol = ncols, scales = "free_y")
        } else {
            pl <- ggplot(tmp.pl, aes(x = tal, y = relpre, colour = Programme)) + geom_line(size = 1.1) + geom_point(aes(x = tal, y = relobs), size = 1.8) +
                         theme(legend.position = c(0.9,0.9))
        }
      
    } else {

      tmp.pl <- tmp %>% group_by(tal = t2rec) %>% summarise(obs = sum(orec), pre = sum(prec)) %>% as.data.frame()
      
      if(logscl) tmp.pl %<>% mutate(obs = log(obs + 0.5), pre = log(pre + 0.5))
      
      pl <- ggplot(tmp.pl, aes(x = tal, y = pre)) + geom_line(size = 1.2, colour = alpha("black", 0.6)) +
                   xlab(xaxe) + ylab(yaxe) + geom_point(aes(x = tal, y = obs), size = 3, colour = alpha("red", 0.7))
      
    }
    
    
    pl <- pl + xlab(xaxe) + ylab(yaxe) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    print(pl)  
    
}




