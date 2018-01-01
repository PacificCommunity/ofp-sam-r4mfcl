#' residuals plot of size composition fit
#'  
#' @param fitfl outputs of read.fit
#' @param frq outputs of read.frq
#' @param fltl vector of string names of fisheries
#' @param n.col number of columns of plots in one page 
#' @param nbrks number of breaks in y-axis
#' @param alph alpha of coulr of plots
#' @param sz.range sz.range
#' @param Fish.keep vector of string which fisheires need to be included in plot
#' @param plot TRUE if send plot to graphics device
#' @param LenFit LOGICAL if length composition is used
#' @param ylabel string caption for y-axis
#' @param verbose verbose or not
#' @param rep outputs of read.rep
#' @param Year1  first year of model
#' @param common.xlim LOGICAL if commong year ranges be used across plots 
#' @param XLIM common range of years used if common.xlim is TRUE
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot geom_point facet_wrap scale_y_continuous element_blank scale_colour_manual theme scale_size
#' @importFrom dplyr mutate filter
#' @importFrom reshape2 melt
#' @importFrom tidyr gather unite separate
#' @importFrom scales alpha pretty_breaks
#' @importFrom data.table rbindlist
#' @export
plot_size.residuals = function(fitfl = read.fit("length.fit"), 
                               frq , 
                               fltl,
                               n.col = 1, nbrks = 6, alph = 0.1, sz.range = c(0.5, 8),
                               Fish.keep = c("1","2"),plot=TRUE,LenFit=TRUE,ylabel="Length (cm)",
                               verbose=TRUE,rep=NULL,Year1=NA,common.xlim=TRUE,XLIM=NULL)
{
#    require(data.table)
#    require(magrittr)
#    require(dplyr)
#    require(ggplot2)
#    require(scales)
    .<-"XXXXXX"
    theme_set(theme_bw())
    if(verbose)cat("L40  starting plot.size.residuals\n")#;browser()
    Year0<-if(!is.na(Year1)){
      Year1-1
    }else{
      ifelse(!is.null(rep),rep$Year1,0)
    }
    if(LenFit){cat("LenFit\n")}else{cat("WeightFit\n")}
    lbins <- if(LenFit){
      seq(from = frq$dl$lffirst, by = frq$dl$lfwidth, length.out = frq$dl$lfint)
    }else{
      seq(from = frq$dl$wffirst, by = frq$dl$wfwidth, length.out = frq$dl$wfint)
    }
    if(verbose)cat("L52 ; ")#;browser()
    datlg <- rbindlist(fitfl$dates)
    if(verbose)cat("L54 in plot.size.residuals\n")#;browser()
    if(any(sapply(fitfl$obslf,is.null))){
      obslg <- rbindlist(fitfl$obslf[!sapply(fitfl$obslf,is.null)], idcol = "ID")
      if(verbose)cat("L57 in plot.size.residuals\n")#;browser()
      prelg <- rbindlist(fitfl$predlf[!sapply(fitfl$predlf,is.null)], idcol = "ID")
      obslg$ID<-(1:length(fitfl$obslf))[!sapply(fitfl$obslf,is.null)][obslg$ID]
      prelg$ID<-(1:length(fitfl$predlf))[!sapply(fitfl$predlf,is.null)][prelg$ID]
    }else{
      obslg <- rbindlist(fitfl$obslf, idcol = "ID")
      prelg <- rbindlist(fitfl$predlf, idcol = "ID")
    }
    if(verbose)cat("L65 ; ") #;browser()
    if(LenFit){
# 2 species model 8 should be 16
      size.av <- if(is.null(frq$version) || frq$version==6){
        table(frq$mat[, 4], frq$mat[, 8] == -1)
      }else if(frq$version==9){
        table(frq$mat[, 4], frq$mat[, 16] == -1)
      }else{
        stop("can not recognize frq version")
      }
    }else{
      size.av <- if(is.null(frq$version) || frq$version==6){   
       table(frq$mat[, 4], sapply(1:nrow(frq$mat),function(i){ ifelse(frq$mat[i, 8] == -1,ifelse(frq$mat[i, 8+1] == -1, TRUE ,FALSE),ifelse(frq$mat[i, 8+frq$dl$lfint] == -1,TRUE,FALSE))}))
      }else if(frq$version==9){
       table(frq$mat[, 4], sapply(1:nrow(frq$mat),function(i){ ifelse(frq$mat[i, 16] == -1,ifelse(frq$mat[i, 16+1] == -1, TRUE ,FALSE),ifelse(frq$mat[i, 16+frq$dl$lfint] == -1,TRUE,FALSE))}))
      }else{
        stop("can not recognize frq version in plot.size.residuals")
      }      
    }
  #  if(frq$version==9){cat("L27 in plot.size.resials\n");browser()}
    size.av <- which(size.av[,  1] > 0)
    if(verbose)cat("L86 ; ") #;browser()
    
    colnames(datlg)<-c("Year","Month","Week")
    reslg <- as.data.frame(obslg - prelg) %>% mutate(ID = factor(obslg$ID, levels = size.av), Year = datlg$"Year" + Year0 +(datlg$"Month" + 1)/12 - 0.125)
    if(verbose)cat("L90 ; ") #;browser()
    colnames(reslg) <- c("Fishery", as.character(lbins), "Year")
    reswd <- reslg %>% 
          #      melt(id.vars = c("Fishery","Year"), variable.name = "Length", value.name = "Residual") %>%
               unite(col="Fishery_Year",!!!syms(c("Fishery","Year")), remove=TRUE) %>% 
               gather(key="Length",value="Residual",-!!sym("Fishery_Year"))  %>% 
               separate( col="Fishery_Year",into=c("Fishery","Year"),sep="_") %>%
               mutate(Sign=ifelse(!!sym("Residual") <= 0, "Negative", "Positive"), Residual = abs(!!sym("Residual")))
                
              #  mutate(.,Fishery = as.numeric(as.character(!!sym("Fishery"))), Length = as.numeric(as.character(!!sym("Length"))), 
              #         Sign = ifelse(!!sym("Residual") <= 0, "Negative", "Positive"), Residual = abs(!!sym("Residual")))
              cat("L97 plot_size.residuals\n");browser()
    if(!LenFit){cat("L96\n") } #;browser()}
    if(!any(reswd$Fishery %in% Fish.keep)){
      cat("L97 Fish.keep is not included\n")
      cat("unique(reswd$Fishery):\n",unique(reswd$Fishery),"\n")
      cat("Fish.keep            :",Fish.keep,"\n")
      browser()
    }else{
      cat("L102 Fish.keep:\n");print(Fish.keep)
      cat("L103 unique(reswd$Fishery):\n",unique(reswd$Fishery),"\n")
    }
    cat("L105 ;") # ;browser() match(x, table, nomatch = 0) > 0
    cat(colnames(reswd),"\n")
    pldat <- reswd %>% filter( "%in%"(!!sym("Fishery"),  Fish.keep)) %>% 
        mutate(Fishery = factor(fltl[!!sym("Fishery")], levels = unique(fltl[!!sym("Fishery")]))) # Kludge to display them in correct order

    pl <- ggplot(pldat, aes_string(x = "Year", y = "Length", size = "Residual", colour = "Sign")) + geom_point() +
                 facet_wrap(~ Fishery, ncol = n.col, scales="free_y") + scale_size(range = sz.range) +
                 scale_y_continuous(breaks = pretty_breaks(n = nbrks)) +
                 scale_colour_manual(values = alpha(c("red", "blue"), alph)) +
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    if(common.xlim){
      if(is.null(XLIM)){XLIM<-c(min(pldat$Year,na.rm=TRUE)-1,max(pldat$Year,na.rm=TRUE)+1)}
      pl<-pl+xlim(XLIM)
    }
    pl<-pl+ylab(ylabel)
    if(plot)print(pl)
    if(verbose)cat("L121 finished plot.size.residuals\n")
    return(invisible(pl))
}



