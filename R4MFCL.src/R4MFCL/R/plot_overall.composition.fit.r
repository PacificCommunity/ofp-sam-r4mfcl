#fishlab.swo<-c("DW_1N","DW_1C","DW_1S","AU_1","SP_1","Otrher_1","DW_2N","DW_2C_pre","DW_2C_post","DW_2S",
#    "NZ_2","SP_2","Other_2N","Other_2C")
#' Making plot of fit of agrreagted size composition data by fishery
#' 
#' @param filename string file name of **.fit file, either length.fit or weight.fit
#' @param xlabel string, caption of x-axis
#' @param remove.fsh logical or string if TRUE or "TRUE", only fishery with data be plotted
#' @param VecFsh vector of fishery number, probably not used
#' @param Ncols number of columns of plot in one page
#' @param line.wdth width of lines 
#' @param fleetlabs vector of string, names of fishery
#' @param lincol color of line
#' @param filcol color of aread filled with color
#' @param ngrks number of greaks 
#' @param nSp number of species
#' @param aggregate LOGICAL  
#' @param dir character either "h" or "v" if "h" plot be made horizontal order 
#' @param plot LOGICAL if plot be made to grap
#' @param verbose LOGICAL if making verbose outputs
#' @param fit LOGICAL if making overlaying plot of fit or not
#' @param rep outputs of report.rep, only needed if nSp>1 and fit==T
#' @importFrom ggplot2 ggplot theme_set theme_bw geom_line aes_string geom_point facet_wrap guides labs ylab
#' @importFrom ggplot2 geom_bar scale_y_continuous xlab ylab
#' @importFrom tidyr unite
#' @importFrom dplyr mutate rename if_else
#' @importFrom magrittr '%>%'
# ' @importFrom stringr str_split
#' @importFrom data.table as.data.table
#' @importFrom stats na.omit
#' @importFrom rlang sym syms
#' @export
#' 
plot_overall.composition.fit = function(filename="length.fit", 
                                        xlabel="Length (cm)", 
                                        remove.fsh=TRUE,
                                        VecFsh=1:14, 
                                        Ncols=4, 
                                        line.wdth=1.2, 
                                        fleetlabs, 
                                        lincol="#FF3333",
                                        fillcol="#6699CC", 
                                        nbrks=3,
                                        nSp=1,
                                        aggregate=FALSE,
                                        dir="h",
                                        plot=TRUE,
                                        verbose=TRUE,
                                        fit=TRUE,
                                        rep=NULL)
{
#    require(R4MFCL)
#    require(ggplot2)
#    require(reshape2)
#    require(scales)
#    require(magrittr)
#    require(dplyr)
 #   require(stringr)
#    require(tidyr)
    if(verbose)cat("Starting plot_overall.composition.fit.r\n")

    theme_set(theme_bw())
    ## we need to determine the version of fit file.
    fittxt<-readLines(filename)

    version<-if(strsplit(fittxt[1],split=" +")[[1]][1]=='#'){
                strsplit(fittxt[1],split=" +")[[1]][3]
              }else{
                1
              }
    cat("fit file version=",version,"\n")

    Nfsh <- scan(filename, nlines=1, skip=ifelse(version==1,2,3)) - 1   # Determine the number of fisheries from file header
    Nskips <- scan(filename, nlines=1, skip=ifelse(version==1,4,5))   # Determine the number of lines in the matrix for each fishery, from file header
    size.pars <- scan(filename, nlines=1, skip=ifelse(version==1,1,2))  # Extract the parameters that determine the size bins - no. bins, first bin size, bin width
    sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])   # Construct the size bins from the file header
    fishSpPtr <-if(version>=2){scan(filename,nlines=1,skip=7)}else{NA}
    if(version>=2)nSp<-length(unique(fishSpPtr))
    #cat("L38\n");browser()
    if(nSp>1 & fit & is.null(rep))stop("rep is needed for multi species/sex model")
    if(verbose)cat("L38 in plot.overall.composition.fit.r; nSp=",nSp,"\n") #;browser()
    VecFsh <- 1:(Nfsh)   # Vector of fisheries numbers - just numeric for now
    LineKeep <- (VecFsh-1) * (Nskips + 6) + 1   # Identify the lines of the observed size frequencies for the fisheries
    if(nSp>1 & fit){
      pcatch<-rep$PredCatch
      Year1<-rep$Year1
      Rlz.t.fsh<-rep$Rlz.t.fsh
      Rlz.t.fsh1<-Rlz.t.fsh-Year1+1
      tmp<-read.fit(filename)
      yq<-lapply(tmp$dates,function(x){x[,1]+(x[,2]%/%3+1)/4-0.125})
#cat("L50\n");browser()
      catchtotalByFsh<-1:Nfsh %>% sapply(function(i){
                        tmp<-sapply(yq[[i]],function(x){which(Rlz.t.fsh1[i,]==x)});if(length(tmp)>0){pcatch[i,tmp]}else{NULL}
                    }) %>% sapply(sum)
      # catchtotalByFsh<-sapply(1:Nfsh,function(i){pcatch[i,sapply(yq[[i]],function(x){which(Rlz.t.fsh1[i,]==x)})]})

    }
    dat <- readLines(filename)   # Read in the file as text - run time could be reduced by only reading in from '# fishery totals' down but no skiplines argument in readLines - will have a hunt

    dat <- dat[(grep("totals",dat)+4):length(dat)]   # Remove all unwanted data above the fishery totals

    dat.obs <- dat[LineKeep]   # This is the only observed data we want keep - pulls out vector for the fishery then skips down to the next fishery and grabs vector, etc. etc.
    dat.obs <- as.data.frame(t(read.table(text=dat.obs, nrows=length(LineKeep))))   # Get it in the right format and transpose
    names(dat.obs) <- VecFsh   # Match the fishery names to the columns

    keep.fsh = c(na.omit(ifelse(apply(dat.obs,2,sum) > 0, names(dat.obs), NA)), "sizebin", "set")   # Used to identify which fisheries have data - if all zeros then removed later on if remove.fsh == "TRUE"
#    cat("L49\n");browser()
    dat.obs$sizebin <- sizebins   # Add sizebins - becomes the x axis later on
    dat.obs$set <- "Observed"   # Neet to identify this data as observed


# Same process as above done for predicted sizes - could have done them simultaneously but harder to get them in the right format for plotting
    dat.pred <- dat[LineKeep+1]
    dat.pred <- as.data.frame(t(read.table(text=dat.pred, nrows=length(LineKeep))))
    names(dat.pred) <- VecFsh
    dat.pred$sizebin <- sizebins
    dat.pred$set <- "Predicted"
    dat.pred.org<-dat.pred
    if(nSp>1 & fit){
      for(i in 1:Nfsh){
        if(catchtotalByFsh[i]>0){
          dat.pred[,i]<-if(i<=Nfsh/2 ){
            dat.pred[,i]*catchtotalByFsh[i]/sum(catchtotalByFsh[c(i,i+Nfsh/2)])*2
          }else{
            dat.pred[,i]*catchtotalByFsh[i]/sum(catchtotalByFsh[c(i,i-Nfsh/2)])*2
          }
        }
      }
#      cat("L88\n");browser()
    }
# Combine observed and predicted datasets
    dat.full <- rbind(dat.obs, dat.pred)

    if(is.logical(remove.fsh) && remove.fsh || remove.fsh=="TRUE"){
        dat.full <- dat.full[,match(keep.fsh,names(dat.full))]
        # If true only fisheries with data will be plotted, if false then will be plotted as zeros
    }

    plot.dat <- melt(dat.full, id=c("set","sizebin"))

    names(plot.dat)[3:4] <- c("Fishery","freq")   # Format data into the shape required for ggplot
    if(nSp==1){
        plot.dat$Fishery <- factor(fleetlabs[as.numeric(paste(plot.dat$Fishery))], levels = fleetlabs)
    }else{
      plot.dat %>% mutate(Sp=fishSpPtr[as.numeric(as.character(!!sym("Fishery")))]) %>% 
        mutate(Gender=if_else(!!sym("Sp")==1,"Male","Female")) %>%
        mutate(Fishery.num=!!sym("as.numeric(as.character(Fishery))")) %>%
        mutate(tmp=!!sym("if_else(Fishery.num<=Nfsh/2,Fishery.num,Fishery.num-Nfsh/2)")) %>%
        mutate(Fishery.tmp=if(Nfsh/2<10){tmp}else{str_pad(tmp,width=2,pad="0")}) %>% select(-!!sym("tmp")) %>%
        mutate(Fishery2=fleetlabs[ifelse(!!sym("Gender")=="Male",!!sym("Fishery.num"),!!sym("Fishery.num-Nfsh/2"))]) %>%
        unite(col="Fishery3",!!!syms(c("Fishery.tmp","Fishery2")),remove=TRUE) %>% select(-!!sym("Fishery"))  %>%
        rename(Fishery=!!sym("Fishery3")) %>% select(-!!sym("Sp")) %>% select(-!!sym("Fishery.num"))->plot.dat
        plot.dat$freq<-plot.dat$freq*0.5
#        plot.dat %>% group_by(set,Fishery,sizebin,Gender)  %>% summarise_each(funs(mean)) %>%   # debug_pipe() %>%
#          filter(set == "Observed") %>% select(-Gender)  %>%summarise_each(funs(sum)) ->plot.dat.obs  ## Need to check if sum is OK

    ## Need to make conditioning  if there is Gender specific observed size composition
    }
#   if(verbose)cat("L103\n");browser()
# Produce and print plot

    if(nSp==1){
      p <- ggplot(plot.dat[plot.dat$set == "Observed",], aes_string(x="sizebin", y="freq")) +
                geom_bar(stat="identity", colour=fillcol, fill=fillcol) +
                facet_wrap(~ Fishery, ncol=Ncols, scales="free_y",dir=dir) +
                xlab(xlabel) + ylab("Samples")
      if(fit){p<-p+geom_line(data=plot.dat[plot.dat$set == "Predicted",], aes_string(x="sizebin", y="freq"), colour=lincol, size=line.wdth)}
    }else{
      p<-ggplot(plot.dat[plot.dat$set == "Observed",], aes_string(x="sizebin", y="freq")) +
                geom_bar(stat="identity", colour=fillcol, fill=fillcol) +
                facet_wrap(~ Fishery, ncol=Ncols, scales="free_y",dir=dir) +
                xlab(xlabel) + ylab("Samples")
      if(fit){p<-p+geom_line(data=plot.dat[plot.dat$set == "Predicted",],
                aes_string(x="sizebin", y="freq",group="Gender", colour="Gender"), size=line.wdth,position="Stack")}
    }
    p<-p+scale_y_continuous(breaks=pretty_breaks(n=nbrks)) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    if(verbose)cat("L125\n")
    if(plot)print(p)
    return(invisible(p))
}

















