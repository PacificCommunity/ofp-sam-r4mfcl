#' Make diagnostic plot given a directory and other info
#'
#' @param rundir directory that contains .ini .frq .par plot-.rep and where a Figures directory will be made to save all figures
#' @param spp species that is before the files listed above
#' @param stndfish vector of numbers for fisheries with standardized CPUE
#' @param fdescloc optional location of fdesc.txt file that contains information for labels of figures
#' @param par option number of par you want ploted otherwise finds and uses largest number value
#' @param fishcols optional vector of colors for each fishery type
#' @param regcols optional vector of colors for each region
#' @param tagrepfile optional name of tag reporting rate file produced by MFCL not necessary if .tag file does not exist
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 alpha
#'
#' @export

## Function to make figures from a model directory, species, fleetlabels, and possibly the par name and saves them in a Plots directory that is created within the original folder
## If par name is not supplied then it defaults to taking out.par and if that does not exist then it takes the largest par value
DiagnosticPlots <-  function(rundir,spp='skj',stndfish,years=c(1972,2018),fdescloc=NULL,par=NULL,fishcols=NULL,regcols=NULL,tagrepfile=NULL){
    ## rundir is directory that contains the .frq, .par, plot-.rep, .tag, etc. and then will create a Figures directory within it to save the plots in
    ## spp is the species name that the mfcl model is for
    ## Stndfish is a vector of number for the fisheries with the standardized CPUE
    ## check if last value in string is / if not add it in
    if (substr(rundir,nchar(rundir),nchar(rundir))!='/') rundir=paste0(rundir,'/')
    ## Set working directory and load some files
    setwd(rundir)
    pltdir=paste(rundir,"Figures",sep='/')
    ## Keep out.par or find the largest par file and keep that
    if (is.null(par)){
        if('out.par' %in% list.files(rundir)) {
            par='out'
        }else{
            numparf=list.files(path=rundir,patt='[0-9]{1,2}\\.par$')
            par= numparf %>% gsub('.par','', .) %>% as.numeric %>% sort(decreasing=TRUE) %>% '['(1)
        }
    }
    ## Read in par and rep files
    readpar=read.par(paste0(par,'.par'),verbose=FALSE)
    readrep=read.rep(paste0('plot-',par,'.par.rep'))

    ## Check if no input and give default if not present
    if (is.null(tagrepfile)) tagrepfile="temporary_tag_report"
    if (is.null(regcols)) regcols=alpha(c("dodgerblue4","aquamarine3","steelblue2","slategray1","darkseagreen3","red","purple","yellow","orange"), 0.7)
    if (is.null(fishcols)) fishcols=c("forestgreen","firebrick3","dodgerblue2","yellow2","blue","lightblue")
    ## make directory for plots if it does not exists
    if (!dir.exists(pltdir)) {dir.create(pltdir)}
    ## check if files exist and if they do then read them in
    if (file.exists(paste0(spp,'.ini'))){ #.ini
        readini=read.ini(paste0(spp,'.ini'))
    } else {stop(paste0("Error: The rundir supplied does not contain a ",spp,".ini file. The rundir supplied was:\n",rundir,"\n"))}
    if (file.exists(paste0(spp,'.frq'))){ #.frq
        readfrq=read.frq(paste0(spp,'.frq'))
    } else {stop(paste0("Error: The rundir supplied does not contain a ",spp,".frq file. The rundir supplied was:\n",rundir,"\n"))}
    ## The number of fisheries from the frq file
    Nfish=readfrq$struct$nf
    if (!is.null(fdescloc)){            #fdesc
        fdesc <- read.table(fdescloc, header=TRUE)
    } else if (!file.exists("fdesc.txt")){ stop("Error: You did not supply a location for the fdesc.txt and the rundir supplied does not contain a fdesc.txt file. You need to create a file with 7 columns and an entry for each fishery in the model. The columns should be named 1. num 2. gear_long  3. method  4. code  5. gear 6. flag 7. region. This file creates all of the labels for the fisheries.  The rundir supplied was:\n",rundir,"\n")
    } else {
        fdesc <- read.table( paste0(rundir,"fdesc.txt"), header=TRUE)
    }
    fltlabs <- paste(fdesc$method, fdesc$flag, fdesc$region, sep="-") # These are the labels that are used in many of the plots
    ## Define characteristics of fishing gear
    gearspecs <- list(code=c("L","P","S","Z","SA","SU"), code2=c("ll","pl","ps","ot","sa","su"),
                      gearEN=c("Longline","Pole-and-line","Purse seine","Other","PS-associated","PS-free school"),
                      gearFR=c("Palangre","Canne","Senne","Autres","Senne (bancs associés)","Senne (bancs libres)"),
                      cls=fishcols)
    fdesc$grcol <- gearspecs$cls[match(fdesc$method, gearspecs$code)]



    ##____________________________________________________________________________________________________________
    ##plot.catch.fit
    windows(6000, 4000)
    plot_catch.fit.gg(plotrepfile = readrep, fleetlabs = factor(fltlabs, levels=fltlabs), yrlims = years, line.col = alpha("blue", 0.6),
                   pnt.col = alpha("black", 0.6), n.col = 4, plot.log = FALSE, lnsize = 0.6, nbrks = c(3,3), yaxe = "Annual catch (1,000's mt or No. fish)")
    savePlot(file = paste0(pltdir, "/catch_fit.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Stacked biomass
    windows(2000, 1000)
    plot_biomass.stacked.gg(plotrep=readrep, pmain="Regional spawning potential", type="SSB", lgposi="topright", reg.cols=regcols, tit.colour=alpha("transparent", 0.5))
    savePlot(file=paste0(pltdir, "/biomass_stacked_SSB.png"), type="png")
    plot_biomass.stacked.gg(plotrep=readrep, pmain="Regional total biomass", type="TOT", lgposi="topright", reg.cols=regcols, tit.colour=alpha("transparent", 0.5))
    savePlot(file=paste0(pltdir, "/biomass_stacked_TOT.png"), type="png")
    plot_biomass.stacked.gg(plotrep=readrep, pmain = "Regional recruitment", type ="REC", lgposi="topright", reg.cols=regcols, tit.colour=alpha("transparent", 0.5))
    savePlot(file=paste0(pltdir, "/biomass_stacked_REC.png"), type="png")
    dev.off()

    if (file.exists(paste0(spp,'.var'))){
         windows(2000, 1000)
         plot_biomass.wCI(paste0(spp,'.var'),years=seq(years[1],years[2],1),btype="SSB",divide=1000000,ylab="Spawning biomass(1000 t)",col=1:2,alpha=0.2)
    }


    ##____________________________________________________________________________________________________________
    ## Plot of selectivities
    windows(3200, 2800)
    plot_selectivity(rep = readrep,verbose=FALSE,Gender=FALSE)
    savePlot(file = paste0(pltdir, "/selectivity.png"), type = "png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of catchability, q
    windows(3000, 2000)
    plot_q(parfile = readpar, plotrepfile = readrep, fleetlabs = fltlabs, line.col = alpha("blue", 0.6), n.col = Nfish, lnsize = 0.5, nbrks = 4, annual = TRUE)
    savePlot(file = paste0(pltdir, "/q.png"), type = "png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Test overall composition fit - i.e. aggregated over time
    if (file.exists("length.fit")){
        windows(2500, 2100)
        plot_overall.composition.fit(filename = "length.fit", xlabel = "Length (cm)", remove.fsh = TRUE,  Ncols = 3, line.wdth = 0.8,lincol = "#FF3333", fleetlabs = fltlabs, fillcol = "#6699CC", nbrks = 3, nSp=readpar$nSp,verbose=FALSE,rep=readrep)
        savePlot(file = paste0(pltdir, "/overall_composition_fit_len.png"), type = "png")
        ## ## size summaries
        ## plot_fsh.size.summaries("length.fit",xlabel="Length (cm)", remove.fsh=TRUE,Ncols=4, plotname=paste0(pltdir, "/length_size_summaries.png"),plot.wdth=2500,plot.hgt=2100,line.wdth=1)
        dev.off()

        ##____________________________________________________________________________________________________________
        ## Plot of residual patterns in composition data
        windows(3200, 2200)
        plot_size.residuals(read.fit("length.fit"),readfrq,fltl=fltlabs,n.col=5,Fish.keep=1:Nfish,verbose=FALSE,rep=readrep,Year1=years[1])
        savePlot(file = paste0(pltdir, "/length_residuals.png"), type = "png")
        dev.off()
    } else {warning("length.fit does not exist")}
    if (file.exists("weight.fit")){
        windows(2500, 2100)
        plot_overall.composition.fit(filename = "weight.fit", xlabel = "Weight (kg)", remove.fsh = TRUE,  Ncols = 3, line.wdth = 0.8,lincol = "#FF3333", fleetlabs = fltlabs, fillcol = "#6699CC", nbrks = 3, nSp=readpar$nSp,verbose=FALSE,rep=readrep)
        savePlot(file = paste0(pltdir, "/overall_composition_fit_wgt.png"), type = "png")
        dev.off()
        plot_size.residuals(read.fit("weight.fit"),readfrq,fltl=fltlabs,n.col=5,Fish.keep=1:Nfish,LenFit=FALSE,label="Weight (kg)",verbose=FALSE,rep=readrep,Year1=years[1])
        savePlot(file = paste0(pltdir, "/length_residuals.png"), type = "png")
    } else {warning("weight.fit does not exist")}

    ##____________________________________________________________________________________________________________
    ## Plot of effort deviations
    windows(1900, 1200)
    ## Standardised CPUE fisheries
    plot_effort.deviations(plotrepfile=readrep, frqfile=readfrq, fleetlabs=fltlabs, ylimit=c(-2,2),
                           fishplot=stndfish, nclm=3, spandef = 2)
    savePlot(file=paste0(pltdir, "/effort_deviations_standardized.png"), type="png")

    ## Unstandardised CPUE fisheries
    plot_effort.deviations(plotrepfile=readrep, frqfile=readfrq, fleetlabs=fltlabs, ylimit=c(-2,2), spandef = 2,
                           fishplot=c(1:Nfish)[(-1*stndfish)], nclm=4) # Get rid of fisheries that are given missing effort until the final year
    savePlot(file=paste0(pltdir, "/effort_deviations_unstandardized.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of data availability
    windows(2000, 1800) # Note that most of the colours below (apart from for BET) are just space fillers to check if the code works
    plot_data.availability(frqfile=readfrq, timespan=years, plotcols=fdesc$grcol, fleetlabs=fltlabs, Ylab.space=6, stand.fsh=stndfish)
    savePlot(file=paste0(pltdir, "/data_availability.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of standardised CPUE indices with the cvs used in the assessment
    windows(3200, 2000)
    plot_cpue.with.cvs(repfile=readrep, frqfiles=readfrq, fleetlabs=fltlabs, nfish=stndfish, n.cols=3, plot.annual=TRUE, fac.levels=fltlabs[stndfish])
    savePlot(file=paste0(pltdir, "/cpue_with_cvs.png"), type="png")
    dev.off()


    ## plot_depletion.regional(readrep, type="SSB",plot.layout=c(3,2),ylab="Depletion(Adult biomass)")
    if (file.exists("catch.rep")){
        ##____________________________________________________________________________________________________________
        ## Plot of fishery depletion - typically the refpoint argument will come from the results of the get.outcomes function
        ## Produces it's own windows devices and saves as two plots produced (only one plot when there is only one region, e.g. MLS)
        windows(2000, 1200)
        tmprefpt <- get.outcomes(readrep, readpar, "catch.rep", nofish = TRUE, nofishp = c(44,4), lateyr = years[2])
        plot_depletion.gg(readrep,refpoint=tmprefpt$SBlatest.SBF0,refyear=max(readrep$alltimes), type="SSB",by.region=FALSE,femaleOnly=TRUE,verbose=FALSE)
        savePlot(file=paste0(pltdir, "/depletion_overall.png"),'png')
        plot_depletion.gg(readrep,refpoint=tmprefpt$SBlatest.SBF0,refyear=max(readrep$alltimes), type="SSB",by.region=TRUE,femaleOnly=TRUE,verbose=FALSE,overlay=TRUE)
        savePlot(file=paste0(pltdir, "/depletion_regional_overlayed.png"),'png')
        dev.off()
        windows(3000, 1600)
        plot_depletion.gg(readrep,refpoint=tmprefpt$SBlatest.SBF0,refyear=max(readrep$alltimes), type="SSB",by.region=TRUE,femaleOnly=TRUE,verbose=FALSE,overlay=FALSE)
        savePlot(file=paste0(pltdir, "/depletion_regional_grid.png"),'png')
        dev.off()
        ## plot.depletion(figdir=pltdir, plotrep=readrep, refpoint=tmprefpt$SBlatest.SBF0, refyear= years[2], type="SSB", plotdim=c(4,3),
        ##                pnames=c("/depletion_reg.png","/depletion_overall.png"), ptype="png", lim.refpoint=0.2,
        ##                sblab = "biomass", tag.refpoint=0.5)



        ##____________________________________________________________________________________________________________
        ## Plot of catch by gear at the region and full area levels
        windows(2000, 1200)
        ## Full area
        plot_timeseries.catch(catdat="catch.rep", repfile=readrep, nocls=NULL, gear=fdesc$method, region=fdesc$region, all.regions=TRUE,brwidth=1, collist=setNames(gearspecs$cls, gearspecs$code))
        savePlot(file=paste0(pltdir, "/timeseries_catch_full.png"), type="png")
        dev.off()

        windows(3100, 2500)
        ## By region
        plot_timeseries.catch(catdat="catch.rep", repfile=readrep, nocls=NULL, gear=fdesc$method, region=fdesc$region, all.regions=FALSE,brwidth=1, collist=setNames(gearspecs$cls, gearspecs$code))
        savePlot(file=paste0(pltdir, "/timeseries_catch_regional.png"), type="png")
        dev.off()
    } else {warning(paste0("Cannot create depletion plot because you do not have the catch.rep in the supplied rundir. The rundir supplied was:\n",rundir,"\n"))}

    ##____________________________________________________________________________________________________________
    ## Plot of overall temporal change in adult and juvenile F
    windows(2300, 1600)
    plot.temporal.F(plotrep=readrep, inifile=readini, dome=TRUE, ymax=NULL, French=FALSE)
    savePlot(file=paste0(pltdir, "/temporal_F.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of regional temporal change in F-at-age
    windows(2200, 1800)
    plot.regional.F.at.age(plotrep = readrep, nbrks = 3, ncls =2, lnsize = 0.5, alph = 0.7)
    savePlot(file=paste0(pltdir, "/regional_F_at_age.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Test the plot of age-specific fishing mortality and population age distribution
    windows(2000, 1800)
    plot.age.specific.F(plotrep=readrep, years.keep=seq(years[1], years[2], by=10), plotcol="#6699CC", nbrks=c(3,4))
    savePlot(file=paste0(pltdir, "/age_specific_F.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of natural mortality function
    windows(2000, 1600)
    plot_natural.mortality.function.gg(repfiles=list(readrep), modlab = c(""),verbose=FALSE)
    savePlot(file=paste0(pltdir, "/natural_mortality_function.png"), type="png")
    dev.off()

        ##____________________________________________________________________________________________________________
    ## Plot of growth function
    windows(2000, 1600)
    plot_growth.function.gg2(rep=readrep,YLIM=c(readfrq$dl$lffirst,readfrq$dl$lfint*readfrq$dl$lfwidth))
    savePlot(file=paste0(pltdir, "/growth_function.png"), type="png")
    dev.off()


    ##____________________________________________________________________________________________________________
    ## Plot of fit to the conditional age-at-length data
    if (file.exists(paste0(spp,".age_length"))){
        windows(2000, 1600)
        plot.age.length.fit(alfile=paste0(spp,".age_length"), frqfile=readfrq, inifile=readini, plotfile=readrep,
                            parfile=readpar, fixlog=FALSE, fix_pars=NA, sdmult=2, ylims=c(0,200), xlbl="Age (quarters)")
        savePlot(file=paste0(pltdir, "/age_length_fit.png"), type="png")
        dev.off()
    }

    ##____________________________________________________________________________________________________________
    ## Spawner recruitment relationship plot
    windows(2000, 1600)
    plot_SRR.gg(repfile=readrep, annual=TRUE)
    savePlot(file=paste0(pltdir, "/SRR_Ann.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of yield curve
    windows(2000, 1600)
    plot_yield.gg(repfile = readrep, xlimits = c(0, 3))
    savePlot(file = paste0(pltdir, "/yield.png"), type = "png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of SSB vs SSB in the absence of fishing with a plot for each region
    if (readfrq$struct$nreg > 1) {
        windows(2600, 2000) # Note that this plot is not appropriate for single region models like MLS - they can use the plot.depletion plot above instead
        plot_nofishing.regional.gg(plotrep = readrep, type = "SSB", plot.layout = c(5,2), legpos = "bottomleft", mainleg="bottom")
        savePlot(file = paste0(pltdir, "/nofishing_regional.png"), type = "png")
        dev.off()
    }

    ##____________________________________________________________________________________________________________
    ## Plot of recruitment with a plot for each region
    ## Need var file...
    windows(2200, 1800)
    plot.regional.recruitment(plotrep = readrep, varfile = NULL, plot.layout = c(5,2), legpos = "topleft")
    savePlot(file = paste0(pltdir, "/regional_recruitment.png"), type = "png")
    dev.off()
    windows(5000, 2800)
    plot.regional.recruitment.gg(plotrep = readrep, annual = FALSE, divisor = 1000000, fillcol = alpha("black",0.6), varfile = NULL,
                                 Ncols = 2, nbrks = 3, brwidth = 1, p.type = "line", lnsize = 1, y.lab = "Recruitment (millions)")
    savePlot(file = paste0(pltdir, "/regional_recruitment_gg.png"), type = "png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of regional total and spawning biomass
    windows(5000, 2800)
    plot.regional.biomass.gg(plotrep = readrep, annual = FALSE, divisor = 1000, fillcol = alpha("black",0.6), varfile = NULL,
                             Ncols = 2, nbrks = 3, brwidth = 1, p.type = "line", lnsize = 1, biomass = "AdultBiomass",
                             y.lab = "Biomass (1,000's of mt)")
    savePlot(file = paste0(pltdir, "/regional_biomass_gg.png"), type = "png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of the availability of composition data
    windows(3000,2400)
    plot.length.comp.availability(frq = readfrq, bordercol="#6699CC", fillcol="#6699CC", Ncols = 3, nbrks = 3,
                                  annual = TRUE, fleetlabs = fltlabs, divisor = 1,Max1000=TRUE)
    savePlot(file=paste0(pltdir, "/length_comp_available.png"), type="png")
    dev.off()
    ## windows(3000,2400)
    ## plot.weight.comp.availability(frq = readfrq, bordercol="#6699CC", fillcol="#6699CC", Ncols = 3, nbrks = 3,
    ##                               annual = TRUE, fleetlabs = fltlabs, divisor = 1,Max1000=TRUE)
    ## savePlot(file=paste0(pltdir, "/weight_comp_available.png"), type="png")
    ## dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of fit to the standardised CPUE data
    windows(2000, 1500)
    plot_cpue.fit.gg(readrep,readfrq,readpar,fisheries=stndfish,fleetlabs=fltlabs,XLIM=years,fac.levels=fltlabs[stndfish])
    savePlot(file=paste0(pltdir, "/cpue_fit.png"), type="png")
    dev.off()

    ##____________________________________________________________________________________________________________
    ## Plot of maturity schedule
    windows(2000, 1600)
    plot_maturity_age_par(list(readpar),ylab="Proportion Mature",modlab=c(""))
    savePlot(file=paste0(pltdir, "/maturity_at_age.png"), type="png")
    dev.off()



    ## plot_quarterly.movement.map
    ##
    ## plot_tag_diags
    ## plot_Kobe



    ## Only do tag plots if the .tag file exits but if it doesn't just give a warning
    if (file.exists(paste0(spp,'.tag'))){
        readtag=read.tag(paste0(spp,'.tag'))
        readtagrep=read_tag_rep(paste0(spp,'.frq'),tagrepfile,paste0(spp,".tag"),year1=years[1], rm.prelims=TRUE)

        ##____________________________________________________________________________________________________________
        ## Plot of tagging time at liberty
        windows(2000, 1600)
        plot.T.at.liberty(tagfl=readtagrep, logscl=FALSE, by.program=FALSE, fac.plot=TRUE, xaxe="Periods at liberty (quarters)",
                          yaxe="Number of tag returns", ncols=2)
        savePlot(file=paste0(pltdir, "/T_at_liberty.png"), type="png")

        ## On log scale
        plot.T.at.liberty(tagfl=readtagrep, logscl=TRUE, by.program=FALSE, fac.plot=TRUE, xaxe="Periods at liberty (quarters)",
                          yaxe="log(number of tag returns)", ncols=2)
        savePlot(file=paste0(pltdir, "/T_at_liberty_log-scale.png"), type="png")

        ## By tagging programme - one panel each
        plot.T.at.liberty(tagfl=readtagrep, logscl=FALSE, by.program=TRUE, fac.plot=TRUE, xaxe="Periods at liberty (quarters)",
                          yaxe="Number of tag returns", ncols=2)
        savePlot(file=paste0(pltdir, "/T_at_liberty_byProgram.png"), type="png")
        ## ## By tagging programme - one panel with different colour lines for the programmes
        ##      plot.T.at.liberty(tagfl=readtagrep, logscl=FALSE, by.program=TRUE, fac.plot=FALSE, xaxe="Periods at liberty (quarters)",
        ##                        yaxe="Number of tag returns", ncols=2)

        dev.off()

        ##____________________________________________________________________________________________________________
        ## Plots of tag reporting rates
        windows(2200, 1600)
        plot.tag.reporting.rates(parf=readpar, grp.names = rr.labs, new.style=TRUE, xmargin=NA, nls=6)
        savePlot(file=paste0(pltdir, "/tag_reporting_rates_new.png"), type="png")

        plot.tag.reporting.rates(parf=readpar, grp.names = rr.labs, new.style=FALSE, xmargin=5, nls=6)
        savePlot(file=paste0(pltdir, "/tag_reporting_rates_old.png"), type="png")
        dev.off()

        ## Over all tag groups
        windows(2000,1200)
        ## Including mixing period tags
        plot.grouped.tags(tagfl = readtagrep, mix.time = NULL, remove.mix = FALSE, xaxe = "Year", yaxe = "Number of tag returns",
                          ln.sz = 0.7, ln.col = alpha("black", 0.7), pt.sz = 2, pt.col = alpha("red", 0.7), all.fishies = TRUE,
                          Ncols = 2, grpnms = fltlabs, keepGrps = c(1,2,4,5))
        savePlot(file=paste0(pltdir, "/grouped_tags_overall.png"), type="png")
        ## Excluding mixing period tags
        ##    plot.grouped.tags(tagfl = readtagrep, mix.time = 1, remove.mix = TRUE, xaxe = "Year", yaxe = "Number of tag returns",
        ##                      ln.sz = 0.7, ln.col = alpha("black", 0.7), pt.sz = 2, pt.col = alpha("red", 0.7), all.fishies = TRUE,
        ##                      Ncols = 2, grpnms = fltlabs, keepGrps = c(1,2,4,5))
        dev.off()

        ## By tag group
        windows(2300,2400)
        ## Including mixing period tags
        plot.grouped.tags(tagfl=readtagrep, mix.time=NULL, remove.mix=FALSE, xaxe="Year", yaxe="Number of tag returns",
                          ln.sz=0.5, ln.col=alpha("black", 0.6), pt.sz=1, pt.col=alpha("red", 0.6), all.fishies=FALSE,
                          Ncols=2, grpnms=tagrecgrps, keepGrps=c(1,2,4,5,7,8), fsh.grps=c(1,2,3,4,5,5,6,7,8,8,9,10,11,11,12,13,14,15,16,17,17,18,19),
                          fac.levels=c("P-JPN-1","S-ALL-1","L-ALL-1","P-ALL-2","S-ALL-2","L-ALL-2","P-ALL-5","S-ALL-5"))
        savePlot(file=paste0(pltdir, "/grouped_tags_bygroup1.png"), type="png")
        dev.off()

        windows(2300,2600)
        plot.grouped.tags(tagfl=readtagrep, mix.time=NULL, remove.mix=FALSE, xaxe="Year", yaxe="Number of tag returns",
                          ln.sz=0.5, ln.col=alpha("black", 0.6), pt.sz=1, pt.col=alpha("red", 0.6), all.fishies=FALSE,
                          Ncols=2, grpnms=tagrecgrps, keepGrps=c(10,11,13:17), fsh.grps=c(1,2,3,4,5,5,6,7,8,8,9,10,11,11,12,13,14,15,16,17,17,18,19),
                          fac.levels=c("P-ALL-3","S-ALL-3","Z-PH-4","Z-ID-4","S-ID.PH-4","P-ALL-4","S-ALL-4"))
        savePlot(file=paste0(pltdir, "/grouped_tags_bygroup2.png"), type="png")
        dev.off()

        plot_tag_diags(readtagrep,readpar,'logscale')

        plot_tag_diags(readtagrep,readpar,'residuals')

    } else {warning("The rundir supplied does not contain a ",spp,".tag file. Only a problem for BET, YFT, SKJ")}


    ## Do these last because they take forever to do!
    ## need to reprogram this to be more efficient
        ##____________________________________________________________________________________________________________
    ## Plot of median length over time
    windows(2200, 1800)
    plot_length.temporal(frq = paste0(spp,'.frq'), tmp.rep = readrep, fleetlabs = fltlabs, YLIM = c(readfrq$dl$lffirst,readfrq$dl$lfint*readfrq$dl$lfwidth), Nrows = 5, Ncols = 4, annual = FALSE,verbose=FALSE)
    savePlot(file = paste0(pltdir, "/plot_length_temporal.png"), type = "png")
    dev.off()
    ## Median weight over time
    windows(2200, 1800)
    plot.weight.temporal(frq = paste0(spp,".frq"), tmp.rep = readrep, ini = readini, fleetlabs = fltlabs, YLIM = c(0,100), Nrows = 4, Ncols = 4, annual = FALSE)
    savePlot(file = paste0(pltdir, "/plot_weight_temporal.png"), type = "png")
    dev.off()


}



