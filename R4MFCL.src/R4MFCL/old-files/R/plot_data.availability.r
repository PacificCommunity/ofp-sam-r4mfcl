#' @export
plot_data.availability = function(frqfile=read.frq(basefrq), spp='skj', timespan=c(1950,2012), gear=gearspecs, fleets=spp_fleets)
{
    Nfshry = frqfile$struct$nf
      plotcols = gear$cls[match(fleets$gear,gear$code)]

    datmat = as.data.frame(frqfile$mat)
      datmat$poscatch = ifelse(datmat$catch > 0, 1,0)
        datmat$yrqtr = datmat$year + (datmat$qtr-0.5)/12
          datmat$posCPUE = ifelse(datmat$se %in% c(0.05,-1), 0, 1)
            datmat$poslength = ifelse(datmat[,8] == -1, 0, 1)

    if(spp != 'skj') datmat$posweight = ifelse(apply(datmat[,(8+frqfile$dl$lfint-1):(8+frqfile$dl$wfint-1)],1,sum) > 0, 1, 0) # No weights for skj so ignore for them

# Is there catch data
    catch.oui = aggregate(datmat$poscatch, list(yrqtr=datmat$yrqtr,fishery=datmat$fishery), sum)
      catch.oui = catch.oui[catch.oui$x > 0,]
        catch.oui$plotcol = plotcols[catch.oui$fishery]

# Is there standardised cpue data
    cpue.oui = aggregate(datmat$posCPUE, list(yrqtr=datmat$yrqtr,fishery=datmat$fishery), sum)
      cpue.oui = cpue.oui[cpue.oui$x > 0,]
        cpue.oui$plotcol = plotcols[cpue.oui$fishery]
    
# Is there length data
    length.oui = aggregate(datmat$poslength, list(yrqtr=datmat$yrqtr,fishery=datmat$fishery), sum)
      length.oui = length.oui[length.oui$x > 0,]
        length.oui$plotcol = plotcols[length.oui$fishery]

# Is there weight data
    if(spp != 'skj')
    {
        weight.oui = aggregate(datmat$posweight, list(yrqtr=datmat$yrqtr,fishery=datmat$fishery), sum)
          weight.oui = weight.oui[weight.oui$x > 0,]
            weight.oui$plotcol = plotcols[weight.oui$fishery]
    }

    num.columns = ifelse(spp == 'skj', 3, 4)

    par(mfcol = c(1, num.columns),mar=c(2,2,1.5,1), oma=c(0,10,0,0))

    plot(catch.oui$yrqtr, catch.oui$fishery, ylim=c(0,Nfshry), xlim=timespan, xlab='', ylab='', col=catch.oui$plotcol, pch=19, las=1, yaxt='n', main='Catch')
      axis(side=2, seq(1,Nfshry,1), labels=fleetlabs, las=1)

    plot(cpue.oui$yrqtr, cpue.oui$fishery, ylim=c(0,Nfshry), xlim=timespan, xlab='', ylab='', col=cpue.oui$plotcol, pch=19, las=1, yaxt='n', main='CPUE')
      axis(side=2, seq(1,Nfshry,1), labels=FALSE)

    plot(length.oui$yrqtr, length.oui$fishery, ylim=c(0,Nfshry), xlim=timespan, xlab='', ylab='', col=length.oui$plotcol, pch=19, las=1, yaxt='n', main='Length')
      axis(side=2, seq(1,Nfshry,1), labels=FALSE)

    if(spp != 'skj')   plot(weight.oui$yrqtr, weight.oui$fishery, ylim=c(0,Nfshry), xlim=timespan, xlab='', ylab='', col=weight.oui$plotcol, pch=19, las=1, yaxt='n', main='Weight')
      axis(side=2, seq(1,Nfshry,1), labels=FALSE)
}









